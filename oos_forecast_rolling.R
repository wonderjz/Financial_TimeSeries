library(fpp3)
library(readr)
combined_data<- read_csv("fredmd_EF3451.csv")

data_withlag <- combined_data |>
  mutate(
    ur = unrate,
    ln_ip = log(industrial_production),
    ln_cpi = log(cpi),
    r = interest_rate,
    ur_1 = lag(ur, order_by = date),
    ln_ip_1 = lag(ln_ip, order_by = date),
    ln_cpi_1 = lag(ln_cpi, order_by = date),
    r_1 = lag(r, order_by = date),
    # transform daily into monthly format
    date = yearmonth(date)                    
  )

new_tsibble <- data_withlag |>
  filter(!is.na(ur_1)) |>  # Remove rows with NA values due to lagging
  as_tsibble(index = date)  # Ensure it remains a tsibble

tslm_model <-new_tsibble |> slice(1:120) |> model(TSLM(ur ~ ur_1 + r_1 + ln_cpi_1))
report(tslm_model)

future_data = new_data(slice(new_tsibble,1:120), 1) |>
  mutate(
    ur_1 = new_tsibble$ur_1[121],
    ln_cpi_1 = new_tsibble$ln_cpi_1[121],
    r_1 = new_tsibble$r_1[121]
  )

forecasts <- forecast(tslm_model, new_data = future_data)
print(forecasts)
print(forecasts$.mean)

############# rolling window forecast
# Parameters
window_size <- 120         # Example window size
forecast_horizon <- 1      # Forecast horizon: for the next month
forecasts_list1 <- list()  # for saving the results in each loop
forecasts_list2 <- list()  # for saving the results in each loop

# Perform rolling window forecasts
# the last regression in the loop uses data up to T - h
for (i in 1:(nrow(new_tsibble) - forecast_horizon -  (window_size-1))) {
  start_index <- i
  end_index <- start_index + window_size - 1
  data_win <- new_tsibble |> slice(start_index:end_index)  
  
  # Fit the linear regression models
  # model1 
  tslm_model1 <- data_win |> model(TSLM(ur ~ ur_1 + ln_cpi_1 + r_1 ))
  #report(tslm_model1)
  # model2 
  tslm_model2 <- data_win |> model(TSLM(ur ~ ur_1 + ln_cpi_1 + ln_ip_1 ))
  #report(tslm_model2)
  
  # Prepare newdata for predictions
  future_data1 = new_data(data_win, 1) |>
    mutate(
      ur_1 = data_win$ur[window_size],
      ln_cpi_1 = data_win$ln_cpi[window_size],
      r_1 = data_win$r[window_size]
    )
  future_data2 = new_data(data_win, 1) |>
    mutate(
      ur_1 = data_win$ur[window_size],
      ln_cpi_1 = data_win$ln_cpi[window_size],
      ln_ip_1 = data_win$ln_ip[window_size]
    )

  # Generate forecasts
  forecasts1 <- forecast(tslm_model1, new_data = future_data1)
  forecasts2 <- forecast(tslm_model2, new_data = future_data2)
  
  forecasts_list1[i] <-forecasts1$.mean
  forecasts_list2[i] <-forecasts2$.mean
  # print(slice(data_win,120:120))  # print the last row of the data_win to verify the window
}

# Comparing forecasts
rolling_forecasts_a <- do.call(rbind, forecasts_list1)
rolling_forecasts_b <- do.call(rbind, forecasts_list2)

eval_subsample <- slice(new_tsibble, window_size+1:nrow(new_tsibble) ) |>
  mutate(
    fc1 = rolling_forecasts_a,
    fc2 = rolling_forecasts_b,
    err1 = ur - fc1,
    err2 = ur - fc2
  )
mse1 <- mean(eval_subsample$err1^2)
mse2 <- mean(eval_subsample$err2^2)
mse2/mse1

# plot forecasts and forecast errors
eval_subsample|>
  ggplot(aes(x = date)) +
  geom_line(aes(y = ur), colour = "black") +
  geom_line(aes(y = fc1), colour = "blue") +
  geom_line(aes(y = fc2), colour = "#D55E00")

eval_subsample|>
  ggplot(aes(x = date)) +
  geom_line(aes(y = err1), colour = "blue") +
  geom_line(aes(y = err2), colour = "#D55E00")