# Auto Arima --------------------------------------------------------------
# Arima Cluster 1: -----

m_arima_d_c1 <- auto.arima(ts_d_c1_treino, seasonal = TRUE, 
                           stepwise = FALSE, approximation = FALSE,
                           lambda = "auto")

summary(m_arima_d_c1)
checkresiduals(m_arima_d_c1)

forecast_arima_c1 <- forecast(m_arima_d_c1, h = 12)

autoplot(ts_d_c1_treino) +
  autolayer(m_arima_d_c1$fitted) +
  autolayer(forecast_arima_c1) +
  autolayer(ts_d_c1_valida)

accuracy(forecast_arima_c1, ts_d_c1_valida)


# Arima Cluster 2: -----

m_arima_d_c2 <- auto.arima(ts_d_c2_treino, seasonal = TRUE, 
                           stepwise = FALSE, approximation = FALSE,
                           lambda = "auto")

summary(m_arima_d_c2)
checkresiduals(m_arima_d_c2)

forecast_arima_c2 <- forecast(m_arima_d_c2, h = 12)

autoplot(ts_d_c2_treino) +
  autolayer(m_arima_d_c2$fitted) +
  autolayer(forecast_arima_c2) +
  autolayer(ts_d_c2_valida)

accuracy(forecast_arima_c2, ts_d_c2_valida)

# Arima Cluster 3: -----

m_arima_d_c3 <- auto.arima(ts_d_c3_treino, seasonal = FALSE, 
                           stepwise = FALSE, approximation = FALSE,
                           lambda = "auto")

summary(m_arima_d_c3)
checkresiduals(m_arima_d_c3)

forecast_arima_c3 <- forecast(m_arima_d_c3, h = 12)

autoplot(ts_d_c3_treino) +
  autolayer(m_arima_d_c3$fitted) +
  autolayer(forecast_arima_c3) +
  autolayer(ts_d_c3_valida)

accuracy(forecast_arima_c3, ts_d_c3_valida)






