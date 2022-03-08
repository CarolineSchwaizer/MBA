setClass(Class="TsList",
         representation(
           orders="ts",
           avg_price="ts",
           revenue="ts")
         )


f_test <- function(df){
  # Nomes das variáveis
  vars <- colnames(df)
  # Instancia lista de datas para TS
  inds <-  seq(as.Date(min(dates$dates)), 
               as.Date(max(dates$dates)), by = "day")
  
  for (i in vars){
    name <- paste("ts_", as.character(i), sep = '')
    ts_tmp <- ts(df[,i], 
                 start = c(2017,
                           as.numeric(format(inds[1], '%j'))),
                 frequency = 365, class = "ts")
    assign(as.character(name),
           ts_tmp)
    }
  
  return(new("TsList",
             orders=ts_orders,
             avg_price=ts_avg_price,
             revenue=ts_revenue))
}


# Instancia slot da Classe TsList como 'ts'
tmp_ts <- slot(ts_c1, "avg_price")
# Executa decomposição
tmp_dec <- tslm(tmp_ts ~ trend + fourier(tmp_ts, K = 2))
tmp_trend <- coef(tmp_dec)[1] + coef(tmp_dec)['trend']*seq_along(tmp_ts)
tmp_comp <- cbind(
  Data = tmp_ts, 
  Trend = tmp_trend,
  Season = tmp_ts - tmp_trend - residuals(tmp_dec),
  Remainder = residuals(tmp_dec)
)
autoplot(tmp_comp, facets = TRUE)
ets(window(slot(ts_c1, "orders"), start = c(2017, 1), end = c(2018, 6)),
    model = "ZZZ")


forecast(window(slot(ts_c1, "orders"), start = c(2017, 1), end = c(2018, 6)),
         h = 90,
         level = 0.8)

# GC
tmp_dec$residuals[1:10]
tmp_ts[1:10]

plot(tmp_dec$residuals)
plot(tmp_ts)
decompose(slot(ts_c1, "orders"))
