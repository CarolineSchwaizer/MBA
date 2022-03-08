# Bibliotecas -------------------------------------------------------------

library(dplyr)
library(sqldf)
library(tidyverse)
library(readr)
library(lubridate)
library(forecast)
library(zoo)
library(ggplot2)
library(patchwork)
library(urca)

# Funções e Estruturas -----------------------------------------------------------------

# Função customizada para decomposição:
f.decompose <- function(tsList, k) {
  # Instancia variáves:
  vars <- c("orders", "avg_price", "revenue")
  titles <- c("N. Médio de Ordens", "Preço Médio", "Receita Média")
  objname <- deparse(substitute(tsList))
  # Lista para retorno.
  results <- list()
  for (i in 1:length(vars)){
    k <<- as.numeric(k)
    
    # Instancia slot da Classe TsList como 'ts'
    tmp_ts <- slot(tsList, vars[i])
    # Executa decomposição
    tmp_dec <- tslm(tmp_ts ~ trend + fourier(tmp_ts, K = k))
    tmp_trend <- coef(tmp_dec)[1] + coef(tmp_dec)['trend']*seq_along(tmp_ts)
    tmp_comp <- cbind(
      Data = tmp_ts, 
      Trend = tmp_trend,
      Season = tmp_ts - tmp_trend - residuals(tmp_dec),
      Remainder = residuals(tmp_dec)
    )
    # GC
    rm(tmp_ts, tmp_dec, tmp_trend)
    
    # Armazena resultados:
    results <- append(results, 
                      list(tmp_comp))
    # Saída gráfica:
    print(autoplot(tmp_comp, facets = T) + 
            labs(title = paste(objname, titles[i], sep = ' '),
                 y = "Components") +
            theme_get()
    )
  }
  # GC Global
  rm(k)
  return(results)
}

# Função para correção de NAs e Outliers:
f.dffix <- function(df) {
  df_tmp <- data.frame(df)
  ind <- c(colnames(df_tmp))
  if (sum(!complete.cases(df)) > 0) {
    for (i in 2:length(ind)) {
      df_tmp[,i] <- na.interp(df_tmp[,i])
    }
  }
  for (i in 2:length(ind)) {
    df_tmp[ # Usa index para localizar ocorrências
      tsoutliers(df_tmp[,i])$index,
      i] <- tsoutliers(df_tmp[,i])$replacements
  }
  return(df_tmp)
}

# Classe para armazenar multiplas 'ts'
setClass(Class="TsList",
         representation(
           orders="ts",
           avg_price="ts",
           revenue="ts",
           s.orders="ts", 
           s.avg_price="ts",
           s.revenue="ts")
)

# Função para processar data.frame em 'ts'
f.df2ts <- function(df, k){
  # Nomes das variáveis
  vars <- colnames(df)
  
  if (k <= 365 & k > 12){
    # Instancia lista de datas para TS
    inds <-  seq(as.Date(min(dates$dates)), 
                 as.Date(max(dates$dates)), by = "day")
    
    for (i in vars){
      name <- paste("ts_", as.character(i), sep = '')
      ts_tmp <- ts(df[,i], 
                   start = as.Date(min(dates)),
                   frequency = k, class = "ts")
      assign(as.character(name),
             ts_tmp)
    }
    
    return(new("TsList",
               orders=ts_orders,
               avg_price=ts_avg_price,
               revenue=ts_revenue,
               s.orders=if (exists("ts_s.orders")){
                 ts_s.orders } else { as.ts(1)},
               s.avg_price=if (exists("ts_s.avg_price")){
                 ts_s.avg_price} else {as.ts(1)},
               s.revenue=if (exists("ts_s.revenue")){
                 ts_s.revenue} else {as.ts(1)}))
    
  } else if (k <= 12) {
    inds <-  seq(as.Date(min(dates$dates)),
                 as.Date(max(dates$dates)), by = "month")
    
    for (i in vars){
      name <- paste("ts_", as.character(i), sep = '')
      ts_tmp <- ts(df[,i], 
                   #start = c(year(min(inds)),
                   #           month(min(inds))),
                   # Remove 'start' para criar 
                   # períodos dummy determinados por freq.
                   frequency = 12, class = "ts")
      
      assign(as.character(name), ts_tmp)
    }
    return(new("TsList",
               orders=ts_orders,
               avg_price=ts_avg_price,
               revenue=ts_revenue,
               s.orders=if (exists("ts_s.orders")){
                 ts_s.orders } else { as.ts(1)},
               s.avg_price=if (exists("ts_s.avg_price")){
                 ts_s.avg_price} else {as.ts(1)},
               s.revenue=if (exists("ts_s.revenue")){
                 ts_s.revenue} else {as.ts(1)}))
  } else {
    return(print('Incorrect parameters'))
  }
}

# Tratamento Dados --------------------------------------------------------

# Carrega Base:
df_base <- read_csv2(file = "./Data/df_proj_order_items_cluster.csv",
                     col_names = TRUE, locale = locale("en"),
                     col_types = cols(dt_purchase = col_datetime(
                       format = "%Y/%m/%d %H:%M:%S"), 
                       dt_approved = col_datetime(
                         format = "%Y/%m/%d %H:%M:%S"),
                       cluster = col_character())
)

# Limpa variáveis:
df_base <- select(df_base,
                  dt_purchase,
                  order_id, order_item_id, 
                  product_id, category, 
                  seller_id,
                  cluster, 
                  price) %>% 
  # Remove os clusters 'NA's
  filter(!is.na(cluster)) %>%
  # Recodifica NAs de Categoria 
  mutate(category = ifelse(is.na(category), "nao_informada", category)) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(category = as.factor(category)) %>% 
  #mutate(order_date = as.Date(dt_purchase)) %>% 
  mutate(order_date = as.yearmon(dt_purchase)) %>% 
  arrange(dt_purchase, order_id, order_item_id)

# Cria DF com dados diários
df_daily <- select(df_base,
                   dt_purchase,
                   order_id, order_item_id, 
                   product_id, category, 
                   seller_id,
                   cluster, 
                   price) %>% 
  # Remove os clusters 'NA's
  filter(!is.na(cluster)) %>%
  # Recodifica NAs de Categoria 
  mutate(category = ifelse(is.na(category), "nao_informada", category)) %>%
  mutate(cluster = as.factor(cluster)) %>%
  mutate(category = as.factor(category)) %>% 
  mutate(order_date = as.Date(dt_purchase)) %>% 
  #mutate(order_date = as.yearmon(dt_purchase)) %>% 
  arrange(dt_purchase, order_id, order_item_id) %>% 
  # #Filtra outlier:
  # filter(seller_id != "S000902" & order_date != '2017-08-29')
  # Filtra datas
  filter(between(order_date, as.Date('2017-01-01'), as.Date('2018-08-29')))


# Pré-agregação por data
df_daily_agr <- sqldf("
          SELECT
            order_date as order_date__Date, 
            --order_date as order_date__yearmon, 
            cluster as cluster__factor,
            --category as category__factor,
            COUNT(order_item_id) as orders__numeric, 
            SUM(price)/COUNT(order_id) as avg_price__numeric,
            SUM(price) as revenue__numeric
          FROM df_daily
          GROUP BY
            order_date, 
            cluster
            --category",
                      method = "name__class")


# Pré-processamento: agregações temporais --------------------------------------

library(tseries)
library(xts)
library(lubridate)
df_c1 <- filter(df_daily_agr, cluster == "1") %>% select(-cluster)
df_c2 <- filter(df_daily_agr, cluster == "2") %>% select(-cluster)
df_c3 <- filter(df_daily_agr, cluster == "3") %>% select(-cluster)


# # Separa em series XTS por cluster:
# # Finalidade do xts é facilitar as agregações por dia
# ts_c1 <- xts(df_cluster1[,c(-1, -2, -3)], 
#              order.by = df_cluster1$order_date)
# 
# ts_c2 <- xts(df_cluster2[,c(-1, -2, -3)], 
#              order.by = df_cluster2$order_date)
# 
# ts_c3 <- xts(df_cluster3[,c(-1, -2, -3)], 
#              order.by = df_cluster3$order_date)
# 
# # GC
# rm(df_cluster1, df_cluster2, df_cluster3)
# 
# 
# # Cria agregações de volumes e receitas geradas. 
# # Objetos Temporários
# temp_c1_orders <- apply.daily(ts_c1[,"orders"], sum)
# temp_c1_price <- apply.daily(ts_c1[,"avg_price"], mean)
# temp_c1_volume <- apply.daily(ts_c1[,"revenue"], mean)
# temp_c2_orders <- apply.daily(ts_c2[,"orders"], mean)
# temp_c2_price <- apply.daily(ts_c2[,"avg_price"], mean)
# temp_c2_volume <- apply.daily(ts_c2[,"revenue"], mean)
# temp_c3_orders <- apply.daily(ts_c3[,"orders"], mean)
# temp_c3_price <- apply.daily(ts_c3[,"avg_price"], mean)
# temp_c3_volume <- apply.daily(ts_c3[,"revenue"], mean)
# 
# ####### Cluster 1
# ts_c1 <- merge(temp_c1_orders,
#                temp_c1_price,
#                join = "inner") %>% 
#   merge(temp_c1_volume,
#         join = "inner")
# 
# # Converte de volta para dataframe.
# 
# ###### Cluster 1
# df_c1 <- data.frame(order_date = index(ts_c1), coredata(ts_c1))
# 
# # GC
# rm(ts_c1, temp_c1_orders, temp_c1_price, temp_c1_volume)
# 
# 
# ###### Cluster 2
# ts_c2 <- merge(temp_c2_orders,
#                temp_c2_price,
#                join = "inner") %>% 
#   merge(temp_c2_volume,
#         join = "inner")
# 
# # Converte de volta para dataframe. 
# df_c2 <- data.frame(order_date = index(ts_c2), coredata(ts_c2))
# 
# # GC
# rm(ts_c2, temp_c2_orders, temp_c2_price, temp_c2_volume)
# 
# 
# ####### Cluster 3
# ts_c3 <- merge(temp_c3_orders,
#                temp_c3_price,
#                join = "inner") %>% 
#   merge(temp_c3_volume,
#         join = "inner")
# 
# # Converte de volta para dataframe. 
# df_c3 <- data.frame(order_date = index(ts_c3), coredata(ts_c3))
# 
# # GC
# rm(ts_c3, temp_c3_orders, temp_c3_price, temp_c3_volume)
# 
# 

# Série Temporal: Limpeza de Dados -------------------------------------------------

# Cria sequência comum de dias das transações.
dates <- data.frame(dates =
                      seq(as.Date(min(df_daily$order_date)),
                          as.Date(max(df_daily$order_date)),
                          by = "day")
)


# # Cria sequência comum de dias das transações, expande artificialmente dados
# # desde 29-08-2016 para completar 2 ciclos.  
# dates <- data.frame(dates =
#                       seq(as.Date("2016-08-29"), 
#                           as.Date(max(df_daily$order_date)), 
#                           by = "day")
# )


# Faz join com data frames segmentados.
df_c1 <- left_join(dates, 
                   df_c1,
                   by = c("dates" = "order_date"))

df_c2 <- left_join(dates, 
                   df_c2,
                   by = c("dates" = "order_date"))

df_c3 <- left_join(dates, 
                   df_c3,
                   by = c("dates" = "order_date"))



# Reprocessa os DF limpando NAs e ajustando outliers:
# Para remoção de NAs usamos forecast::na.interp()
# Para correção de outliers usamos forecast::tsoutliers

df_c1_fix <- f.dffix(df_c1)
df_c2_fix <- f.dffix(df_c2)
df_c3_fix <- f.dffix(df_c3)

# Compara séries:
# Cluster 1
ggplot(tidyr::pivot_longer(df_c1, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 1: outliers')

ggplot(tidyr::pivot_longer(df_c1_fix, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 1: após correção')

# Cluster 2
ggplot(tidyr::pivot_longer(df_c2, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 2: outliers')

ggplot(tidyr::pivot_longer(df_c2_fix, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 2: após correção')

# Cluster 3
ggplot(tidyr::pivot_longer(df_c3, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 3: outliers')

ggplot(tidyr::pivot_longer(df_c3_fix, -dates), 
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol =1, 
             scales = "free") + labs(title='Cluster 3: após correção')


# GC
rm(df_c1, df_c2, df_c3)


# Série Temporal: Armazena Correções --------------------------------------

df_monthly_c1 <- df_c1_fix %>% mutate(dates = as.yearmon(dates)) %>% 
  group_by(dates) %>% summarise(orders = sum(orders),
                                avg_price = sum(avg_price),
                                revenue = sum(revenue))

ggplot(tidyr::pivot_longer(df_monthly_c1, -dates),
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol=1,
             scales = "free") + labs(title = 'Cluster 1: mensal')

df_monthly_c2 <- df_c2_fix %>% mutate(dates = as.yearmon(dates)) %>% 
  group_by(dates) %>% summarise(orders = sum(orders),
                                avg_price = sum(avg_price),
                                revenue = sum(revenue))

ggplot(tidyr::pivot_longer(df_monthly_c2, -dates),
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol=1,
             scales = "free") + labs(title = 'Cluster 2: mensal')

df_monthly_c3 <- df_c3_fix %>% mutate(dates = as.yearmon(dates)) %>% 
  group_by(dates) %>% summarise(orders = sum(orders),
                                avg_price = sum(avg_price),
                                revenue = sum(revenue))

ggplot(tidyr::pivot_longer(df_monthly_c3, -dates),
       aes(x=dates, y=value)) + geom_line() + 
  facet_wrap(vars(name), ncol=1,
             scales = "free") + labs(title = 'Cluster 3: mensal')

write.csv2(df_monthly_c1, file = "./Data/Cluster1_monthly.csv",
           col.names = TRUE)
write.csv2(df_monthly_c2, file = "./Data/Cluster2_monthly.csv",
           col.names = TRUE)
write.csv2(df_monthly_c3, file = "./Data/Cluster3_monthly.csv",
           col.names = TRUE)
write.csv2(df_c1_fix, file = "./Data/Cluster1_daily.csv",
           col.names = TRUE)
write.csv2(df_c2_fix, file = "./Data/Cluster2_daily.csv",
           col.names = TRUE)
write.csv2(df_c3_fix, file = "./Data/Cluster3_daily.csv",
           col.names = TRUE)


rm(df_base, df_daily, df_daily_agr)

# Série Temporal: definição -----------------------------------------------
# Cria séries temporais para modelo. 
inds <-  seq(as.Date(min(dates$dates)), 
             as.Date(max(dates$dates)), by = "day")

# Série Cluster 1:
ts_c1 <- f.df2ts(df_c1_fix, 365)
f.decompose(ts_c1, 60)

# Série Cluster 2:
ts_c2 <- f.df2ts(df_c2_fix, 365)
f.decompose(ts_c2, 60)  


# Série Cluster 3:
ts_c3 <- f.df2ts(df_c3_fix, 365)
f.decompose(ts_c3, 60)  


# Testes com mensais:
ts_m_c1 <- f.df2ts(df_monthly_c1, 12)
ts_m_c2 <- f.df2ts(df_monthly_c2, 12)
ts_m_c3 <- f.df2ts(df_monthly_c3, 12)

f.decompose(ts_m_c1, 2)
f.decompose(ts_m_c2, 2)
f.decompose(ts_m_c3, 2)

cor(df_c1_fix[,-1])
cor(df_c2_fix[,-1])
cor(df_c3_fix[,-1])

cor(df_monthly_c1[,-1])
cor(df_monthly_c2[,-1])
cor(df_monthly_c3[,-1])

# GC
#rm(dates, inds)

### Os testes demostram que:
# 1. Existe uma tendência clara de crescimento para c1 e c3. 
# 2. Existe uma tendência clara de queda para receitas de c2. 
# 3. De modo geral, a há uma sazonalidade clara/regular entre os clusters. 
#     Apenas ciclos muito longos foram detectados, mas que não seguem um padrão
#     claro entre si.
#     Com exceção dos preços médios para o cluster 1, que possuem um ciclo de 
#     alta para cada 2 meses. Causa não identificada.
# 4. Sazonalidade será um componente difícil de modelar. 
# 5. A análise de correlações das variáveis demostra que o número de ordens 
#     será um previsor mais útil. 
#       a. Não possui correlação forte com os preços médios. 
#       b. Possui correlação muito forte com receita.



# Modelos -----------------------------------------------------------------

# Cria séries normalizadas para testes futuros
# Diárias
df_c1_fix$s.orders <- scale(df_c1_fix$orders)
df_c2_fix$s.orders <- scale(df_c2_fix$orders)
df_c3_fix$s.orders <- scale(df_c3_fix$orders)

# Mensais
df_monthly_c1$s.orders <- scale(df_monthly_c1$orders)
df_monthly_c2$s.orders <- scale(df_monthly_c2$orders)
df_monthly_c3$s.orders <- scale(df_monthly_c3$orders)

# Atualiza 'ts' para vetor escalado.
ts_c1 <- f.df2ts(df_c1_fix, 240/8)
ts_c2 <- f.df2ts(df_c2_fix, 240/8)
ts_c3 <- f.df2ts(df_c3_fix, 240/8)

ts_m_c1 <- f.df2ts(df_monthly_c1, 12)
ts_m_c2 <- f.df2ts(df_monthly_c2, 12)
ts_m_c3 <- f.df2ts(df_monthly_c3, 12)


# Cria amostas de treinamento e validação:
decompose(ts_c1@orders, type = "multiplicative") %>% autoplot()

# Média Móvel -------------------------------------------------------------

## SÉRIES DIÁRIAS ##
# Cluster 1:
autoplot(ts_c1@orders, series = "Orders") +
  autolayer(ma(ts_c1@orders, 30), series="30-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster1: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "30-MA"="red"),
                      breaks=c("Orders", "30-MA"))


# Cluster 2:
autoplot(ts_c2@orders, series = "Orders") +
  autolayer(ma(ts_c2@orders, 30), series="30-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster 2: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "30-MA"="red"),
                      breaks=c("Orders", "30-MA"))


# Cluster 3:
autoplot(ts_c3@orders, series = "Orders") +
  autolayer(ma(ts_c3@orders, 30), series="30-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster 3: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "30-MA"="red"),
                      breaks=c("Orders", "30-MA"))


## SÉRIES MENSAIS

# Cluster 1:
autoplot(ts_m_c1@orders, series = "Orders") +
  autolayer(ma(ts_m_c1@orders, 2), series="2-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster1: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "2-MA"="red"),
                      breaks=c("Orders", "2-MA"))


# Cluster 2:
autoplot(ts_m_c2@orders, series = "Orders") +
  autolayer(ma(ts_m_c2@orders, 2), series="2-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster 2: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "2-MA"="red"),
                      breaks=c("Orders", "2-MA"))


# Cluster 3:
autoplot(ts_m_c3@orders, series = "Orders") +
  autolayer(ma(ts_m_c3@orders, 2), series="2-MA") +
  xlab("Período") + 
  ylab("No. médio de ordens") +
  ggtitle("Cluster 3: Número médio de ordens diárias") +
  scale_colour_manual(values=c("Orders"="grey50", "2-MA"="red"),
                      breaks=c("Orders", "2-MA"))


# Para todos os casos, mas menos acentuado no cluster 2, as séries mostram uma
# mudança de comportamento a partir de 2018. 
# Para os dados diários, como a granularidade é maior, optamos por reduzir a série
# a apenas a 2018.


# Modelos: Reduzindo série ------------------------------------------------

# Redução apenas para "Orders", demais séries ainda tem 602 períodos.
slot(ts_c1, "orders") <- window(ts_c1@orders, start = 362)
slot(ts_c2, "orders") <- window(ts_c2@orders, start = 362)
slot(ts_c3, "orders") <- window(ts_c3@orders, start = 362)


# Modelo Linear -----------------------------------------------------------

# Cluster 1
m_linear_c1 <- tslm(ts_c1@orders ~ trend)
summary(m_linear_c1)
plot(ts_c1@orders)
lines(m_linear_c1$fitted.values, lwd=2)
Acf(m_linear_c1$residuals)

# Cluster 2
m_linear_c2 <- tslm(ts_c2@orders ~ trend)
summary(m_linear_c2)
plot(ts_c2@orders)
lines(m_linear_c2$fitted.values, lwd=2)
Acf(m_linear_c2$residuals)

# Cluster 3
m_linear_c3 <- tslm(ts_c3@orders ~ trend)
summary(m_linear_c3)
plot(ts_c3@orders)
lines(m_linear_c3$fitted.values, lwd=2)
Acf(m_linear_c3$residuals)




# Modelo Polinomial -------------------------------------------------------

# Cluster 1
m_poli_c1 <- tslm(ts_c1@orders + trend + I(trend^2))
plot(ts_c1@orders)
lines(m_poli_c1$fitted.values, lwd=2)
checkresiduals(m_poli_c1, test="LB")

# Cluster 2
m_poli_c2 <- tslm(ts_c2@orders ~ trend + I(trend^2))
plot(ts_c2@orders)
lines(m_poli_c2$fitted.values, lwd=2)
checkresiduals(m_poli_c2, test="LB")

# Cluster 3
m_poli_c3 <- tslm(ts_c3@orders ~ trend + I(trend^2))
plot(ts_c3@orders)
lines(m_poli_c3$fitted.values, lwd=2)
checkresiduals(m_poli_c3, test="LB")


m_linear_c1 <- tslm(ts_c1@s.orders ~ trend)
checkresiduals(m_linear_c1, test = 'LB')


rm(df_monthly_c1, df_monthly_c2, df_monthly_c3,
   ts_m_c1, ts_m_c2, ts_m_c3)

# Modelo Final ------------------------------------------------------------
##### MDOELO FINAL #####
 # - Estacionar a série: remover componente de sazonalidade através de decomposição
 #    usando STL + trendcycle. Justificativa: flutuações sazonais não são relevantes para 
 #    planejamento de longo prazo e expansão dos clientes. 
 #  - Fazer projeções com
 #      a. Método de tendência Holt (damped) como referência ("naive") para erro. 
 #      b. Auto Arima sem sazonalidade (decomposto) para projeção final.

# Recria objetos das séries:
  # Reduz escopo devido à mudança significativa no perfil da série.
  df_cluster1 <- df_c1_fix %>% filter(dates >= as.Date("2018-01-01"))
  df_cluster2 <- df_c2_fix %>% filter(dates >= as.Date("2018-01-01"))
  df_cluster3 <- df_c3_fix %>% filter(dates >= as.Date("2018-01-01"))

  #GC
  rm(df_c1_fix, df_c2_fix, df_c3_fix,
     ts_c1, ts_c2, ts_c3)
  
  # Recria séries temporais, agora para 2018 apenas (241 dias)
  ts_cluster1 <- f.df2ts(df_cluster1, 241/8)
  ts_cluster2 <- f.df2ts(df_cluster2, 241/8)
  ts_cluster3 <- f.df2ts(df_cluster3, 241/8)

  # Amostra testes:
  ts_c1_orders_training <- window(ts_cluster1@orders, start = 1, end = 6.941909)
  ts_c2_orders_training <- window(ts_cluster2@orders, start = 1, end = 6.941909)
  ts_c3_orders_training <- window(ts_cluster3@orders, start = 1, end = 6.941909)
  
  # Amostra validação:
  ts_c1_orders_valid <- window(ts_cluster1@orders, start = 6.975104)
  ts_c2_orders_valid <- window(ts_cluster2@orders, start = 6.975104)
  ts_c3_orders_valid <- window(ts_cluster3@orders, start = 6.975104)


  rm(ts_cluster1,ts_cluster2, ts_cluster3)
# Balizamento: Holt + Damped ----------------------------------------------

  ## Usamos Holt damped para criar uma referência de acurácia,
  ## em vez de usar apenas um modelo naïve. 
  
  # Cluster 1:
  hw_c1 <- holt(ts_c1_orders_training,
                h = 61,
                phi = 0.9,
                damped = TRUE, 
                seasonal = "multiplicative")
  autoplot(ts_c1_orders_training) +
    autolayer(hw_c1, series = "Damped Holt") +
    autolayer(ts_c1_orders_valid, series = "Validation")
  
  accuracy(hw_c1, ts_c1_orders_valid)
  
  # Cluster 2:
  hw_c2 <- holt(ts_c2_orders_training,
                h = 61,
                phi = 0.9,
                damped = TRUE, 
                seasonal = "additive")
  autoplot(ts_c2_orders_training) +
    autolayer(hw_c2, series = "Damped Holt") +
    autolayer(ts_c2_orders_valid, series = "Validation")
  
  accuracy(hw_c2, ts_c2_orders_valid)
  
  # Cluster 3:
  hw_c3 <- holt(ts_c3_orders_training,
                h = 61,
                phi = 0.9,
                damped = TRUE, 
                seasonal = "additive")
  autoplot(ts_c3_orders_training) +
    autolayer(hw_c3, series = "Damped Holt") +
    autolayer(ts_c3_orders_valid, series = "Validation")
  
  accuracy(hw_c3, ts_c3_orders_valid)
  
  
  #### Análise dos resíduos:
  plot(hw_c1$residuals)
  Acf(hw_c1$residuals)
  
  plot(hw_c2$residuals)
  Acf(hw_c2$residuals)
  
  plot(hw_c3$residuals)
  Acf(hw_c3$residuals)
      ### Ainda há indícios claros de sazonalidade. 
  
rm(hw_c1, hw_c2, hw_c3)

# Modelo Auto Regressivo - série estacionada.  ----------------------------
# 
# O componente de sazonalidade não é relevante para a projeção a ser criada. 
# A sazonalidade identificada na análise de autocorrelação indica picos semanais de repetição
# que não são importantes para o Forecast. Por este motivo, optou-se por ignorar a sazonalidade e 
# "estacionar" a série para analisar apenas os demais componentes. 
  
  ts_diff_c1_orders_training <- diff(ts_c1_orders_training, 7)
  ts_diff_c2_orders_training <- diff(ts_c2_orders_training, 7)
  ts_diff_c3_orders_training <- diff(ts_c3_orders_training, 7)
  
  ts_diff_c1_orders_training %>% ur.kpss() %>% summary()
  ts_diff_c2_orders_training %>% ur.kpss() %>% summary()
  ts_diff_c3_orders_training %>% ur.kpss() %>% summary()

  ## Segundo KPSS, as 3 séries estacionam com 7 lags, indicando uma sazonalidade
  ## semanal nos dados que não é relevante para fins de forecast. 
  
  ar_c1 <- auto.arima(ts_diff_c1_orders_training, 
                      seasonal = FALSE, stepwise = FALSE,
                      approximation = FALSE)
  
  summary(ar_c1)
  
  ar_c1_s <-auto.arima(ts_c1_orders_training,
                       seasonal = TRUE, stepwise = FALSE,
                       approximation = FALSE)
  summary(ar_c1_s)
  