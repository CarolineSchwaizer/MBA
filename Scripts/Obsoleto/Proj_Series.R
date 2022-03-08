# Tratamento de Dados -----------------------------------------------------

# Bibliotecas
library(dplyr)
library(sqldf)
library(tidyverse)
library(readr)

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
  mutate(order_date = as.Date(dt_purchase)) %>% 
  #mutate(order_date = as.yearmon(dt_purchase)) %>% 
  arrange(dt_purchase, order_id, order_item_id) 


# Agregações por Data -----------------------------------------------------
df_base_agr <- sqldf("
          SELECT
            order_date as order_date__Date, 
            --order_date as order_date__yearmon, 
            cluster as cluster__factor,
            category as category__factor,
            COUNT(order_item_id) as orders__numeric, 
            avg(price) as avg_price__numeric,
            SUM(price) as volume__numeric
          FROM df_base
          GROUP BY
            order_date, 
            cluster, 
            category",
          method = "name__class")


# Series Temporais --------------------------------------------------------
library(tseries)
library(xts)
library(lubridate)
df_cluster1 <- filter(df_base_agr, cluster == "1")
df_cluster2 <- filter(df_base_agr, cluster == "2")
df_cluster3 <- filter(df_base_agr, cluster == "3")


# Separa series por cluster:
ts_c1 <- xts(df_cluster1[,c(-1, -2, -3)], 
             order.by = df_cluster1$order_date,
             frequency = 7)

ts_c2 <- xts(df_cluster2[,c(-1, -2, -3)], 
             order.by = df_cluster2$order_date,
             frequency = 7)

ts_c3 <- xts(df_cluster3[,c(-1, -2, -3)], 
             order.by = df_cluster3$order_date,
             frequency = 7)



# Séries Base -------------------------------------------------------------


# Cria agregações de volumes e receitas geradas. 
      # Objetos Temporários
      temp_c1_orders <- apply.weekly(ts_c1[,"orders"], sum)
      temp_c1_price <- apply.weekly(ts_c1[,"avg_price"], mean)
      temp_c1_volume <- apply.weekly(ts_c1[,"volume"], sum)
      temp_c2_orders <- apply.weekly(ts_c2[,"orders"], sum)
      temp_c2_price <- apply.weekly(ts_c2[,"avg_price"], mean)
      temp_c2_volume <- apply.weekly(ts_c2[,"volume"], sum)
      temp_c3_orders <- apply.weekly(ts_c3[,"orders"], sum)
      temp_c3_price <- apply.weekly(ts_c3[,"avg_price"], mean)
      temp_c3_volume <- apply.weekly(ts_c3[,"volume"], sum)
# Cluster 1
ts_c1 <- merge(temp_c1_orders,
               temp_c1_price,
               join = "inner") %>% 
         merge(temp_c1_volume,
               join = "inner")
        # GC
        rm(temp_c1_orders, temp_c1_price, temp_c1_volume)

# Cluster 2
ts_c2 <- merge(temp_c2_orders,
                       temp_c2_price,
                       join = "inner") %>% 
          merge(temp_c2_volume,
                join = "inner")
        # GC
        rm(temp_c2_orders, temp_c2_price, temp_c2_volume)
        
# Cluster 3
ts_c3 <- merge(temp_c3_orders,
                       temp_c3_price,
                       join = "inner") %>% 
          merge(temp_c3_volume,
                join = "inner")
        # GC
        rm(temp_c3_orders, temp_c3_price, temp_c3_volume)
        

# Análise Gráfica ---------------------------------------------------------
library(ggplot2)
library(patchwork)

p_c1 <- ggplot(ts_c1, aes(x = index(ts_c1))) + theme_get() + 
  labs(title = "Cluster 1")
 (p_c1 + geom_line(aes(y = orders)) + p_c1 + geom_line(aes(y = volume))) /
  p_c1 + geom_line(aes(y = avg_price))

p_c2 <- ggplot(ts_c2, aes(x = index(ts_c2))) + theme_get() +
  labs(title = "Cluster 2")
(p_c2 + geom_line(aes(y = orders)) + p_c2 + geom_line(aes(y = volume))) /
  p_c2 + geom_line(aes(y = avg_price))

p_c3 <- ggplot(ts_c3, aes(x = index(ts_c3))) + theme_get() +
  labs(title = "Cluster 3")
(p_c3 + geom_line(aes(y = orders)) + p_c3 + geom_line(aes(y = volume))) /
  p_c3 + geom_line(aes(y = avg_price))

# Análise Série Temporal ---------------------------------------------------
### Temos menos que duas observações por período, então métodos 
### tradicionais de decomposição não podem ser aplicados (function: decompose())
library(forecast)

temp <- select(df_cluster1, order_date, orders) %>% 
  group_by(order_date = as.numeric(order_date)) %>% 
  summarise(n_orders = sum(orders))


ts_c1 <- ts(temp[,2], start = c(2017, 01), frequency = 12)
  
decompose_c1 <- tslm(ts_c1 ~ trend + fourier(ts_c1, 2))

decompose_c1 <- tslm(ts_c1[,1] ~ trend + fourier(ts_c1, 2))



decompose(as.ts(ts_c1[,"orders"], frequency = 52))


decompose(ts(ts_c1[,"orders"], frequency = 52))
