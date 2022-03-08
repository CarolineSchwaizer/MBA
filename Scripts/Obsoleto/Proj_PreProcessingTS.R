# Tratamento de Dados -----------------------------------------------------

# Bibliotecas
library(dplyr)
library(sqldf)
library(tidyverse)
library(readr)
library(lubridate)
library(forecast)
library(zoo)

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



# Agregações por Data -----------------------------------------------------
df_base_agr <- sqldf("
          SELECT
            --order_date as order_date__Date, 
            order_date as order_date__yearmon, 
            cluster as cluster__factor,
            category as category__factor,
            COUNT(order_item_id) as orders__numeric,
            SUM(price)/COUNT(order_id) as avg_price__numeric,
            SUM(price) as revenue__numeric
          FROM df_base
          GROUP BY
            order_date, 
            cluster, 
            category",
                     method = "name__class")



# Agregações usando XTS ---------------------------------------------------
library(tseries)
library(xts)
library(lubridate)
df_cluster1 <- filter(df_base_agr, cluster == "1")
df_cluster2 <- filter(df_base_agr, cluster == "2")
df_cluster3 <- filter(df_base_agr, cluster == "3")


# Separa series por cluster:
ts_c1 <- xts(df_cluster1[,c(-1, -2, -3)], 
             order.by = df_cluster1$order_date)

ts_c2 <- xts(df_cluster2[,c(-1, -2, -3)], 
             order.by = df_cluster2$order_date)

ts_c3 <- xts(df_cluster3[,c(-1, -2, -3)], 
             order.by = df_cluster3$order_date)

  # GC
  rm(df_cluster1, df_cluster2, df_cluster3)

# Séries Base -------------------------------------------------------------


# Cria agregações de volumes e receitas geradas. 
# Objetos Temporários
temp_c1_orders <- apply.monthly(ts_c1[,"orders"], mean)
temp_c1_price <- apply.monthly(ts_c1[,"avg_price"], mean)
temp_c1_volume <- apply.monthly(ts_c1[,"revenue"], mean)
temp_c2_orders <- apply.monthly(ts_c2[,"orders"], mean)
temp_c2_price <- apply.monthly(ts_c2[,"avg_price"], mean)
temp_c2_volume <- apply.monthly(ts_c2[,"revenue"], mean)
temp_c3_orders <- apply.monthly(ts_c3[,"orders"], mean)
temp_c3_price <- apply.monthly(ts_c3[,"avg_price"], mean)
temp_c3_volume <- apply.monthly(ts_c3[,"revenue"], mean)

####### Cluster 1
ts_c1 <- merge(temp_c1_orders,
               temp_c1_price,
               join = "inner") %>% 
  merge(temp_c1_volume,
        join = "inner")

# Teste decomposição LOESS
decomp <- stl(ts_c1[,1], t.window = 7, s.window = 10, s.degree = 0, robust = TRUE)

# Converte de volta para dataframe. 
df_c1 <- data.frame(order_date = index(ts_c1), coredata(ts_c1))

        # GC
        rm(ts_c1, temp_c1_orders, temp_c1_price, temp_c1_volume)




###### Cluster 2
ts_c2 <- merge(temp_c2_orders,
               temp_c2_price,
               join = "inner") %>% 
  merge(temp_c2_volume,
        join = "inner")

# Converte de volta para dataframe. 
df_c2 <- data.frame(order_date = index(ts_c2), coredata(ts_c2))

        # GC
        rm(ts_c2, temp_c2_orders, temp_c2_price, temp_c2_volume)



####### Cluster 3
ts_c3 <- merge(temp_c3_orders,
               temp_c3_price,
               join = "inner") %>% 
  merge(temp_c3_volume,
        join = "inner")

# Converte de volta para dataframe. 
df_c3 <- data.frame(order_date = index(ts_c3), coredata(ts_c3))
        
        # GC
        rm(ts_c3, temp_c3_orders, temp_c3_price, temp_c3_volume)

        

# Pré-processamento Série Temporal ------------------------------------------------
  ### Temos menos que duas observações por período, então métodos 
  ### tradicionais de decomposição não podem ser aplicados (function: decompose())

library(forecast)

# Função customizada para decomposição:
ts_decompose <- function(df, k) {
  # Instancia variáves:
  vars <- c("orders", "avg_price", "volume")
  titles <- c("N. Ordens", "Preço Médio", "Volume de Vendas")
  # Lista para retorno.
  results <- list()
  for (i in 1:length(vars)){
    k <<- as.numeric(k)
    # Converte dataframe para objeto TS
    tmp_ts <- ts(data = as.numeric(
      paste0(df[,vars[i]])
      ),
      start = c(year(min(df[,1])),
                month(min(df[,1]))),
      frequency = 12)
    # Roda modelo linear para decomposição:
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
            labs(title = titles[i],
                 y = "Components") +
            theme_get()
    )
  }
  # GC Global
  rm(k)
  return(results)
}


# Séries Básicas ----------------------------------------------------------

ts_c1 <- ts(df_c1[,-1], start = c(2016, 09), frequency = 13)
autoplot(decompose(ts_c1))
findfrequency(ts_c1)
