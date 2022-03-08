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
    # Instancia data inicial para TS
    inds <-  as.Date(min(dates$dates))
    
    for (i in vars){
      name <- paste("ts_", as.character(i), sep = '')
      ts_tmp <- ts(df[,i], 
                   start = c(as.numeric(format(inds[1], '%Y')),
                             as.numeric(format(inds[1], '%j'))
                             ),
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
    inds <-  as.Date(min(dates$dates))
    
    for (i in vars){
      name <- paste("ts_", as.character(i), sep = '')
      ts_tmp <- ts(df[,i], 
                   start = c(as.numeric(format(inds[1], '%Y')),
                             as.numeric(format(inds[1], '%m'))
                             ),
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

# Série Temporal: Limpeza de Dados -------------------------------------------------

# Cria sequência comum de dias das transações.
dates <- data.frame(dates =
                      seq(as.Date(min(df_daily$order_date)),
                          as.Date(max(df_daily$order_date)),
                          by = "day")
)


# # Cria sequência comum de dias das transações, expande artificialmente dados
# # desde 29-08-2016 para completar 2 ciclos. 
## Anula sequência anterior:
dates <- data.frame(dates =
                      seq(as.Date("2016-08-29"),
                          as.Date(max(df_daily$order_date)),
                          by = "day")
)


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

# Dados Mensais com criação de valores dummy nos meses iniciais 
# para suprir os 2 períodos:

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

# GC
rm(df_base, df_daily, df_daily_agr)


# Série Temporal: definição -----------------------------------------------
      ### Correlações entre as variáveis justificam a escolha de "Ordens"
      ### como única variável a ser analizada. 
      cor(df_c1_fix[,-1])
      cor(df_c2_fix[,-1])
      cor(df_c3_fix[,-1])
      
      cor(df_monthly_c1[,-1])
      cor(df_monthly_c2[,-1])
      cor(df_monthly_c3[,-1])


      # Cria séries temporais para modelo. 
      inds <-  seq(as.Date(min(dates$dates)), 
                   as.Date(max(dates$dates)), by = "day")

  # Série Cluster 1:
    ts_c1 <- f.df2ts(df_c1_fix, 365)
    f.decompose(ts_c1, 7)
    decompose(ts_c1@orders) %>% autoplot(facets = TRUE)

  # Série Cluster 2:
    ts_c2 <- f.df2ts(df_c2_fix, 365)
    f.decompose(ts_c2, 7)  
    decompose(ts_c2@orders) %>% autoplot(facets = TRUE)
  
  # Série Cluster 3:
    ts_c3 <- f.df2ts(df_c3_fix, 365)
    f.decompose(ts_c3, 7)  
    decompose(ts_c3@orders) %>% autoplot(facets = TRUE)

  # Testes com mensais:
    ts_m_c1 <- f.df2ts(df_monthly_c1, 12)
    ts_m_c2 <- f.df2ts(df_monthly_c2, 12)
    ts_m_c3 <- f.df2ts(df_monthly_c3, 12)

  # Decomposição Customizada com Fourier:
    f.decompose(ts_m_c1, 3)
    f.decompose(ts_m_c2, 3)
    f.decompose(ts_m_c3, 3)
  
  # Decomposição regular (Hyndman)
    decompose(ts_m_c1@orders) %>% autoplot(facets = TRUE)
    decompose(ts_m_c2@orders) %>% autoplot(facets = TRUE)
    decompose(ts_m_c3@orders) %>% autoplot(facets = TRUE)

# GC
rm(dates, inds)

### Os testes demostram que:
# 1. Existe uma tendência clara de crescimento de ordens para todos os clusters.
# 2. De modo geral, não há uma sazonalidade clara/regular entre os clusters. 
#     Apenas ciclos muito longos foram detectados, mas que não seguem um padrão
#     claro entre si.
#     Com exceção dos preços médios para o cluster 1, que possuem um ciclo de 
#     alta para cada 2 meses. Causa não identificada.
# 3. Sazonalidade será um componente difícil de modelar. 
# 4. A análise de correlações das variáveis demostra que o número de ordens 
#     será um previsor mais útil. 
#       a. Não possui correlação forte com os preços médios. 
#       b. Possui correlação muito forte com receita.



# Separação de Amostras ---------------------------------------------------

### Dados diários:
  # Devido a mudança de comportamento na curva quando observadas as ordens diárias, 
  # acredito ser melhor trabalhar apenas com os últimos períodos para a modelagem.
  # A partir de 2018.400, as séries mudam significativamente de nível para os clusters 
  # 1 e 3, o quê justifica o corte a partir de Maio, 2018. 
  window(ts_c1@orders, start = 2018.000) %>% plot()
  window(ts_c2@orders, start = 2018.000) %>% plot()
  window(ts_c3@orders, start = 2018.000) %>% plot()
  
  
  ts_d_c1 <- window(ts_c1@orders, start = 2018.400)
  ts_d_c2 <- window(ts_c2@orders, start = 2018.400)
  ts_d_c3 <- window(ts_c3@orders, start = 2018.400)
  

  # Reduzindo período para série diária:
  ts_d_c1_treino <- window(ts_d_c1, start = 2018.400, end = 2018.630)
  ts_d_c1_valida <- window(ts_d_c1, start = 2018.630)
  
  
  ts_d_c2_treino <- window(ts_d_c2, start = 2018.400, end = 2018.630)
  ts_d_c2_valida <- window(ts_d_c2, start = 2018.630)
 
  
  ts_d_c3_treino <- window(ts_d_c3, start = 2018.400, end = 2018.630)
  ts_d_c3_valida <- window(ts_d_c3, start = 2018.630)



### Dados mensais:
  # Os dados entre 2016/08 e 2016/12 foram dados dummy, criados apenas para fechar 
  # os ciclos de 2 períodos completos para que os algoritmos de Hyndman funcionem. 
  # Isso possivelmente vai comprometer a amostra de treinamento. 
  ts_m_c1_treino <- window(ts_m_c1@orders, start = c(2016, 08), end = c(2018, 05))
  ts_m_c1_valida <- window(ts_m_c1@orders, start = c(2018, 05))
  

  ts_m_c2_treino <- window(ts_m_c2@orders, start = c(2016, 08), end = c(2018, 05))
  ts_m_c2_valida <- window(ts_m_c2@orders, start = c(2018, 05))
  
  ts_m_c3_treino <- window(ts_m_c3@orders, start = c(2016, 08), end = c(2018, 05))
  ts_m_c3_valida <- window(ts_m_c3@orders, start = c(2018, 05))
  
# GC
  rm(df_monthly_c1, df_monthly_c2, df_monthly_c3,
     df_c1_fix, df_c2_fix, df_c3_fix,
     ts_c1, ts_c2, ts_c3)
  


# Modelos Diários ---------------------------------------------------------

# Diário: Holt-Damped -----
  
  # Para cria um modelo de referência para os testes de 'acurácia', 
  # utilizamos o método de Holt com atenuação da série. 
  
  # Cluster 1:
  m_holt_d_c1 <- holt(ts_d_c1_treino,
                      h = 17,
                      phi = 0.9,
                      damped = TRUE,
                      seasonal = "multiplicative")
  
  # Cluster 2:
  m_holt_d_c2 <- holt(ts_d_c2_treino,
                      h = 17,
                      phi = 0.9,
                      damped = TRUE, 
                      seasonal = "multiplicative")
  
  # Cluster 3:
  m_holt_d_c3 <- holt(ts_d_c3_treino,
                      h = 17,
                      phi = 0.9,
                      damped = TRUE, 
                      seasonal = "multiplicative")
  
  ### Com os modelos para os 3 clusters, podemos comparar as projeções de Holt
  ### para os 48 dias da amostra de valdação. 
  
      # Cluster 1:
      autoplot(ts_d_c1_treino) +
        autolayer(m_holt_d_c1, series = "Damped Holt") +
        autolayer(ts_d_c1_valida, series = "Validation")
      
      accuracy(m_holt_d_c1, ts_d_c1_valida)
  
      # Cluster 2:
      autoplot(ts_d_c2_treino) +
        autolayer(m_holt_d_c2, series = "Damped Holt") +
        autolayer(ts_d_c2_valida, series = "Validation")
      
      accuracy(m_holt_d_c2, ts_d_c2_valida)
      
      # Cluster 3:
      autoplot(ts_d_c3_treino) +
        autolayer(m_holt_d_c3, series = "Damped Holt") +
        autolayer(ts_d_c3_valida, series = "Validation")
      
      accuracy(m_holt_d_c3, ts_d_c3_valida)
      
    ### Análise dos resíduos dos 3 modelos demonstra indícios de 
    ### sazonalidade no cluster 1, cluster 2 está estacionado e cluster 3 apresenta
    ### sazonalidade e uma curva de tendência polinomial. 
      
      # Cluster 1:  
        checkresiduals(m_holt_d_c1)
        
      # Cluster 2:  
        checkresiduals(m_holt_d_c2)
        
      # Cluster 3:  
        checkresiduals(m_holt_d_c3)
        
# Diário: Auto-Regressivo com Sazonalidade -----

  # Pela verificação dos resíduos pude ver que os clusters 1 e 3 tem indicações
  # sazonalidade, porém difíceis de identificar. 
  # O cluster 2 já tem os resíduos próximos a ruído branco apenas com a modelagem 
  # de tendência Holt. Portanto não precisamos modelar a sazonalidade. 
  # Para os demais, usamos seasonal = TRUE para capturar a sazonalidade no modelo. 
        
        # Auto Arima --------------------------------------------------------------
        
        
        # Arima Cluster 1: -----
        
        m_arima_d_c1 <- auto.arima(ts_d_c1_treino, seasonal = TRUE, 
                                   stepwise = FALSE, approximation = FALSE,
                                   lambda = "auto")
        
        summary(m_arima_d_c1)
        checkresiduals(m_arima_d_c1)
        
        fc_arima_d_c1 <- forecast(m_arima_d_c1, h = 12)
        
        autoplot(ts_d_c1_treino) +
          autolayer(m_arima_d_c1$fitted) +
          autolayer(fc_arima_d_c1) +
          autolayer(ts_d_c1_valida)
        
        ## O forecast claramente falha em capturar a tendência de queda no final
        ## da série. Isso compromete a acurácia, aumentando o erro em 18x.
        ## A tendência apresenta um comportamento polinomial de grau 4, 
        ## mas mesmo uma modelagem com essa curva de tendência apresenta pouco
        ## ganho de performance na validação. 
        
        accuracy(fc_arima_d_c1, ts_d_c1_valida)
        
        
        # Arima Cluster 2: -----
        
        m_arima_d_c2 <- auto.arima(ts_d_c2_treino, seasonal = TRUE, 
                                   stepwise = FALSE, approximation = FALSE,
                                   lambda = "auto")
        
        summary(m_arima_d_c2)
        checkresiduals(m_arima_d_c2)
        
        fc_arima_d_c2 <- forecast(m_arima_d_c2, h = 12)
        
        autoplot(ts_d_c2_treino) +
          autolayer(m_arima_d_c2$fitted) +
          autolayer(fc_arima_d_c2) +
          autolayer(ts_d_c2_valida)
        
        ## Apesar de ser uma série com comportamente mais estável e simples que 
        ## o Cluster 1, o nível de erro na validação para o cluster 2 permanece
        ## muito alto e também falha em capturar a tendência de queda nos últimos 
        ## meses. 
        
        accuracy(fc_arima_d_c2, ts_d_c2_valida)
        
        
        # Arima Cluster 3: -----
        
        m_arima_d_c3 <- auto.arima(ts_d_c3_treino, seasonal = FALSE, 
                                   stepwise = FALSE, approximation = FALSE,
                                   lambda = "auto")
        
        summary(m_arima_d_c3)
        checkresiduals(m_arima_d_c3)
        
        fc_arima_d_c3 <- forecast(m_arima_d_c3, h = 12)
        
        autoplot(ts_d_c3_treino) +
          autolayer(m_arima_d_c3$fitted) +
          autolayer(fc_arima_d_c3) +
          autolayer(ts_d_c3_valida)
        
        ## Similar aos demais modelos, o erro na validação cresce muito e a 
        ## projeção falha em capturar a tendência de queda exponencial no final 
        ## do período. 
        
        accuracy(fc_arima_d_c3, ts_d_c3_valida)
        
#### Modelos diários: conclusões -----
        
  # De modo geral, os modelos diários tiveram uma performance muito ruim quando 
  # comparados ao modelo simples de tendência com o método de Holt. A modelagem 
  # Arima não ofereceu ganhos que justifiquem a complexidade do modelo. 
  # A opção paliativa para a modelagem é usar dados agregados por mês, que já 
  # eliminam o componente complexo de sazonalidade que é adicionado pelos dados 
  # diários.
  # Do ponto de vista da modelagem para esse tipo de previsão, a sazonalidade não 
  # é um componente relevante para o plano de médio prazo.

# GC
  rm(list = c(ls()[ls() %>% grep(pattern = "_d_")]))

# Modelos Mensais ---------------------------------------------------------

  # Damped Holt ----
      
    # Cluster 1 ----
      m_holt_m_c1 <- holt(ts_m_c1_treino,
                            h = 3,
                            phi = 0.9,
                            damped = TRUE,
                            seasonal = "multiplicative")
        
        autoplot(ts_m_c1_treino) +
          autolayer(m_holt_m_c1$fitted, series = "Damped Holt Fitted") +
          autolayer(m_holt_m_c1, series = "Damped Holt Projection") +
          autolayer(ts_m_c1_valida, series = "Validation")
        
      checkresiduals(m_holt_m_c1)
      
      ## Sem autocorrelação nos resíduos e erro satifatório na validação. 
      
      accuracy(m_holt_m_c1, ts_m_c1_valida)
        
        
    # Cluster 2 ----
      m_holt_m_c2 <- holt(ts_m_c2_treino,
                            h = 3,
                            phi = 0.9,
                            damped = TRUE,
                            seasonal = "multiplicative")
        
        autoplot(ts_m_c2_treino) +
          autolayer(m_holt_m_c2$fitted, series = "Damped Holt Fitted") +
          autolayer(m_holt_m_c2, series = "Damped Holt Projection") +
          autolayer(ts_m_c2_valida, series = "Validation")
        
        checkresiduals(m_holt_m_c2)
        
      ## Sem autocorrelação nos resíduos e erro satifatório na validação.   
      
        accuracy(m_holt_m_c2, ts_m_c2_valida)
        
    # Cluster 3 ----
      m_holt_m_c3 <- holt(ts_m_c3_treino,
                            h = 3,
                            phi = 0.9,
                            damped = TRUE,
                            seasonal = "multiplicative")
        
        autoplot(ts_m_c3_treino) +
          autolayer(m_holt_m_c3$fitted, series = "Damped Holt Fitted") +
          autolayer(m_holt_m_c3, series = "Damped Holt Projection") +
          autolayer(ts_m_c3_valida, series = "Validation")
        
      checkresiduals(m_holt_m_c3)
    
     ## Sem autocorrelação nos resíduos e erro satifatório na validação.   
     
      accuracy(m_holt_m_c3, ts_m_c3_valida)
      
# Auto Arima ----
      
  # Cluster 1 ----
      m_arima_m_c1 <- auto.arima(ts_m_c1_treino, seasonal = FALSE,
                                 stepwise = FALSE, approximation = FALSE)
      
      summary(m_arima_m_c1)
      fc_arima_m_c1 <- forecast(m_arima_m_c1, h = 3, level = 0.95)
      
      autoplot(ts_m_c1_treino) +
        autolayer(m_arima_m_c1$fitted, series = "Arima Fitted") +
        autolayer(fc_arima_m_c1, series = "Arima projection") +
        autolayer(ts_m_c1_valida, series = "Validation")
      
      accuracy(fc_arima_m_c1, ts_m_c1_valida)
      
      
  # Cluster 2 ----
      m_arima_m_c2 <- auto.arima(ts_m_c2_treino, seasonal = FALSE,
                                 stepwise = FALSE, approximation = FALSE)
      
      summary(m_arima_m_c2)
      fc_arima_m_c2 <- forecast(m_arima_m_c2, h = 3, level = 0.95)
      
      autoplot(ts_m_c2_treino) +
        autolayer(m_arima_m_c2$fitted, series = "Arima Fitted") +
        autolayer(fc_arima_m_c2, series = "Arima projection") +
        autolayer(ts_m_c2_valida, series = "Validation")
      
      accuracy(fc_arima_m_c2, ts_m_c2_valida)
      
      
  # Cluster 3 ----
      m_arima_m_c3 <- auto.arima(ts_m_c3_treino, seasonal = FALSE,
                                 stepwise = FALSE, approximation = FALSE)
      
      summary(m_arima_m_c3)
      fc_arima_m_c3 <- forecast(m_arima_m_c3, h = 3, level = 0.95)
      
      autoplot(ts_m_c3_treino) +
        autolayer(m_arima_m_c3$fitted, series = "Arima Fitted") +
        autolayer(fc_arima_m_c3, series = "Arima projection") +
        autolayer(ts_m_c3_valida, series = "Validation")
      
      accuracy(fc_arima_m_c3, ts_m_c3_valida)
        

# Modelos Mensais: conclusões ---------------------------------------------

  # Comparação de Acurácias ----
      # Cluster 1 ----
      print("Cluster 1: Holt")
      accuracy(m_holt_m_c1, ts_m_c1_valida) # Best
      print("Cluster 1: Arima")
      accuracy(fc_arima_m_c1, ts_m_c1_valida)
      
      # Cluster 2 ----
      print("Cluster 2: Holt")
      accuracy(m_holt_m_c2, ts_m_c2_valida) # Best
      print("Cluster 2: Arima")
      accuracy(fc_arima_m_c2, ts_m_c2_valida)
      
      # Cluster 3 ----
      print("Cluster 3: Holt")
      accuracy(m_holt_m_c2, ts_m_c2_valida) # Best
      print("Cluster 3: Arima")
      accuracy(fc_arima_m_c2, ts_m_c2_valida)

 # Em todos os casos, o modelo simples de tendência "atenuada" (damped) de Holt
 # teve um de validação mais baixo. O modelo de tendência será usado para a 
 # composição do modelo final, conforme abaixo.
 # 
 # Justificativas: 
 #    1.  Sazonalidade não é um componente relevante para as projeções.
 #    2.  O objetivo é identificar as tendências de crescimento de ordens para o
 #        cada cluster e verificar quais são os com melhor resultado. 
 #    3.  A modelo de Holt já captura adequadamente a tendência para as projeções. 

      

# Modelo Final: Mensal + Tendência Holt (damped) --------------------------

fc_final_c1 <- holt(ts_m_c1@orders,
                    h = 4,
                    phi = 0.9,
                    damped = TRUE,
                    seasonal = "multiplicative")
      
      
fc_final_c2 <- holt(ts_m_c2@orders,
                    h = 4,
                    phi = 0.9,
                    damped = TRUE,
                    seasonal = "multiplicative")


fc_final_c3 <- holt(ts_m_c3@orders,
                    h = 4,
                    phi = 0.9,
                    damped = TRUE,
                    seasonal = "multiplicative")


# Cluster 1 - Projeção ----
  autoplot(ts_m_c1@orders, series = "Valores atuais",
           main = "Cluster 1 - projeção de queda no número de ordens",
           ylab = "Ordens", xlab = "Período") + 
      autolayer(fc_final_c1,series = "Projeção 4 meses")

# Cluster 2 - Projeção ----
autoplot(ts_m_c2@orders, series = "Valores atuais",
         main = "Cluster 2 - projeção de queda no número de ordens",
         ylab = "Ordens", xlab = "Período") + 
  autolayer(fc_final_c2,series = "Projeção 4 meses")

# Cluster 3 - Projeção ----
autoplot(ts_m_c3@orders, series = "Valores atuais",
         main = "Cluster 3 - projeção de crescimento do número de ordens",
         ylab = "Ordens", xlab = "Período") + 
  autolayer(fc_final_c3,series = "Projeção 4 meses")
