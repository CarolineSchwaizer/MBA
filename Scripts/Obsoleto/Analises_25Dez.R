
# Bibliotecas -------------------------------------------------------------

## Sempre carreguem todas as bibliotecas abauixo antes de começar. 
## Instalem antes de iniciar o que vocês ainda não tiverem. 

library(dplyr)
library(sqldf)
library(tidyverse)
library(readr)
library(lubridate)
library(forecast)
library(zoo)
library(ggplot2)
library(patchwork)



# Carregar bases ----------------------------------------------------------

## Coloquei aqui dois exemplos de como carregar os dados.
## As datas em formato Mês-Ano são um pouco chatas, por isso precisam de mais 
## informação para carregar corretamente. 

## Mantenham o padrão de taxonomia para que a gente não se confunda mais adiante:
## Dataframes = <df>_<id do cluster>_<periodicidade>
## TimeSeries = <ts>_<id do cluster>_<variável>_<periodicidade>

df_c1_daily <- read_csv2("Data/Cluster1_daily.csv",
                         col_types = cols(...1 = col_skip(),
                                          dates = col_date(format = "%Y-%m-%d"),
                                          orders = col_number(),
                                          avg_price = col_number(),
                                          revenue = col_number()),
                         locale = locale(date_names = "pt"),
                         trim_ws = TRUE)


df_c1_monthly <- read_csv2("Data/Cluster1_monthly.csv", 
                           col_types = cols(...1 = col_skip(),
                                             dates = col_date(format = "%b %Y"), 
                                             orders = col_number(), 
                                             avg_price = col_number(), 
                                             revenue = col_number()),
                            locale = locale(date_names = "pt"), 
                            trim_ws = TRUE) %>% 
                  mutate(dates = as.yearmon(dates))
  


# Criar Timeseries --------------------------------------------------------

## Cada dataframe tem 3 variáveis. Para facilitar a vida de vocês, carreguem 
## uma variável por time series. Trabalhar com objetos 'ts' com vários vetores 
## pode ser um pouco chato de codificar. 

## Prestem atenção às frequências sempre que forem carregar. Frequências erradas
## no comando causam agregações incorretas nos dados da série. 

## Exemplo da carga diária:

ts_c1_orders_daily <- ts(df_c1_daily$orders, 
                         start = c(2017, 01, 05), # Data inicial é sempre a 
                                                 # mesma para as séries diárias
                         frequency = 365 # Sempre usem 365 para as 'daily'
                                         # O 'end' (período final) é calculado
                                         # pela data inicial + frequência. 
                        )

## Se a 'ts' diária foi criada corretamente, a data final (maior valor) será
## igual a 2018-08-29 para todas as variáveis e todos os dataframes. 
## Todas as sérias tem exatamente 602 dias de observações. 

## Exemplo de carga mensal:

ts_c1_avg_price_monthly <- ts(df_c1_monthly$avg_price,
                              start = c(2017, 01), # 'dia' aqui é irrelevante.
                              frequency = 12 # Gera observações por mês.
                              )

## As 'ts' mensais tem todas 20 observações, iniciando em Jan 2017 e terminando
## em Ago 2018. 


# Notas sobre Forecasts ---------------------------------------------------

## Alguns testes se dão melhor com séries menores, outros com séries maiores. 
## SAZONALIDADE será um problema para analizar, pois tanto as 'ts' diárias
## como as mensais não possuem um ciclo completo de 2 anos. Então são mais 
## difíceis de analisar e modelar. Não se procupem com isso agora. 

## Alguns testes vão produzir erros ou avisos sobre o ajuste da série sobre o
## teste a ser aplicado. Tirando o da sazonalidade "('time series has no or less
## than 2 periods)", se encontrarem erros, anotem e pesquisem o que foi sugerido. 

# Modelos que precisamos testar:
#   1. Regressões Exponenciais
#   2. Regressões Polinomiais (Grau 3 e 4)
#   3. Modelo de Média Móvel (MA)
#   4. Modelo de Suavização. 
#   
# Como separar as amostras:
#   Mensais: 16 meses treinamento, 4 meses de validação.
#   Diárias: 512 dias treinamento, 90 de validação.
#   
# O que testar:
#   a. Rodar modelos e anotar AIC, R2, etc. 
#   b. Rodar o forecast e anotar o MAPE.
#   c. Verificar com gráfico simples/tosco (ajeitamos só os do modelo final)
#   



                           