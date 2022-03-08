# Base de Dados -----------------------------------------------------------
library(readr)
order_items <- read_csv2("./Data/df_proj_order_items_complete.csv",
                        col_names = TRUE)

products <- read_csv2("./Data/df_products_newids.csv",
                      col_names = TRUE)


# Carrega base (dadoss tratados EDA)
library(sqldf)
  df_base <- sqldf("
        SELECT 
          o.order_id, 
          o.order_item_id,
          o.product_id,
          o.seller_id, 
          p.category,
          o.price,
          o.days_approved,
          o.days_shipping,
          o.delayed_shipping,
          o.review_score,
          o.distance
        FROM order_items o
        LEFT JOIN products p
          ON o.product_id = p.product_id
          ")
  
  # Converte categorias "NA" para "nao_informada".
  df_base$category <- ifelse(is.na(df_base$category), 
                             "nao_informada", 
                             df_base$category)

# Calcula indicadores iniciais 
  df_sellers <-  sqldf("
           select distinct
            seller_id,
            category,
            count(distinct product_id) as cnt_products__numeric,
            count(distinct order_id) as cnt_orders__numeric,
            avg(price) as avg_price__numeric,
            min(price) as min_price__numeric, 
            max(price) as max_price__numeric,
            max(price)-min(price) as price_range__numeric,
            sum(price) as revenue__numeric,
            avg(days_shipping) as avg_shipping__numeric,
            sum(delayed_shipping) as n_delayed,
            avg(review_score) as avg_score__numeric,
            avg(distance) as avg_distance__numeric
          from df_base
            group by seller_id, category",
                       method = "name__class")


    # Reagrega as médias:
    df_seller_kpis <- sqldf("
                            select distinct 
                              seller_id, 
                              sum(cnt_orders) as n_orders__numeric,
                              sum(cnt_products) as n_products__numeric,
                              count(distinct category) as n_categories,
                              avg(avg_price) as avg_price__numeric,
                              sum(revenue) as revenue__numeric,
                              max(max_price)-min(min_price) as price_range__numeric,
                              avg(avg_shipping) as avg_shipping__numeric,
                              avg(avg_score) as avg_score__numeric,
                              avg(avg_distance) as avg_distance,
                              sum(n_delayed)/sum(cnt_orders) as pct_delayed
                            from df_sellers
                            group by seller_id",
                            method = "name__class")

    # Remove valores missing 
    df_seller_kpis <- df_seller_kpis[complete.cases(df_seller_kpis),]

    # No. de casos removidos:
        sum(!complete.cases(df_seller_kpis))

        

# Análises de Colinearidade -----------------------------------------------

# Analiza correlações entre todas as variáveis.
library(corrplot)
library(RColorBrewer)
library(dplyr)
df_seller_kpis[,-1] %>% filter(!is.na(avg_score)) %>% cor() %>% 
  corrplot(method = "circle", type = "upper", order = "hclust",
           col=brewer.pal(n=8, name="RdYlBu"))



# Clusterização -- tratamento preliminar ---------------------------------------------

# Normalização das distribuições:
  n_seller_kpis <-  df_seller_kpis[complete.cases(df_seller_kpis),] %>% 
    select(-seller_id,
           -revenue, 
           -n_orders, 
           -pct_delayed) %>% 
    scale()
# Matriz distancia:
  n_seller_kpis_dist <- dist(n_seller_kpis)



# Clusterização: hierárquico ----------------------------------------------

# Cria clusters por método Ward e centroid
    hc_ward_seller <- hclust(n_seller_kpis_dist, method = "ward.D2")
    hc_centroid_seller <- hclust(n_seller_kpis_dist, method = "centroid")

# Graficos iniciais:
  plot(hc_ward_seller, labels = F, main="Cluster method Ward")
    abline(h=45, col=2)

  plot(hc_centroid_seller, labels = F, main="Cluster method Centroid")
    abline(h=7, col=2 )

# Adiciona clusters hierarquicos a base principal
  df_seller_kpis$hc_ward <- as.factor(cutree(hc_ward_seller, k = 6))
  df_seller_kpis$hc_centroid <- as.factor(cutree(hc_centroid_seller, k = 3))


## Comparações gráfica por tipo de cluster:
  par(mfrow=c(1, 2))
   boxplot(df_seller_kpis$n_categories~df_seller_kpis$hc_ward,
           main = "Categories HC Ward", col=topo.colors(2))
   boxplot(df_seller_kpis$n_categories~df_seller_kpis$hc_centroid,
           main = "Categories HC Centroid", col=topo.colors(2))


  par(mfrow=c(1, 2))
    boxplot(df_seller_kpis$avg_distance~df_seller_kpis$hc_ward,
            main = "Distance HC Ward", col=topo.colors(2))
    boxplot(df_seller_kpis$avg_distance~df_seller_kpis$hc_centroid,
            main = "Distance HC Centroid", col=topo.colors(2))
            # Método por centróides gerou muitos outliers. 

# Compara Frequencias:
library(gmodels)
  gmodels::CrossTable(df_seller_kpis$hc_ward)
  gmodels::CrossTable(df_seller_kpis$hc_centroid)
      # Método por centróides piorou as dstribuições e aloca nos clusters. 



# Clusterização: método K-means -------------------------------------------

## CLUSTER KMEANS:
library(NbClust)
    # Calcula número ótimo de clusters.
    n_cluster_seller <- NbClust(n_seller_kpis, distance = "euclidean",
                            min.nc = 3, max.nc = 6,
                            method = "kmeans",
                            index = "all")
    # Cria clusters por método k-means. 
    kmn_sellers=kmeans(n_seller_kpis, 3, nstart=10, trace = TRUE)

    # Adicina clusters à base principal:
    df_seller_kpis$c_kmeans <- as.factor(kmn_sellers$cluster)
    
    # Tabela inútil de comparação:
    table(df_seller_kpis$hc_ward, 
          df_seller_kpis$c_kmeans)
    # Tabela útil para as distribuições:
    gmodels::CrossTable(df_seller_kpis$c_kmeans)



# Análise Gráfica - Clusters K-Means --------------------------------------


# Compara distribuições:
library(ggplot2)
library(patchwork)

  # Object GGplot gráfico base:
    p_base <- ggplot(filter(df_seller_kpis, 
                            avg_shipping >= 0)) + theme_get()
    p_empty <- ggplot() + theme_void()

    
# Categorias
p_categories <- p_base + geom_boxplot(aes(y=n_categories, group = c_kmeans, fill = c_kmeans),
                          outlier.shape = 4) + 
  labs(title = "Categorias", y = "categorias") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")

# Número de produtos:
p_products <- p_base + geom_boxplot(aes(y=n_products, 
                                        group = c_kmeans, 
                                        fill = c_kmeans),
                                    outlier.shape = 4) + 
  labs(title = "Variedade de Produtos", y = "# produtos") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")

# Preço médio
p_price <- p_base + geom_boxplot(aes(y=avg_price, 
                                     group = c_kmeans, 
                                     fill = c_kmeans),
                                 outlier.shape = 4) + 
  labs(title = "Preço médio", y = "preço") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Amplitude dos Preços
p_price_range <- p_base + geom_boxplot(aes(y=price_range, group = c_kmeans, fill = c_kmeans),
                                       outlier.shape = 4) + 
  labs(title = "Variação Preço", y = "var. preço") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Avaliação
p_score <- p_base + geom_boxplot(aes(y=avg_score, group = c_kmeans, fill = c_kmeans),
                                 outlier.shape = 4) + 
  labs(title = "Nota Avaliação", y = "nota") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Postagem
p_shipping <- p_base + geom_boxplot(aes(y=avg_shipping, group = c_kmeans, fill = c_kmeans),
                                   outlier.shape = 4) + 
  labs(title = "Postagem", y = "dias p/ postar") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Distância média:
p_distance <- p_base + geom_boxplot(aes(y=avg_distance, 
                                        group = c_kmeans, 
                                        fill = c_kmeans),
                                    outlier.shape = 4) + 
  labs(title = "Distâncias", y = "distância entrega") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Número de Ordens
p_orders <- p_base + geom_boxplot(aes(y=n_orders, 
                                      group = c_kmeans, 
                                      fill = c_kmeans),
                                  outlier.shape = 4) + 
  labs(title = "Volume de Vendas", y = "vendas") + 
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position =  "none")


# Análise Gráfica: comparativa --------------------------------------------

p_categories + p_products + p_price_range + p_price
p_score + p_shipping + p_distance + p_empty
p_orders


# Retrofit ----------------------------------------------------------------
#Adiciona clusters a base original
df_temp <- sqldf("
      SELECT
        o.*,
        p.category,
        k.c_kmeans as cluster__character
      FROM order_items o
      LEFT JOIN df_seller_kpis k
      ON o.seller_id = k.seller_id
      LEFT JOIN products p
      ON o.product_id = p.product_id
      ",
      method = "name__class")

# Salva nova base com cluster
write_csv2(df_temp, file = "./Data/df_proj_order_items_cluster.csv",
           append = FALSE)

## GC ##
rm(df_temp, df_sellers, df_base,
   hc_centroid_seller, hc_ward_seller,
   kmn_sellers,
   n_cluster_seller, 
   n_seller_kpis, n_seller_kpis_dist,
   p_base, p_categories, p_distance, 
   p_empty, p_orders, p_price, p_price_range,
   p_products, p_score, p_shipping)
