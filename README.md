# MBA
Projeto Final MBA em Business Analytics e Big Data (FGV)

LOGÍSTICA DE ENTREGAS NO E-COMMERCE

## Contextualização
EMPRESA OLIST
Olist é uma plataforma que conecta a loja virtual de seus clientes ao seu público alvo na internet, funcionando como uma conexão entre lojistas e consumidores através de marketplaces. 
A empresa de tecnologia foi fundada em 2015 e atua no formato SaaS (Software as a Service), oferecendo suporte em gestão, logística e atendimento ao consumidor final, com atuação em mais de 180 países.
De forma geral, apresenta 3 principais frentes de atuação:
SOFTWARE: gerenciamento de pedidos centralizado, dados para envio, geração de etiquetas personalizadas, entre outros serviços;
CONTRATOS EXCLUSIVOS: contratos já firmados com os principais marketplaces do Brasil e com os Correios;
COMPARTILHAMENTO DE REPUTAÇÃO: vendas realizadas no geram uma reputação que é compartilhada entre todos os lojistas participantes do olist.

## Problema de Pesquisa
Pensando em uma estratégia para expansão dos negócios do Olist, é possível determinar perfis de fornecedores, agrupá-los conforme suas características e prever as receitas de cada grupo?

## Objetivos
Objetivo Principal:
Gerar um forecast de receita dos fornecedores baseado no perfil  para angariamento de novos fornecedores

Objetivos Específicos:
Realizar uma análise de cluster de modo a agrupar fornecedores com perfis parecidos
Realizar um forecast dos fornecedores por cluster, de modo a obter dados para angariar mais clientes

## Base de Dados Inicial
A base de dados está inicialmente estruturada em 8 datasets, relacionados da seguinte forma:

![image](https://user-images.githubusercontent.com/88386282/157254574-0b42b1b0-b90f-4ff1-8f0f-3d7d832dc658.png)

olist_order_customer_dataset: informações de clientes e respectivas localizações
olist_geolocation_dataset: latitude e longitude de cada CEP
olist_order_items_dataset: ordem, produto e vendedor de cada item, juntamente com informações da logística de entrega
order_payments_dataset: informações do pagamento de cada ordem
olist_order_reviews_dataset: nota de avaliação das ordens e comentários
olist_orders_dataset: informações das ordens, clientes e datas
olist_products_dataset: informações dos produtos e categorias
olist_sellers_dataset: informações de vendedores e respectivas localizações

## Feature Engineering
Especificamente para a análise de clusters, uma série de variáveis adicionais foi construída. Essas variáveis procuram traduzir o processo de logística em indicadores que mensurem os tempos, atrasos e distâncias das entregas das ordens de compra. 

As variáveis criadas foram:
Temporais
Estimativa de entrega (dias) = Data de compra – Data estimada da entrega.
Dias até a entrega (dias) = Data aprovação da compra – Data de entrega da compra.
Dias para aprovar a compra (dias) = Data da compra – Data da aprovação.
Lógicas
Atraso na postagem = Data limite para postagem < Data da postagem; TRUE
Atraso na entrega = Estimativa de entrega < Dias até entrega; TRUE
Espaciais
Distância para entrega = distância geodésica (Geolocalização revendedor; Geolocalização comprador)

## Análise Exploratória
O principal objetivo da análise exploratória de dados foi identificar relações causais entre os indicadores pertinentes ao processamento da compra e a avaliação atribuída pele cliente ao final do processo de compra.

Granularidade dos dados:
O universo da análise é constituído por aproximadamente 100 mil ordens
de compra. Cada compra corresponde a apenas 1 cliente, comprando de uma variedade de 3 mil fornecedores distintos. A gama de produtos negociados é igualmente ampla, com mais de 32 mil códigos únicos para produtos dentro de 74 categorias.
Quando se comparam a contagem de cidades distintas para clientes e fornecedores (customer_city e seller_city) observa-se claramente uma concentração muito maior na distribuição geográfica dos fornecedores.

![image](https://user-images.githubusercontent.com/88386282/157255217-249f27a6-fa6a-4e80-98dd-b449713ab2d6.png)

Variáveis temporais:
O modelo conta com algumas variáveis tipo timestamp que permitem calcular tempos de entrega, processamento e postagem dos produtos. Elas ajudam a entender o período e possíveis sazonalidades nas ordens de compra e descrevem o período da análise: entre 2016-09-04 21:15:19 e 2018-10-17 17:30:18.

Variáveis escalares:
As duas principais variáveis escalares do modelo correspondem aos preços da ordem e custo do frete: price e freight_value. Para essas variáveis, as distribuições mostram claros outliers nas transações. 

![image](https://user-images.githubusercontent.com/88386282/157255266-ad3349a0-fa49-49df-a95a-7bb0e71410fe.png)

Valores faltantes:
Os valores vazios ou incorretos na base se concentram nas variáveis do tipo timestamp e nas variáveis que armazenam os textos de avaliações de clientes. 
Quando comparados os números de casos completos entre orders, order_items e order_reviews, pode-se observar um quadro bem distinto na base de avaliações. Apenas 10% da base de avaliações possuí casos ocorrências completas.
Apesar de apenas 10% das 99 mil ocorrências serem casos completos
para as order_reviews, as variáveis que são relevantes para a análise mostram uma distribuição gerenciável de missing values. A variável score (nota da avaliação) não possui valores em falta. 

![image](https://user-images.githubusercontent.com/88386282/157255366-0b5cdbf1-8842-4b2c-8374-61d3ac9830d0.png)

Escopo:
As transações que representam as ordens de compra possuem 7 status diferentes para as etapas do processamento. Entretanto, as ordens entregues representam 97% da base. Como o recorte analítico pretende avaliar o processo da ordem inteiramente, optou-se pela primeira redução de escopo considerando apenas as ordens com status igual a delivered. 
Em paralelo, observa-se o comportamento do volume de vendas entre os períodos mais antigos e mais recentes do modelo. Nos períodos anteriores a janeiro de 2017, o volume de transações não é significante, estabelecendo outro critério para redução de escopo. Cabe também uma análise de outliers para os volumes de venda no período de 2017-11-24, no qual o volume é cerca de 7 vezes o valor médio da série temporal.

![image](https://user-images.githubusercontent.com/88386282/157255433-94aa2b69-d0c1-4c68-a23a-ae6ed050d571.png)
![image](https://user-images.githubusercontent.com/88386282/157255447-d9965a27-b094-4cdd-b314-ac959d38aa78.png)

Análise das distribuições:
  Evolução das Vendas por Região:
A distribuição do volume de vendas por região foi medida através da soma	dos valores das ordens, agregados pelos estados e regiões dos clientes. A evolução apresenta uma tendência de crescimento dos volumes durante o período, excetuando Norte e Centro Oeste que parecem estar estacionadas.
Naturalmente, o Sudeste se destaca das demais regiões, sendo simultaneamente uma região de grande densidade e onde a maioria dos clientes e fornecedores se concentram, como será revisado adiante.

![image](https://user-images.githubusercontent.com/88386282/157255575-09ba0a98-5a2f-42a3-a801-f6b759e17347.png)

  Distribuição das Novas Variáveis:
A análise das distribuições das novas variáveis apontou problemas que não haviam sido detectados inicialmente no tratamento da base, por exemplo:
Datas de aprovação de Compras que contam como entregues.
Datas de aprovação que são posteriores às datas de envio ou de entrega, o que contraria o processo da venda.
Datas de aprovação que são erros da base, como compras aprovadas após o envio dos produtos.

![image](https://user-images.githubusercontent.com/88386282/157255662-a5527901-7e65-4513-bbda-22700d37c998.png)

A amplitude das três escalas implica em valores, mesmo nos níveis mais extremos, como o caso de ordens que foram despachadas (shipped) mais de cem (100) dias antes do previsto. A contagem dos valores, entretanto, não é significativa para mostrar alguma área no mapa de distribuições. Também foram detectados outliers nos cruzamentos básicos de valores: preços e fretes.
Procurou-se identificar se fatores como tipo do produto ou distância para as entregas pesavam no desvio.

![image](https://user-images.githubusercontent.com/88386282/157255728-e73e8fc4-808d-4d40-ac20-7de4a14ee2b6.png)

  Variáveis Lógicas:
As variáveis lógicas apresentaram um comportamento mais equilibrado e interessante. 
O cruzamento das notas da avaliação com o status de atraso das ordens mostra uma distribuição muito regular para os pedidos com atraso. Uma progressão inversa das distribuições de notas para pedidos sem atraso, mas uma concentração em notas baixas para pedidos com atraso.

![image](https://user-images.githubusercontent.com/88386282/157255797-e771869e-0b25-4bc2-8f13-4365d4fd5ca7.png)

Distribuição geográfica:
O número de clientes e fornecedores apresenta distribuições muito concentradas em algumas regiões. A presença de fornecedores nas regiões Norte e Centro Oeste é pouco significante. Já a distribuição dos clientes é geograficamente mais espalhada, mas não de forma a eliminar as grandes concentrações em regiões como Sudeste e Sul. 

![image](https://user-images.githubusercontent.com/88386282/157255917-2b2fe87b-5a73-4f30-9686-87a2bb955f8a.png)

As distâncias percorridas e o tempo apresentam uma grande variação conforme as distâncias das entregas aumentam. 
Essa flutuação ocorre apenas nos casos com atrasos nas entregas. As entregas sem atraso possuem uma relação linear regular entre a distância e o tempo para entregar a ordem.

![image](https://user-images.githubusercontent.com/88386282/157255968-35a63435-c58a-4f98-8bc9-5df2542ee9f4.png)

## Técnicas de Machine Learning
Clusterização:
K-means – Melhores Resultados
Ward.d2
Centroide 

Previsão de Séries Temporais:
Suavização exponencial: Modelo de tendência linear de Holt’s (damped)
Auto Regressivo com Sazonalidade

## Metodologia
Visão geral:
A pesquisa conduzida é essencialmente exploratória. Mesmo com o uso de ferramentas de projeção e técnicas não supervisionadas não foram testadas hipóteses direta, ou indiretamente. O foco da pesquisa foi a compreensão das características dos revendedores da Olist e seu possível impacto nas vendas e receita da empresa. 
O método utilizado é comparativo, pois buscou-se compreender as diferenças internas na classificação dos revendedores, sem se preocupar com generalizações ou validar se critérios como qualidade, eficiência, preço tem impacto na receita. 

Fonte de dados:
Primária: base de dados proprietária da Olist, com detalhamento das ordens de compra.
Produtos, Categorias, Preços e Fretes.
Processamento das ordens: datas de compra, aprovação, estimativas, entregas, etc. 
Dados anonimizados de Fornecedores e Compradores. 
Secundária: base pública de geolocalização baseada em CEP. 
Região, sub-região, setor, sub-setor, divisor de sub-setor dos CEPs.
Informações aproximadas de latitude e longitude.

Técnicas aplicadas:
Feature Engineering
Identificação de Ouliers.
Criação de novas variáveis a partir das existentes.

Redução de dimensionalidade
Classificação de fornecedores usando métodos cluster.
Agrupamentos criados conforme critérios de variedade de produtos e categorias, nível e variação de preços, distâncias percorridas para entrega.
Informações como receita e número de ordens são removidas para não influenciar a classificação.

Previsões baseadas em séries temporais (forecast)
Utilizando dados mensais agregados.
Separando previsões para cada grupo identificado.

## Resultados Encontrados

Clusterização:
A distribuição ideal foi de 3 clusteres, pelo método K-means:
Cluster 1: Produtos baratos, mais focados em artigos para casa. Fornecedores de maior porte, com distâncias curtas indicando que são das regiões de maior concentração de clientes/fornecedores, como o Sudeste.​ 289 fornecedores (9,8%)
Cluster 2: Produtos caros de tecnologia e esportes, de fornecedores ineficientes no processamento das compras. ​359 fornecedores (12,2%)​
Cluster 3: Produtos baratos em artigos pessoais, de fornecedores mais ágeis, de regiões próximas as de maior densidade. ​2293 fornecedores (78%)​
Em um primeiro momento, com base na análise de clusters, seria melhor optar por fornecedores que apresentam maior representatividade no cluster 1, o qual possui melhores resultados e um volume bastante significativo de ordens.

Forecasts:
As previsões para as séries temporais se concentraram apenas no número de Ordens. 
A previsão de receita e o número de ordens possuem muita colinearidade, portanto pode-se deduzir uma pela outra. 
Preço médio é um variável “estacionada” e sem diferenças significativas entre os grupos.
Dados das ordens foram agregados por mês, eliminado a granularidade diária (irrelevante para as previsões). 
As séries não apresentam componentes de sazonalidade, apenas tendência. 
Tendência inicial é similar para os 3 Clusters e correspondem a um período do início das operações do Olist.
Neste contexto, o método de Forecast com o melhor resultado foi a Suavização Exponencial com tendência de Holt atenuada (damped). 

Forecasts - Cluster 1
Apesar da constatação positiva inicial de quê o cluster 1 concentra fornecedores de maior porte nas regiões centrais (sudeste) e tem boas avaliações, as previsões de crescimento não foram otimistas. 
Os quatro meses futuros mostram uma alta probabilidade de queda no volume de ordens para os fornecedores nesse perfil. 

![image](https://user-images.githubusercontent.com/88386282/157256618-2a7ce3cc-fcf0-4c66-8054-8d2bcddc0949.png)

Forecasts - Cluster 2
De modo similar ao cluster 1, o segundo agrupamento de fornecedores também não apresenta previsões de crescimento para os próximos 4 períodos.
O cluster 2 poderia ter sido considerado o grupo “problemático”, devido à combinação de altos preços e fornecedores ineficientes no gerenciamento das ordens. Portanto, não é uma surpresa a probabilidade de queda. 

![image](https://user-images.githubusercontent.com/88386282/157256696-faaeb35e-a13f-44b8-b55e-977793eecc30.png)

Forecasts - Cluster 3
O cluster 3, que até Agosto de 2018 representava 78% do volume de vendas da Olist foi o que apresentou resultados favoráveis para o crescimento no curto prazo. 
A previsão mostra a manutenção da tendência de crescimento, mas menos acelerada e com baixa probabilidade de queda. 

![image](https://user-images.githubusercontent.com/88386282/157256750-625e8b00-2ad4-4aa8-97fd-d60f0aca5c16.png)

## Implicações Gerenciais
Expansão dos negócios:
A combinação de classificação e forecasts auxilia a identificar perfis de fornecedores e as ações necessárias para otimizar ou manter os volumes de venda e receita do negócio. 
A classificação também auxilia no processo de expansão, ajudando na seleção de novos fornecedores com maior probabilidade de aceitação pelos clientes do E-Commerce da Olist. 

Manutenção e Melhoria dos serviços:
A identificação dos perfis também pode auxiliar na tomada de ações “corretivas” sobre os grupos de fornecedores que apresentam prognósticos ruins de venda. 
Pelos perfis, pode-se conceber ferramentas similares as de análise competitiva, onde pontos específicos como a eficiência não processamento das compras podem ser oferecidos aos fornecedores para estes melhorarem seus serviços.

## Limitações
A precisão de utilidade dos mecanismos de classificação e forecasts para a base disponibilizada pela Olist oferece limitações de cunho técnico e gerenciais, para a abordagem almejada por essa análise. Essas limitações podem ser divididas em 3 áreas:
Temporalidade:
Os dados contemplam apenas 20 meses.
Algoritmos de séries temporais necessitam ao menos 2 ciclos completos de períodos. 
Períodos mais longos permitem inferir ou explorar sazonalidades e tendências. 

Detalhamento:
Ausência de nomes de produtos dificulta agrupamentos e interpretação dos dados. 
Ausência de informações sobre estrutura de fornecedores não permite distinção entre grandes, médias e pequenas empresas. 

Contexto:
Dados contemplam o período inicial de investimento e de crescimento exponencial (2016-2018). 
Contexto dificulta a identificação da tendência de crescimento orgânico da empresa.

## Projetos Futuros
Ampliar períodos da análise, de modo a termos um período de treinamento maior.
Cortar períodos iniciais com crescimento artificial, de modo a termos uma amostra de treinamento mais condizente com a realidade.
Agregar informações adicionais sobre fornecedores e os produtos, de modo a conseguirmos identificar melhor a natureza dos pedidos.

## Conclusões
Os dados e a análise permitem classificar e realizar prognósticos de vendas futuras, a tomada de decisões para aumento das vendas, além de possibilitar o processo de expansão e angariação de novos clientes.
Devidos as complicações apontadas, a confiabilidade das projeções é baixo, é possível dizer a tendência, mas resultados precisos são difíceis de obter.
