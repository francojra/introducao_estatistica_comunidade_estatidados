
# Introdução à Estatística -----------------------------------------------------------------------------------------------------------------

# Comunidade de Estatística Estatidados ----------------------------------------------------------------------------------------------------

#  Prof. Thiago Marques --------------------------------------------------------------------------------------------------------------------

vetor_pacotes = c(
                "devtools",
                "rmdformats",
                "reactable",
                "Hmisc",
                "dplyr",
                "kableExtra",
                "emo",
                "vembedr"
                )
#install.packages(vetor_pacotes)

lapply(vetor_pacotes, 
       require, 
       character.only = TRUE)

#install.packages("emo")  # Caso seja necessário
#install.packages("vembedr")
#remotes::install_github("juba/rmdformats",force = TRUE)
#remotes::install_github("glin/reactable",force = TRUE)


# Criando vetores --------------------------------------------------------------------------------------------------------------------------

idade2 = c(rep(10, 4), rep(30, 8), rep(50, 4), rep(70, 3), 90)

idade2

# Achando as Frequências simples (fi) ------------------------------------------------------------------------------------------------------

frequencia_simples = table(idade2)

frequencia_simples

# Colocando no Visual excel ----------------------------------------------------------------------------------------------------------------

dados_simples = data.frame(frequencia_simples)

dados_simples

# Ver o banco todo (Não recomendável para bancos gigantescos) ------------------------------------------------------------------------------

View(dados_simples)

# Ver as n primeiras linhas do data.frame --------------------------------------------------------------------------------------------------

head(dados_simples, 2)

# Frequência acumulada ---------------------------------------------------------------------------------------------------------------------

frequencia_acumulada = cumsum(frequencia_simples)

frequencia_acumulada

# Adicionando coluna com acumulada ---------------------------------------------------------------------------------------------------------

dados_simples$frequencia_acumulada = frequencia_acumulada

View(dados_simples)

# Frequência relativa simples --------------------------------------------------------------------------------------------------------------

frequencia_relativa_simples = frequencia_simples / sum(frequencia_simples)

frequencia_relativa_simples

# segunda forma para obter frequencias relativa simples utilizando a função prop.table() ---------------------------------------------------

prop.table(frequencia_simples)

# Adicionando a Frequência relativa simples a tabela ------------------------------------------------------------------------------------------------

dados_simples$frequencia_relativa_simples = frequencia_relativa_simples

# Frequência relativa acumulada ------------------------------------------------------------------------------------------------------------

frequencia_relativa_acumulada = frequencia_acumulada / sum(frequencia_simples)

# Frequência relativa acumulada (Segunda forma) ---------------------------------------------------------------------------------------------

frequencia_relativa_acumulada = cumsum(frequencia_relativa_simples)

# Criando a variavel frequencia_relativa_acumulada -----------------------------------------------------------------------------------------

dados_simples$frequencia_relativa_acumulada = frequencia_relativa_acumulada

View(dados_simples)

# Renomeando uma coluna do dataframe dados_simples -----------------------------------------------------------------------------------------

names(dados_simples)[5] = "Frequencia Relativa Acumulada"
tibble::tibble(dados_simples)

# Colocando na ótica tidyverse -------------------------------------------------------------------------------------------------------------

dados_simples_tidy = dados_simples %>%
  mutate(
    frequencia_acumulada = cumsum(Freq),
    frequencia_relativa_simples = Freq / sum(Freq),
    frequencia_relativa_acumulada = cumsum(frequencia_relativa_simples)
  )

tibble(dados_simples_tidy)

# Ordenando de forma decrescente pela idade utilizando a função arrange() ------------------------------------------------------------------

dados_simples_tidy_ordenado = dados_simples_tidy %>% arrange(desc(Freq))

dados_simples_tidy_ordenado

# Exportando como banco de dados csv -------------------------------------------------------------------------------------------------------

write.csv2(dados_simples_tidy, "coco1.csv", row.names = F) # Salva nova tabela

# Evitando notação científica --------------------------------------------------------------------------------------------------------------

options(scipen=999)

# Instalando e carregando Biblioteca que calcula Moda, Assimetria e Curtose no R -----------------------------------------------------------

#install.packages("e1071")
require(e1071)
#install.packages("DescTools")
library(DescTools)
library(Hmisc)

# Amostra Empresa A ------------------------------------------------------------------------------------------------------------------------

a = c(2000, 3000, 4000, 9000)
desvio1 = 2000 - 4500
desvio2 = 3000 - 4500
desvio3 = 4000 - 4500
desvio4 = 9000 - 4500
soma_desvios_media = sum(desvio1, desvio2, desvio3, desvio4)
soma_desvios_media

# Média, mediana, moda e Resumos empresa A -------------------------------------------------------------------------------------------------

media_a = mean(a)

mediana_a = median(a)

moda_a = DescTools::Mode(a)

cbind(media_a,mediana_a,moda_a)

# Resumos gerais empresa A -----------------------------------------------------------------------------------------------------------------

summary(a)

Hmisc::describe(a)

# Comparando Graficamente ------------------------------------------------------------------------------------------------------------------

# Divisão de janelas
par(mfrow = c(1, 2))

# Histograma com as frequências simples
hist(a)

# Histograma com as densidades
hist(a, probability = T)

# Curva de densidade
lines(density(a))

# Assimetria e Curtose ---------------------------------------------------------------------------------------------------------------------

# Coeficiente de assimetria de pearson
assimetria_a = skewness(a)

# Coeficiente de curtose de pearson
curtose_a=kurtosis(a)

cbind(assimetria_a,curtose_a)

# Consolidando as Estatísticas resumo ------------------------------------------------------------------------------------------------------

dados_a_consolidados = data.frame(
  "moda" = moda_a,
  "mediana" = mediana_a,
  "media" = media_a,
  "assimetria" = assimetria_a,
  "curtose" = curtose_a
)

dados_a_consolidados 

# Amostra Empresa B ------------------------------------------------------------------------------------------------------------------------

b = c(2000,3000,3000,3000,3000,3000,4000,7000,9000)

# Média, mediana, moda e Resumos empresa B -------------------------------------------------------------------------------------------------

media_b = mean(b)

mediana_b = median(b)

moda_b = DescTools::Mode(b)

cbind(media_b,mediana_b,moda_b)

# Resumos gerais empresa B -----------------------------------------------------------------------------------------------------------------

summary(b)

Hmisc::describe(b)

# Comparando Graficamente ------------------------------------------------------------------------------------------------------------------

# Divisão de janelas
par(mfrow = c(1, 2))

# Histograma com as frequências simples
hist(b)

# Histograma com as densidades
hist(b, probability = T)

#Curva de densidade
lines(density(b))

# Assimetria e Curtose ---------------------------------------------------------------------------------------------------------------------

# Coeficiente de assimetria de pearson
assimetria_b = skewness(b)

# Coeficiente de curtose de pearson
curtose_b = kurtosis(b)

cbind(assimetria_b,curtose_b)

# Consolidando as Estatísticas resumo ------------------------------------------------------------------------------------------------------

dados_b_consolidados = data.frame(
  "moda" = moda_b,
  "mediana" = mediana_b,
  "media" = media_b,
  "assimetria" = assimetria_b,
  "curtose" = curtose_b
)

dados_b_consolidados

# Amostra Empresa C ------------------------------------------------------------------------------------------------------------------------

c = c(2000,3000,4000,6000, 7000, 7000, 8000,  9000)

# Média, mediana, moda e Resumos empresa C -------------------------------------------------------------------------------------------------

media_c = mean(c)

mediana_c = median(c)

moda_c = DescTools::Mode(c)

cbind(media_c,mediana_c,moda_c)

# Resumos gerais empresa C -----------------------------------------------------------------------------------------------------------------

summary(c)

Hmisc::describe(c)

# Comparando Graficamente ------------------------------------------------------------------------------------------------------------------

# Divisão de janelas
par(mfrow = c(1, 2))

# Histograma com as frequências simples
hist(c)

# Histograma com as densidades
hist(c, probability = T)

# Curva de densidade
lines(density(c))

# Assimetria e Curtose ---------------------------------------------------------------------------------------------------------------------

#Coeficiente de assimetria de pearson
assimetria_c = skewness(c)

#Coeficiente de curtose de pearson
curtose_c = kurtosis(c)

cbind(assimetria_c,curtose_c)

# Consolidando as Estatísticas resumo ------------------------------------------------------------------------------------------------------

dados_c_consolidados = data.frame(
  "moda" = moda_c,
  "mediana" = mediana_c,
  "media" = media_c,
  "assimetria" = assimetria_c,
  "curtose" = curtose_c
)

dados_c_consolidados

# Medidas de tendência central, assimetria e curtose (Ótica tidyverse) ---------------------------------------------------------------------

#ÓTICA TIDYVERSE
#install.packages("dplyr")
library(dplyr)

# Dados empresa A (Ótica tidyverse) --------------------------------------------------------------------------------------------------------

# Empresa A

a.df = data.frame(a)

a.df_resumo = a.df %>% summarise(
  moda_a = Mode(a),
  mediana_a = median(a),
  media_a = mean(a),
  assimetria_a = skewness(a),
  curtose_a = kurtosis(a)
)

# Empresa B

b.df = data.frame(b)

b.df_resumo = b.df %>% summarise(
  moda_b = Mode(b),
  mediana_b = median(b),
  media_b = mean(b),
  assimetria_b = skewness(b),
  curtose_b = kurtosis(b)
)

# Empresa C

c.df = data.frame(c)

c.df_resumo = c.df %>% summarise(
  moda_c = Mode(c),
  mediana_c = median(c),
  media_c = mean(c),
  assimetria_c = skewness(c),
  curtose_c = kurtosis(c)
)

# Concatenando -----------------------------------------------------------------------------------------------------------------------------

# Transpor para concatenar colunas
consolidado_empresas = data.frame(t(a.df_resumo),t(b.df_resumo),t(c.df_resumo))
consolidado_empresas

# Mudando nomes de linhas e colunas
colnames(consolidado_empresas) = c("resumo_empresa_a","resumo_empresa_b","resumo_empresa_c")
rownames(consolidado_empresas) = c("Moda","Mediana","Media","Assimetria","Curtose")
consolidado_empresas
