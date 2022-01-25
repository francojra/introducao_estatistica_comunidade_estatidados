
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

install.packages("emo")  # Caso seja necessário
install.packages("vembedr")
remotes::install_github("juba/rmdformats",force = TRUE)
remotes::install_github("glin/reactable",force = TRUE)


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


