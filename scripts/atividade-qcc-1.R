
if (! require("pacman")) install.packages("pacman")
pacman::p_load(readr, qcc)

setwd(getwd())

atividade_1 <- readr::read_csv("data/atividade-1.1.csv")

dplyr::glimpse(atividade_1)

dados_matrix <- as.matrix(atividade_1)

# Gráfico X-bar automático
qcc(dados_matrix, type = "xbar",
    title = "Gráfico de Controle X-barra",
    xlab = "Subgrupo", ylab = "Média")

# Gráfico R automático
qcc(dados_matrix, type = "R",
    title = "Gráfico de Controle R",
    xlab = "Subgrupo", ylab = "Amplitude")