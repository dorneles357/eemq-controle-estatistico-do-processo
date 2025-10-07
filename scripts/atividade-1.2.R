
if (! require("pacman")) install.packages("pacman")
pacman::p_load(readr, ggplot2)

setwd(getwd())

atividade_1 <- readr::read_csv("data/atividade-1.2.csv")

dplyr::glimpse(atividade_1)

# x-barra e amplitude de cada subgrupo ------------------------------------

R <- apply(atividade_1, 1, function(x) max(x)-min(x))

# x-barra e amplitude das medias e amplitudes -----------------------------

Rbar <- mean(R)

# constantes --------------------------------------------------------------

D3 <- 0
D4 <- 2.282

# limites -----------------------------------------------------------------

LSC_R <- D4 * Rbar
LIC_R <- D3 * Rbar

# plots -------------------------------------------------------------------

df_grafico <- data.frame(
  Subgrupo = seq_along(length(R)),
  Media = R,
  Linha_Media = Rbar,
  LSC = LSC_R,
  LIC = LIC_R
)

ggplot(df_grafico, aes(x = Subgrupo, y = Media)) +
  geom_line(color = "#2E86AB", linewidth = 0.8) +
  geom_point(color = "#2E86AB", size = 2.5) +
  geom_hline(
    aes(yintercept = Linha_Media),
    color = "#1B4332",
    linewidth = 1,
    linetype = "solid"
  ) +
  geom_hline(
    aes(yintercept = LSC),
    color = "#E63946",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_hline(
    aes(yintercept = LIC),
    color = "#E63946",
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_label(
    aes(
      x = max(Subgrupo),
      y = LSC,
      label = paste("LSC =", round(LSC, 3))
    ),
    hjust = 1.1,
    vjust = 0.5,
    fill = "#E63946",
    color = "white"
  ) +
  geom_label(
    aes(
      x = max(Subgrupo),
      y = LIC,
      label = paste("LIC =", round(LIC, 3))
    ),
    hjust = 1.1,
    vjust = 0.5,
    fill = "#E63946",
    color = "white"
  ) +
  geom_label(
    aes(
      x = max(Subgrupo),
      y = Rbar,
      label = paste("Linha Central =", round(Rbar, 3))
    ),
    hjust = 1.1,
    vjust = 0.5,
    fill = "#1B4332",
    color = "white"
  ) +
  labs(
    title = "Gráfico de Controle R",
    subtitle = "Controle Estatístico do Processo",
    x = "Número do Subgrupo",
    y = "Média do Subgrupo",
    caption = paste("Tamanho do subgrupo:", ncol(atividade_1))) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray50"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray30", fill = NA, linewidth = 0.5)
  )