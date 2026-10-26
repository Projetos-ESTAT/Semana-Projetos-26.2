source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

# 0. Banco de dados ----
# Base ChickWeight (nativa do R): peso (em gramas) de 50 pintinhos medido
# do dia 0 ao dia 21 do experimento, sob 4 dietas diferentes.
dados <- ChickWeight %>%
  as_tibble() %>%
  mutate(Diet = factor(Diet, labels = str_c("Dieta ", 1:4)))

# 1. Distribuição do peso dos pintinhos ----
# 1.1 Histograma ----
hist_peso <- ggplot(dados) +
  aes(x = weight) +
  geom_histogram(fill = "steelblue") +
  labs(title = "Histograma do peso", x = "weight", y = "count")
hist_peso
ggsave("resultados/images/hist_peso_errado.png", hist_peso, width = 20, height = 15, units = "cm")

# 1.2 Tabela de medidas resumo ----
resumo_peso <- dados %>%
  summarise(
    media = mean(weight),
    mediana = median(weight),
    dp = sd(weight),
    min = min(weight),
    max = max(weight)
  )
xtable(resumo_peso, caption = "resumo peso")

# 2. Peso dos pintinhos segundo a dieta ----
# 2.1 Boxplot ----
box_peso_dieta <- ggplot(dados) +
  aes(x = reorder(Diet, weight, FUN = median), y = weight) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Dieta", y = "Peso (gramas)") +
  theme_estat() +
  scale_x_discrete() # eixo x categórico (o theme_estat() define o eixo x como contínuo)
box_peso_dieta
ggsave("resultados/images/box_peso_dieta.pdf", box_peso_dieta, width = 158, height = 93, units = "mm")

# 2.2 Quadro de medidas resumo por dieta ----
# Com dados agrupados, o print_quadro_resumo() precisa do ponto como separador
# decimal (a vírgula aparece no PDF pelo siunitx do estat.cls)
options(OutDec = ".")
dados %>%
  group_by(Diet) %>%
  print_quadro_resumo(
    weight,
    title = "Medidas resumo do peso dos pintinhos (em gramas) por dieta",
    label = "quad:peso_dieta"
  )
options(OutDec = ",")

# 3. Medidas complementares (citadas no texto) ----
assimetria <- function(x) mean(((x - mean(x)) / sd(x))^3)
curtose <- function(x) mean(((x - mean(x)) / sd(x))^4) - 3

medidas_complementares <- function(dados) {
  dados %>%
    summarise(
      n = n(),
      `Coef. de Variação (%)` = round(100 * sd(weight) / mean(weight), 2),
      Assimetria = round(assimetria(weight), 2),
      Curtose = round(curtose(weight), 2),
      `Amplitude Interquartil` = IQR(weight),
      `Valores atípicos` = length(boxplot.stats(weight)$out)
    )
}

# 3.1 Geral ----
medidas_complementares(dados)
dados %>%
  summarise(
    `Até 100 g (%)` = round(100 * mean(weight <= 100), 2),
    `Acima de 200 g (%)` = round(100 * mean(weight > 200), 2)
  )
dados %>%
  filter(weight %in% boxplot.stats(weight)$out) %>%
  arrange(weight)

# 3.2 Por dieta ----
dados %>%
  group_by(Diet) %>%
  medidas_complementares()