### ==================== VISUALIZAÇÃO DAS SIMULAÇÕES ===========================

# Calcula a proporção (freq. relativa) de estratégias escolhidas para MEG
prop_meg <- meg_df |>
  dplyr::mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
  dplyr::group_by(type, sim_id, jogador, periodo, estrategias) |>
  dplyr::reframe(
    count_escolhida = sum(escolhida),  # Conta quantas vezes a estratégia foi escolhida
    total_amostra = n()  # Conta o total de observações no grupo
  ) |>
  dplyr::mutate(prop = (count_escolhida / total_amostra))

# Calcula a proporção (freq. relativa) de estratégias escolhidas para BSG
prop_bsg <- bsg_df |>
  dplyr::mutate(escolhida = ifelse(estrategia_escolhida == estrategias, 1, 0)) |>
  dplyr::group_by(type, sim_id, jogador, periodo, estrategias) |>
  dplyr::reframe(
    count_escolhida = sum(escolhida),  # Conta quantas vezes a estratégia foi escolhida
    total_amostra = n()  # Conta o total de observações no grupo
  ) |>
  dplyr::mutate(
    prop = (count_escolhida / total_amostra),
    estrategias = factor(estrategias, levels = c("Aceitar",
                                                 "Rejeitar",
                                                 "Preço Alto",
                                                 "Preço Baixo")))

## SCRIPTS DOS PLOTS SIMULAÇÕES  ----------------------------------------------

# Simulação 1 (lambda = 0.1)
source("scripts/simulações/sim_1.R")

p_s1

# Simulação 2 (lambda = 0.2)
source("scripts/simulações/sim_2.R")

p_s2

# Simulação 3 (lambda = 0.3)
source("scripts/simulações/sim_3.R")

p_s3

# Simulação 4 (lambda = 0.4)
source("scripts/simulações/sim_4.R")

p_s4

# Simulação 5 (lambda = 0.5)
source("scripts/simulações/sim_5.R")

p_s5

# Simulação 6 (lambda = 0.6)
source("scripts/simulações/sim_6.R")

p_s6

# Simulação 7 (lambda = 0.7)
source("scripts/simulações/sim_7.R")

p_s7

# Simulação 8 (lambda = 0.8)
source("scripts/simulações/sim_8.R")

p_s8

# Simulação 9 (lambda = 0.9)
source("scripts/simulações/sim_9.R")

p_s9

# Simulação 10 (lambda = 1)
source("scripts/simulações/sim_10.R")

p_s10

# Limpeza do ambiente ---------------------------------------------------------
# Lista os objetos que começam com "meg_p", "bsg_p", "s1_bsg", "s1_meg", ..., "s10_bsg", "s10_meg"
obj_remov <- ls(pattern = "^(meg_p|bsg_p|s[1-9]_bsg|s10_bsg|s[1-9]_meg|s10_meg)")

# Remove os objetos listados
rm(list = obj_remov)

## EXPORTAÇÃO DAS FIGURAS  ----------------------------------------------------
# Figura 8 (Simulação 1)
ggsave(plot = p_s1, filename = "figuras/figura_8.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 9 (Simulação 2)
ggsave(plot = p_s2, filename = "figuras/figura_9.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 10 (Simulação 3)
ggsave(plot = p_s3, filename = "figuras/figura_10.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 11 (Simulação 4)
ggsave(plot = p_s4, filename = "figuras/figura_11.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 12 (Simulação 5)
ggsave(plot = p_s5, filename = "figuras/figura_12.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 13 (Simulação 6)
ggsave(plot = p_s6, filename = "figuras/figura_13.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 14 (Simulação 7)
ggsave(plot = p_s7, filename = "figuras/figura_14.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 15 (Simulação 8)
ggsave(plot = p_s8, filename = "figuras/figura_15.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 16 (Simulação 9)
ggsave(plot = p_s9, filename = "figuras/figura_16.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)

# Figura 17 (Simulação 10)
ggsave(plot = p_s10, filename = "figuras/figura_17.pdf",
       width = 12.13, height = 9.02, units = "in",
       device = cairo_pdf)
