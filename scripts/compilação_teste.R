## TESTE DE RESULTADOS (COMPILAÇÃO DO EQUILÍBRIO DE NASH) ---------------------

# Carregando os payoffs dos jogadores
source("scripts/payoffs.R")

# Inicialize uma lista para armazenar os data frames
lista_dfs <- list()

# Verificando o resultado (100 vezes)
for (i in 1:100) {
  matriz_1_ex <- normal_form(
    players = c("Vendedor", "Comprador"),
    pars = c("preco_vendedor", "estrategia_comprador"),
    s1 = c("Preço Alto", "Preço Baixo"),  # Estratégias do vendedor
    s2 = c("Aceitar", "Rejeitar"),  # Estratégias do comprador
    payoffs1 = func_payoff1,
    payoffs2 = func_payoff2,
    discretize = T
  )

  resultado <- solve_nfg(matriz_1_ex, mark_br = TRUE)

  # Extrair a primeira linha do resultado
  if (is.character(resultado)) {
    df_resultado <- tibble(NE = resultado)
  } else {
    df_resultado <- tibble(NE = resultado[1])
  }

  lista_dfs[[i]] <- df_resultado
}

ne_m1 <- do.call(rbind, lista_dfs)

# Verificando os resultados
ne_m1 |>
  filter(NE == "[Preço Alto, Rejeitar]")

# Concatenar todos os elementos dentro de cada lista como uma string única
ne_m1$NE <- sapply(ne_m1$NE, function(x) paste(x, collapse = ", "))

# Agrupando por categoria de NE e calculando a contagem acumulada
ne_m1 <- ne_m1 |>
  group_by(NE) |>
  mutate(count_n = cumsum(rep(1, n()))) |>
  ungroup()

# Criando o gráfico com ggplot2
pc1 <- ggplot(ne_m1, aes(x = count_n, color = NE, group = NE)) +
  geom_freqpoly(binwidth = 2, linewidth = .9, position = 'jitter') +
  labs(
    # title = "Contagem Acumulada por Categoria de NE"  # Se precisar do título, remova o comentário
    x = "Número de Ocorrências",
    y = "Contagem Acumulada",
    color = NULL  # Remove o título da legenda
  ) +
  scale_x_continuous(breaks = breaks_pretty(n = 6)) +
  paletteer::scale_colour_paletteer_d("MetBrewer::Egypt") +  # Define as cores manualmente
  theme_academic() +  # Aplica o tema personalizado
  theme(
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.y = element_text(margin = margin(l = 5, unit = "pt")),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22, angle = 90)
  )

ggsave(plot = pc1, filename = "figuras/figura_15.pdf",
       width = 10.81, height = 7.75, units = "in",
       device = cairo_pdf)
