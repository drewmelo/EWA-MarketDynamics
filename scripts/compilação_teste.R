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
