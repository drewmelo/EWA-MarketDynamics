required_pkgs <- c("dplyr", "tidyr", "stringr", "glue", "purrr", "tibble", "zoo")

invisible(lapply(required_pkgs, library, character.only = TRUE))

source("scripts/funcoes/funcao_dados_auxiliar.R")
source("scripts/funcoes/funcao_processar_simulacoes.R")
source("scripts/funcoes/funcao_analise_sim.R")

sim_teste <- readRDS("testes/fixtures/teste_simulacao.rds")

matriz <- list(
  strategy = list(
    s1 = c("Não Entrar", "Entrar"),
    s2 = c("Não Entrar", "Entrar")
  )
)

df <- build_simulation_df(sim_teste, matriz, jogo = "MEG")

stopifnot(nrow(df) > 0)
stopifnot(all(c("type", "sim_id", "jogador", "periodo", "estrategia_escolhida") %in% names(df)))

prop_df <- get_props(df, sims = 1)

stopifnot(is.list(prop_df))
stopifnot("sim1" %in% names(prop_df))
stopifnot(nrow(prop_df$sim1) > 0)

estab <- get_stability(
  bsg = prop_df$sim1,
  meg = prop_df$sim1,
  limiar = 0.03,
  janela = 3,
  min_estavel = 0.6
)

stopifnot(is.list(estab))
stopifnot(all(c("completo", "resumo") %in% names(estab)))

cat("Check com fixture passou sem erros.\n")