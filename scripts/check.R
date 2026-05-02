# scripts/check.R
# Checagem leve para GitHub Actions

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  warn = 1
)

cat("Iniciando checagem do projeto...\n")

# Pacotes 
library(dplyr)
library(tidyr)
library(stringr)
library(glue)
library(purrr)
library(tibble)
library(zoo)
library(rgamer)

fonte_base <- "sans"
color_main <- c("#0B86CA", "#566876", "#9AADB2", "#B1283AFF")

# Funções do projeto
source("scripts/funcoes/funcao_dados_auxiliar.R")
source("scripts/funcoes/funcao_processar_simulacoes.R")
source("scripts/funcoes/funcao_sim_lambda.R")
source("scripts/funcoes/funcao_analise_sim.R")

# Payoffs
source("scripts/payoffs.R")

# Matrizes dos jogos
matriz_bsg <- rgamer::normal_form(
  players = c("Vendedor", "Comprador"),
  pars    = c("preco_vendedor", "estrategia_comprador"),
  s1      = s1,
  s2      = s2,
  payoffs1 = pay1_avg,
  payoffs2 = pay2_avg,
  discretize = TRUE
)

matriz_meg <- rgamer::normal_form(
  players = c("Empresa A", "Empresa B"),
  s1 = c("Não Entrar", "Entrar"),
  s2 = c("Não Entrar", "Entrar"),
  payoffs1 = c(0, 5, 0, -1),
  payoffs2 = c(0, 0, 5, -1)
)

# Configuração mínima
lambda_values <- c(0.1)

learning_types <- list(
  EWA = list(type = "EWA", delta = 0.75, rho = 0.31, phi = 0.62),
  RL  = list(type = "reinforcement", delta = 0, rho = 0, phi = 1),
  BL  = list(type = "belief", delta = 1, rho = 1, phi = 1)
)

set.seed(123)

# Simulação pequena apenas para checar se roda
results_bsg <- sim_lambda(
  matriz = matriz_bsg,
  game_label = "BSG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 5,
  n_periods = 5
)

results_meg <- sim_lambda(
  matriz = matriz_meg,
  game_label = "MEG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 5,
  n_periods = 5
)

bsg_df <- build_simulation_df(results_bsg, matriz_bsg, jogo = "BSG")
meg_df <- build_simulation_df(results_meg, matriz_meg, jogo = "MEG")

# Testes básicos
stopifnot(nrow(bsg_df) > 0)
stopifnot(nrow(meg_df) > 0)
stopifnot(all(c("type", "sim_id", "jogador", "periodo") %in% names(bsg_df)))
stopifnot(all(c("type", "sim_id", "jogador", "periodo") %in% names(meg_df)))

# Testa funções analíticas leves
prop_bsg <- get_props(bsg_df)
prop_meg <- get_props(meg_df)

estab <- get_stability(
  bsg = prop_bsg$sim1,
  meg = prop_meg$sim1,
  limiar = 0.03,
  janela = 3,
  min_estavel = 0.6
)

stopifnot(is.list(estab))
stopifnot(all(c("completo", "resumo") %in% names(estab)))

cat("Check passou sem erros.\n")