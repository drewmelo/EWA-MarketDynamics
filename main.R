### ======================= SCRIPT PRINCIPAL ==============================
## CONFIGURAÇÃO INICIAL -------------------------------------------------------
# Instalando e carregando pacotes necessários
# install.packages("devtools")
# devtools::install_github("yukiyanai/rgamer")

source("scripts/pacotes.R")

# Carregando funções auxiliares
source("scripts/função_auxiliar.R")

## DEFINIÇÃO DA MATRIZ DE JOGO -------------------------------------------------
# Definindo um jogo de forma normal (payoff matrix)
set.seed(06-08-2024)

# Carregando os payoffs dos jogadores
source("scripts/payoffs.R")

# Criando o jogo Buyer-Seller Game (BSG)
matriz_bsg <- normal_form(
  players = c("Vendedor", "Comprador"),
  pars = c("preco_vendedor", "estrategia_comprador"),
  s1 = c("Preço Alto", "Preço Baixo"),  # Estratégias do vendedor
  s2 = c("Aceitar", "Rejeitar"),  # Estratégias do comprador
  payoffs1 = func_payoff1,
  payoffs2 = func_payoff2,
  discretize = T
)

# Criando o jogo Market Entry Game (MEG)
matriz_meg <- rgamer::normal_form(
  players = c("Empresa A", "Empresa B"),
  s1 = c("Não Entrar", "Entrar"),
  s2 = c("Não Entrar", "Entrar"),
  payoffs1 = c(0, 5, 0, -1),
  payoffs2 = c(0, 0, 5, -1)
)

# Resolvendo o jogo para encontrar o equilíbrio de Nash
s_matriz_bsg <- rgamer::solve_nfg(matriz_bsg, mark_br = T)

s_matriz_meg <- rgamer::solve_nfg(matriz_meg, mark_br = T)

# Exportando os resultados (/tabelas) ------------------------------------------

# Salvar em um arquivo HTML temporário
save_kable(s_matriz_bsg$table, file = "tabelas/tabela_1.html")

save_kable(s_matriz_meg$table, file = "tabelas/tabela_2.html")

# Salvar como PNG
webshot("tabelas/tabela_1.html", file = "tabelas/tabela_1.png",
        vwidth = 435, vheight = 176)

webshot("tabelas/tabela_2.html", file = "tabelas/tabela_2.png",
        vwidth = 435, vheight = 176)


### SIMULAÇÃO DOS DADOS --------------------------------------------------------

# Simulando aprendizado de jogo com os modelos EWA, RL e BL --------------------

lambda_values <- seq(0.1, 1, by = 0.1) # Variação do parâmetro lambda

# Configurações para cada tipo de aprendizado
learning_types <- list(
  ewa = list(type = "EWA", delta = 0.75, rho = 0.31, phi = 0.62),
  rl = list(type = "reinforcement", delta = 0, rho = 0, phi = 1),
  bl = list(type = "belief", delta = 1, rho = 1, phi = 1)
)

## MATRIZ 1 - BSG --------------------------------------------------------------

set.seed(07-02-2025)
# Inicializando lista de resultados para a matriz BSG

results_bsg <- list()

# Iterando sobre os tipos de aprendizado
for (learning_type in names(learning_types)) {
  # Configurando os parâmetros do tipo de aprendizado
  config <- learning_types[[learning_type]]
  results_bsg[[learning_type]] <- list()

  # Variando os valores de lambda
  for (i in seq_along(lambda_values)) {
    lambda_value <- lambda_values[i]

    # Simulação para a matriz BSG
    result <- sim_learning(
      matriz_bsg,
      n_samples = 500,
      n_periods = 500,
      type = config$type,
      lambda = lambda_value,
      delta = config$delta,
      rho = config$rho,
      phi = config$phi
    )

    # Armazenando resultados da simulação
    results_bsg[[learning_type]][[i]] <- result
  }
}

## MATRIZ 2 - MEG --------------------------------------------------------------
# Inicializando lista de resultados para a matriz MEG
results_meg <- list()

# Iterando sobre os tipos de aprendizado
for (learning_type in names(learning_types)) {
  # Configurando os parâmetros do tipo de aprendizado
  config <- learning_types[[learning_type]]
  results_meg[[learning_type]] <- list()

  # Variando os valores de lambda
  for (i in seq_along(lambda_values)) {
    lambda_value <- lambda_values[i]

    # Simulação para a matriz MEG
    result <- sim_learning(
      matriz_meg,
      n_samples = 500,
      n_periods = 500,
      type = config$type,
      lambda = lambda_value,
      delta = config$delta,
      rho = config$rho,
      phi = config$phi
    )

    # Armazenando resultados da simulação
    results_meg[[learning_type]][[i]] <- result
  }
}

### EXTRAÇÃO E PROCESSAMENTO --------------------------------------------------

# Carregando o script para extração e processamento dos dados simulados
source("scripts/extração_processamento.R")

### EXPORTANDO AS SIMULAÇÕES --------------------------------------------------



