### ========================= SCRIPT PRINCIPAL ================================
## CONFIGURAÇÃO INICIAL -------------------------------------------------------

# Instalando e carregando pacotes necessários
# install.packages("devtools")
# devtools::install_github("yukiyanai/rgamer")

# PACOTES A SEREM UTILIZADOS
base::source("scripts/pacotes.R")

## FUNÇÕES A ATIVAR -----------------------------------------------------------

# Carregando funções auxiliares
base::source("scripts/funcoes/funcao_dados_auxiliar.R")

# Carregando funções para exportar as tabelas das matrizes em pdf para /tabelas
base::source("scripts/funcoes/funcao_matriz.R")

# Carregando o script para cálculo da proporção e plotagem das simulações
base::source("scripts/funcoes/funcao_sim_plot.R")

# Carregando funções para gerar medidas resumos dos dados
base::source("scripts/funcoes/funcao_descritiva.R")

# Carregando funções para plotagem das distribuições dos dados
base::source("scripts/funcoes/funcao_dist_dados.R")

## OUTROS SCRIPTS ADICIONAIS --------------------------------------------------

# Carregando os payoffs dos jogadores
base::source("scripts/payoffs.R")

# (Opcional) compilação dos testes de reprodutibilidade dos NE (equilíbrio de Nash)
base::source("scripts/compilacao_teste.R")

## DEFINIÇÃO DA MATRIZ DE JOGO -------------------------------------------------
# Definindo um jogo de forma normal (payoff matrix)
base::set.seed(06-08-2024)

# Criando o jogo Buyer-Seller Game (BSG)
matriz_bsg <- rgamer::normal_form(
  players = c("Vendedor", "Comprador"),
  pars = c("preco_vendedor", "estrategia_comprador"),
  s1 = c("Preço Alto", "Preço Baixo"),  # Estratégias do vendedor
  s2 = c("Aceitar", "Rejeitar"),  # Estratégias do comprador
  payoffs1 = func_payoff1,
  payoffs2 = func_payoff2,
  discretize = TRUE
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
s_matriz_bsg <- rgamer::solve_nfg(matriz_bsg, mark_br = TRUE)

s_matriz_meg <- rgamer::solve_nfg(matriz_meg, mark_br = TRUE)

# Carregando script do equilíbrio de Nash (plotagem)
base::source("scripts/equilibrio_nash.R")

### SIMULAÇÃO DOS DADOS --------------------------------------------------------

# Simulando aprendizado de jogo com os modelos EWA, RL e BL --------------------

lambda_values <- base::seq(0.1, 1, by = 0.1) # Variação do parâmetro lambda

# Configurações para cada tipo de aprendizado
learning_types <- base::list(
  EWA = base::list(type = "EWA", delta = 0.75, rho = 0.31, phi = 0.62),
  RL = base::list(type = "reinforcement", delta = 0, rho = 0, phi = 1),
  BL = base::list(type = "belief", delta = 1, rho = 1, phi = 1)
)

## MATRIZ 1 - BSG --------------------------------------------------------------

base::set.seed(07-02-2025)
# Inicializando lista de resultados para a matriz BSG
results_bsg <- base::list()

# Iterando sobre os tipos de aprendizado
for (learning_type in base::names(learning_types)) {
  # Configurando os parâmetros do tipo de aprendizado
  config <- learning_types[[learning_type]]
  results_bsg[[learning_type]] <- base::list()

  # Variando os valores de lambda
  for (i in base::seq_along(lambda_values)) {
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
results_meg <- base::list()

# Iterando sobre os tipos de aprendizado
for (learning_type in base::names(learning_types)) {
  # Configurando os parâmetros do tipo de aprendizado
  config <- learning_types[[learning_type]]
  results_meg[[learning_type]] <- base::list()

  # Variando os valores de lambda
  for (i in base::seq_along(lambda_values)) {
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
base::source("scripts/extracao_processamento.R")

# Manipulando os dados (pequenas alterações)
bsg_df <- bsg_df |>
  dplyr::mutate(lambda = base::factor(
    sim_id,
    levels = 1:10,
    labels = base::paste("λ = ", stringr::str_replace(lambda_values, "\\.", ",")))
  ) |>
  dplyr::relocate(lambda, .after = sim_id)

meg_df <- meg_df |>
  dplyr::mutate(lambda = base::factor(
    sim_id,
    levels = 1:10,
    labels = base::paste("λ = ", stringr::str_replace(lambda_values, "\\.", ",")))
  ) |>
  dplyr::relocate(lambda, .after = sim_id)

### VISUALIZAÇÃO DAS SIMULAÇÕES (PLOTS) ---------------------------------------

# Criar a lista de plots usando um loop
sim_ids <- 1:sim_id  # Lista de valores de sim_id (lambda de 0.1 a 1)

plots <- stats::setNames(
  base::lapply(sim_ids, function(sim) {
    sim_plot(df_m1 = bsg_df, df_m2 = meg_df,
             matrix_m1 = matriz_bsg, matrix_m2 = matriz_meg,
             sim_id = sim)
  }),
  base::paste0("sim", sim_ids)  # Nomeia os elementos da lista como "sim1", "sim2", ..., "sim10"
)

base::lapply(plots, base::print)

### ANÁLISE DESCRITIVA / RESUMO DOS DADOS -------------------------------------

mr_lista <- base::list(
  mr_bsg = summary_statistics(df = bsg_df, sufixo = "m1"),
  mr_meg = summary_statistics(df = meg_df, sufixo = "m2"))

# Criar lista contendo as tabelas para cada tipo
resumo_dados <- base::list(
  tab_3 = summary_tables(modelo = "EWA", lista_resumo = mr_lista),
  tab_4 = summary_tables(modelo = "RL", lista_resumo = mr_lista),
  tab_5 = summary_tables(modelo = "BL", lista_resumo = mr_lista),
  tab_6 = frequency_probabilities(df = bsg_df) |>
    format_summary_table(matrix_game = matriz_bsg),  # Probabilidades do BSG
  tab_7 = frequency_probabilities(df = meg_df) |>
    format_summary_table(matrix_game = matriz_meg), # Probabilidades do MEG

  # Tabela de contigência (Tipo de modelo e estratégia escolhida)
contigencia <- contingency_plots(
    data_m1 = bsg_df,
    data_m2 = meg_df,
    matrix_m1 = matriz_bsg,  # Matriz para o jogo 1 (ex: BSG)
    matrix_m2 = matriz_meg,  # Matriz para o jogo 2 (ex: MEG)
    tag_label_m1 = "(a) BSG",
    tag_label_m2 = "(b) MEG"
  )
)

### EXPORTAÇÃO FIGURAS / TABELAS ----------------------------------------------

## FIGURAS --------------------------------------------------------------------

# Figuras 3 e 4 (equilíbrios de nash)

ggplot2::ggsave(
  plot = p3, filename = "figuras/figura_3.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)

# Figura 4 (MEG)
ggplot2::ggsave(
  plot = p4, filename = "figuras/figura_4.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)


# Exportando as simulações para a pasta /figuras (figuras 8 a 17)
base::lapply(sim_ids, function(i) {
  ggplot2::ggsave(plot = plots[[base::paste0("sim", i)]],
                  filename = base::paste0("figuras/figura_", i + 7, ".pdf"),  # Figura 8 até 17
                  width = 12.13, height = 9.02, units = "in",
                  device = cairo_pdf)
})

# Figuras 18 e 19 (análise da distribuição dos dados -- histograma)
n_m1 <- base::nrow(bsg_df)

n_m2 <- base::nrow(meg_df)

plot_histogram(
  data = bsg_df,
  bins = base::round(1 + 3.322 * log(n_m1)),
  file_name = "figuras/figura_18.pdf"
)

plot_histogram(
  data = meg_df,
  bins = base::round(1 + 3.322 * log(n_m2)),
  file_name = "figuras/figura_19.pdf"
)

# Figura 20 (correlação entre atração e probabilidade)

plot_correlation(
  data_m1 = bsg_df,  # Dados do BSG
  data_m2 = meg_df,   # Dados do MEG
  file_name_combined = "figuras/figura_20.pdf",
  file_name_m1 = "figuras/figura_20a.pdf",
  file_name_m2 = "figuras/figura_20b.pdf",
  tag_label_m1 = "(a) BSG",
  tag_label_m2 = "(b) MEG"
)


# Figura 21 (compilação dos Equilíbrios de Nash no BSG) -- APÊNDICE A
ggplot2::ggsave(plot = p22, filename = "figuras/figura_21.pdf",
                width = 10.81, height = 7.75, units = "in",
                device = cairo_pdf)

# Figura 22 (plotagem dos dados das tabelas (8 e 9) de contigência)

ggplot2::ggsave(plot = contigencia$plots$combined_plot, filename = "figuras/figura_22.pdf",
                width = 14, height = 9.02, units = "in",
                device = cairo_pdf)


## TABELAS --------------------------------------------------------------------

# Tabelas 1 e 2 (matrizes dos jogos)
export_game_table(
  game_matrix = matriz_bsg,
  title = "Tabela 1 - Matriz de ganhos do jogo BSG",
  file = "tabelas/tabela_1.pdf",
  height = 7, width = 10,  # Ajuste o tamanho do PDF
  overwrite = TRUE
)

export_game_table(
  game_matrix = matriz_meg,
  title = "Tabela 2 - Matriz de ganhos do jogo MEG",
  file = "tabelas/tabela_2.pdf",
  height = 7, width = 10,
  overwrite = TRUE
)

# 🔹 Tabelas 3 a 5 (medidas resumo dos dados)
plot_table(resumo_dados$tab_3,
           title = "Table 3 - Summary Measures of BSG and MEG in the EWA Learning Model",
           footnote = "A presença 'm1' nas colunas implica no jogo BSG, assim como 'm2' em outras colunas são do jogo MEG.",
           file = "tabelas/tabela_3.pdf",
           height = 4, width = 12, font_size = 20,
           overwrite = TRUE)

plot_table(resumo_dados$tab_4,
           title = "Table 4 - Summary Measures of BSG and MEG in the RL Learning Model",
           footnote = "A presença 'm1' nas colunas implica no jogo BSG, assim como 'm2' em outras colunas são do jogo MEG.",
           file = "tabelas/tabela_4.pdf",
           height = 4, width = 12, font_size = 20,
           overwrite = TRUE)

plot_table(resumo_dados$tab_5,
           title = "Table 5 - Summary Measures of BSG and MEG in the BL Learning Model",
           footnote = "A presença 'm1' nas colunas implica no jogo BSG, assim como 'm2' em outras colunas são do jogo MEG.",
           file = "tabelas/tabela_5.pdf",
           height = 4, width = 12, font_size = 20,
           overwrite = TRUE)

# 🔹 Tabelas 6 e 7 (probabilidade média)
plot_table(resumo_dados$tab_6,
           title = "Table 6 - Average Probability During the BSG Game",
           file = "tabelas/tabela_6.pdf",
           height = 8, width = 10, font_size = 17,
           overwrite = TRUE)

plot_table(resumo_dados$tab_7,
           title = "Table 7 - Average Probability During the MEG Game",
           file = "tabelas/tabela_7.pdf",
           height = 8, width = 10, font_size = 17,
           overwrite = TRUE)
