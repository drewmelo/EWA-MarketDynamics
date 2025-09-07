### ========================= SCRIPT PRINCIPAL ================================
## CONFIGURAÇÃO INICIAL -------------------------------------------------------

# Instalando e carregando pacotes necessários
# install.packages("devtools")
# devtools::install_github("yukiyanai/rgamer")

# PACOTES A SEREM UTILIZADOS
base::source("scripts/pacotes.R")

fonte_base <- "timesnewroman"

color_main <- c("#0B86CA", "#566876", "#9AADB2", "#B1283AFF")

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

# Carregando o script para extração e processamento dos dados simulados
base::source("scripts/funcoes/funcao_processar_simulacoes.R")

# Carregando função para simulações do parâmetro lambda de 0.1 a 1
base::source("scripts/funcoes/funcao_sim_lambda.R")

base::source("scripts/funcoes/funcao_latex.R")

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

# Verificando os resultados diretamente
ne_m1 |>
  group_by(NE) |>
  reframe(n = n()) |>
  mutate(freq = n / sum(n)) |>
  filter(n == max(n)) |>
  (\(df) cat("Durante esta reprodutibilidade, em", i, "iterações, a frequência relativa do NE",
             df$NE, "no BSG é", paste0(round(df$freq * 100, 2), "%"), "\n"))()

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

base::set.seed(07-02-2025)

## MATRIZ 1 - BSG --------------------------------------------------------------

results_bsg <- sim_lambda(
  matriz = matriz_bsg,
  game_label = "BSG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 500,
  n_periods = 500
)

## MATRIZ 2 - MEG --------------------------------------------------------------

results_meg <- sim_lambda(
  matriz = matriz_meg,
  game_label = "MEG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 500,
  n_periods = 500
)

### EXTRAÇÃO E PROCESSAMENTO --------------------------------------------------

bsg_df <- build_simulation_df(results_bsg, matriz_bsg, jogo = "BSG")

meg_df <- build_simulation_df(results_meg, matriz_meg, jogo = "MEG")

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
sim_ids <- 1:ifelse( 
                max(bsg_df$sim_id) == max(bsg_df$sim_id), # Lista de valores de sim_id (lambda de 0.1 a 1)
                max(bsg_df$sim_id), NA                    # Necessário o mesmo número de simulações para os dois jogos
             ) 

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
  tab_6 = frequency_probabilities(df = bsg_df),  # Probabilidades do BSG
  tab_7 = frequency_probabilities(df = meg_df))  # Probabilidades do MEG

  # Tabela de contigência (Tipo de modelo e estratégia escolhida)
contigencia <- contingency_plots(
    data_m1 = bsg_df,
    data_m2 = meg_df,
    matrix_m1 = matriz_bsg,  # Matriz para o jogo 1 (ex: BSG)
    matrix_m2 = matriz_meg,  # Matriz para o jogo 2 (ex: MEG)
    tag_label_m1 = "(a) BSG",
    tag_label_m2 = "(b) MEG"
  )

# -------------------------------
# Jogo A (BSG)
# -------------------------------
chi_tbl8a <- chisq.test(contigencia$tables$tab_abs_m1)

tabela_8a <- list(
  prob = contigencia$tables$tab_rel_total_m1,
  obs = contigencia$tables$tab_abs_m1 |> addmargins(),
  esp = chi_tbl8a$expected |> addmargins(),
  res = (contigencia$tables$tab_abs_m1[1:3, 1:4] -
         chi_tbl8a$expected[1:3, 1:4])^2 /
         chi_tbl8a$expected[1:3, 1:4]
)

# -------------------------------
# Jogo B (MEG)
# -------------------------------
chi_tbl8b <- chisq.test(contigencia$tables$tab_abs_m2)

tabela_8b <- list(
  prob = contigencia$tables$tab_rel_total_m2,
  obs = contigencia$tables$tab_abs_m2 |> addmargins(),
  esp = chi_tbl8b$expected |> addmargins(),
  res = (contigencia$tables$tab_abs_m2[1:3, 1:4] -
         chi_tbl8b$expected[1:3, 1:4])^2 /
         chi_tbl8b$expected[1:3, 1:4]
)

# -------------------------------
# Visualização rápida (em milhões, 1 casa decimal)
# -------------------------------

tabela_8b$esp <- tabela_8b$esp[, colnames(tabela_8b$esp) != "Sum"]

tabela_8b$esp <- tabela_8b$esp[rownames(tabela_8b$esp) != "Sum", ]

tabela_8a$esp <- tabela_8a$esp[, colnames(tabela_8a$esp) != "Sum"]

tabela_8a$esp <- tabela_8a$esp[rownames(tabela_8a$esp) != "Sum", ]

writeLines(
  tex_matrix(
      tabela_8a$esp/1e6, tabela_8a$res,
      tabela_8b$esp/1e6, tabela_8b$res,
      caption = "Probabilidade média (valor principal) com estatística complementar (entre parênteses)",
      label   = "tabela_9",
      digits_main = 1, digits_paren = 1, decimal = ",",
      show_paren_in_total = FALSE
  ), "tabelas/tabela_9.tex"
)

# Jogo A (BSG)
tabela_8a$prob
round(tabela_8a$obs / 1e6, 1)   # observados
round(tabela_8a$esp / 1e6, 1)   # esperados
round(tabela_8a$res / 1e3, 1)   # desvios

round(tabela_8a$obs / 1e3, 1)   # observados
round(tabela_8a$esp / 1e3, 1)   # esperados
round(tabela_8a$res, 1)   # desvios

# Jogo B (MEG)
tabela_8b$prob
round(tabela_8b$obs / 1e6, 1)   # observados
round(tabela_8b$esp / 1e6, 1)   # esperados
round(tabela_8b$res / 1e3, 1)   # desvios

round(tabela_8a$obs / 1e3, 1)   # observados
round(tabela_8b$esp / 1e3, 1)   # esperados
round(tabela_8b$res / 1e3, 1)   # desvios

# tabela 8
# 2) salvar em arquivo .tex para colar no seu documento
write_probtex(
  tabela_8a$prob, tabela_8b$prob,
  file    = "tabelas/tabela_8.tex",
  caption = "Probabilidade média por estratégia — BSG e MEG",
  label   = "tabela_8"
)


# valores observados

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

plot_histogram(
  data = bsg_df,
  bins = grDevices::nclass.Sturges(bsg_df$probabilidade),
  file_name = "figuras/figura_18.pdf"
)

plot_histogram(
  data = meg_df,
  bins = grDevices::nclass.Sturges(meg_df$probabilidade),
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
ggplot2::ggsave(plot = p22,
                filename = "figuras/figura_21.pdf",
                width = 10.81, height = 7.75, units = "in",
                device = cairo_pdf)

# Figura 22 (plotagem dos dados das tabelas (8 e 9) de contigência)

ggplot2::ggsave(plot = contigencia$plots$combined_plot,
                filename = "figuras/figura_22.pdf",
                width = 14, height = 9.02, units = "in",
                device = cairo_pdf)


## TABELAS --------------------------------------------------------------------

# Tabelas 1 e 2 (matrizes dos jogos)
export_game_table(
  game_matrix = matriz_bsg,
  title = "Tabela 1 - Matriz de ganhos do jogo BSG",
  file = "tabelas/tabela_1.pdf",
  height = 7, width = 10,  # Ajuste o tamanho do PDF
  overwrite = T
)

export_game_table(
  game_matrix = matriz_meg,
  title = "Tabela 2 - Matriz de ganhos do jogo MEG",
  file = "tabelas/tabela_2.pdf",
  height = 7, width = 10,
  overwrite = T
)

# 🔹 Tabelas 3 a 5 (medidas resumo dos dados)
# Para EWA
model_stats_table(
  data   = resumo_dados$tab_3,
  file   = "tabelas/tabela_3.png",
  modelo = "EWA",
  title  = "Tabela 3 — Estatísticas EWA"
)

# Ou salvar direto num arquivo (sem preâmbulo)
model_stats_tabletex(
  data     = resumo_dados$tab_3,
  modelo   = "EWA",
  caption  = "Estatísticas do Modelo EWA para BSG e MEG",
  label    = "tabela_3",
  file_tex = "tabelas/tabela_3.tex"
)

# Para RL
model_stats_table(
  data   = resumo_dados$tab_4,
  file   = "tabelas/tabela_4.png",
  modelo = "RL",
  title  = "Tabela 4 — Estatísticas RL"
)

# Ou salvar direto num arquivo (sem preâmbulo)
model_stats_tabletex(
  data     = resumo_dados$tab_4,
  modelo   = "RL",
  caption  = "Estatísticas do Modelo RL para BSG e MEG",
  label    = "tabela_4",
  file_tex = "tabelas/tabela_4.tex"
)

# Para BL
model_stats_table(
  data   = resumo_dados$tab_5,
  file   = "tabelas/tabela_5.png",
  modelo = "BL",
  title  = "Tabela 5 — Estatísticas BL"
)

# Ou salvar direto num arquivo (sem preâmbulo)
model_stats_tabletex(
  data     = resumo_dados$tab_5,
  modelo   = "BL",
  caption  = "Estatísticas do Modelo BL para BSG e MEG",
  label    = "tabela_5",
  file_tex = "tabelas/tabela_5.tex"
)

# 🔹 Tabelas 6 e 7 (probabilidade média)
avg_prob_table(
  data      = resumo_dados$tab_6,
  file      = "tabelas/tabela_6.png",
  p1_player = "Vendedor",
  p1_pair   = c("Preço Alto","Preço Baixo"),
  p2_player = "Comprador",
  p2_pair   = c("Aceitar","Rejeitar"),
  title     = "Tabela 6 — Preço Alto/Preço Baixo (P1) e Aceitar/Rejeitar (P2)"
)

avg_prob_tabletex(
  data      = resumo_dados$tab_6,
  p1_player = "Vendedor",  p1_pair = c("Preço Alto","Preço Baixo"),
  p2_player = "Comprador", p2_pair = c("Aceitar","Rejeitar"),
  p1_label  = "P1", p2_label = "P2",
  caption   = "Probabilidade média — BSG",
  label     = "tabela_6",
  font_size = 10,
  file_tex  = "tabelas/tabela_6.tex"
)

avg_prob_table(
  data      =  resumo_dados$tab_7,
  file      = "tabelas/tabela_7.png",
  p1_player = "Empresa A",
  p1_pair   = c("Entrar","Não Entrar"),
  p2_player = "Empresa B",
  p2_pair   = c("Entrar","Não Entrar"),
  title     = "Tabela 7 — Entrar/Não Entrar (P1) e Entrar/Não Entrar (P2)"
)

avg_prob_tabletex(
  data      = resumo_dados$tab_7,
  p1_player = "Empresa A", p1_pair = c("Entrar","Não Entrar"),
  p2_player = "Empresa B", p2_pair = c("Entrar","Não Entrar"),
  p1_label  = "P1", p2_label = "P2",
  caption   = "Probabilidade média — MEG",
  label     = "tabela_7",
  font_size = 10,
  file_tex  = "tabelas/tabela_7.tex"
)

table_wide(
  prob_bsg = tabela_8a$prob,
  prob_meg = tabela_8b$prob,
  file     = "tabelas/tabela_8.png",
  title    = "Distribuição (%) por estratégia e modelo",
  digits   = 1
)

write_probtex(
  tabela_8a$prob, tabela_8b$prob,
  file    = "tabelas/tabela_8.tex",
  caption = "Probabilidade média por estratégia — BSG e MEG",
  label   = "tabela_8"
)

# Dahsboard das simulações
# 1) Bloco de pacotes/temas/fontes (o que te mandei)
# 2) Objetos do projeto: matrizes, palettes, fonte_base, learning_types, etc.
# 3) Funções do projeto:
#    sim_lambda(), build_simulation_df(), data_extraction(), player_render(),
#    remove_cols(), sim_plot(), e quaisquer outras helpers que o app usa.
# 4) Helpers do app: order_strats(), standardize_and_prop(), plot_model(), run_sim() ...
# 5) UI (page_fluid / layout_sidebar / cards ...)
# 6) server <- function(input, output, session) { ... }
# 7) shinyApp(ui, server)

base::source("app.R")