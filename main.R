### ========================= SCRIPT PRINCIPAL ================================
# ------------------------------------------------------------
# Configuração inicial
# 
# Definir ambiente de trabalho, carregar pacotes e estabelecer
# parâmetros globais de visualização.
# ------------------------------------------------------------

# Instalação do pacote rgamer (executar apenas se necessário)
# install.packages("devtools")
# devtools::install_github("yukiyanai/rgamer")

# Carrega pacotes utilizados no projeto
base::source("scripts/pacotes.R")

# Fonte padrão dos gráficos
fonte_base <- "timesnewroman"

# Paleta principal de cores
color_main <- c("#0B86CA", "#566876", "#9AADB2", "#B1283AFF")


# ------------------------------------------------------------
# Carregamento de funções
# 
# Importar todas as funções auxiliares utilizadas nas etapas de
# simulação, análise, visualização e exportação.
# ------------------------------------------------------------

# Funções auxiliares de dados
base::source("scripts/funcoes/funcao_dados_auxiliar.R")

# Funções para exportação de matrizes em PDF
base::source("scripts/funcoes/funcao_matriz.R")

# Funções de cálculo de proporções e gráficos de simulação
base::source("scripts/funcoes/funcao_sim_plot.R")

# Funções para estatísticas descritivas
base::source("scripts/funcoes/funcao_descritiva.R")

# Funções para distribuição dos dados
base::source("scripts/funcoes/funcao_dist_dados.R")

# Funções para processamento das simulações
base::source("scripts/funcoes/funcao_processar_simulacoes.R")

# Funções para simulação do parâmetro lambda
base::source("scripts/funcoes/funcao_sim_lambda.R")

# Funções para exportação em LaTeX
base::source("scripts/funcoes/funcao_latex.R")

# Funções de análise das simulações
base::source("scripts/funcoes/funcao_analise_sim.R")


# ------------------------------------------------------------
# Scripts adicionais
# 
# Carregar componentes complementares do modelo, como payoffs
# e testes opcionais de reprodutibilidade.
# ------------------------------------------------------------

# Payoffs dos jogos
base::source("scripts/payoffs.R")

# Testes de reprodutibilidade dos equilíbrios de Nash (opcional)
base::source("scripts/compilacao_teste.R")


# ------------------------------------------------------------
# Definição das matrizes de jogo
# 
# Construir os jogos em forma normal (BSG e MEG) com base nos
# payoffs definidos e nas estratégias dos jogadores.
# ------------------------------------------------------------

# Buyer-Seller Game (BSG)
# Matriz 2x2 com payoffs médios obtidos via simulação
matriz_bsg <- rgamer::normal_form(
  players = c("Vendedor", "Comprador"),
  pars    = c("preco_vendedor", "estrategia_comprador"),
  s1      = s1,
  s2      = s2,
  payoffs1 = pay1_avg,
  payoffs2 = pay2_avg,
  discretize = TRUE
)

# Market Entry Game (MEG)
# Jogo de entrada estratégica entre duas empresas
matriz_meg <- rgamer::normal_form(
  players = c("Empresa A", "Empresa B"),
  s1 = c("Não Entrar", "Entrar"),
  s2 = c("Não Entrar", "Entrar"),
  payoffs1 = c(0, 5, 0, -1),
  payoffs2 = c(0, 0, 5, -1)
)


# ------------------------------------------------------------
# Resolução dos jogos
# 
# Identificar os equilíbrios de Nash e destacar as melhores
# respostas em cada matriz de jogo.
# ------------------------------------------------------------

# Resolve o BSG
s_matriz_bsg <- rgamer::solve_nfg(matriz_bsg, mark_br = T)

# Resolve o MEG
s_matriz_meg <- rgamer::solve_nfg(matriz_meg, mark_br = T)

# Verificando os resultados diretamente
ne_m1 |>
  group_by(NE) |>
  reframe(n = n()) |>
  mutate(freq = n / sum(n)) |>
  filter(n == max(n)) |>
  (\(df) cat("Durante esta reprodutibilidade, em", i, "iterações, a frequência relativa do NE",
             df$NE, "no BSG é", paste0(round(df$freq * 100, 2), "%"), "\n"))()

p18

# Carregando script do equilíbrio de Nash (plotagem)
base::source("scripts/equilibrio_nash.R")

# ------------------------------------------------------------
# Simulação dos dados
# 
# Simular os jogos BSG e MEG para os modelos EWA, RL e BL,
# variando o parâmetro lambda entre 0,1 e 1.
# ------------------------------------------------------------

# Define os valores de lambda usados nas simulações.
lambda_values <- base::seq(0.1, 1, by = 0.1)

# Define as configurações dos modelos de aprendizado.
learning_types <- base::list(
  EWA = base::list(type = "EWA", delta = 0.75, rho = 0.31, phi = 0.62),
  RL = base::list(type = "reinforcement", delta = 0, rho = 0, phi = 1),
  BL = base::list(type = "belief", delta = 1, rho = 1, phi = 1)
)

# Fixa a semente para garantir reprodutibilidade.
base::set.seed(07-02-2025)


# ------------------------------------------------------------
# Simulação da matriz 1 - BSG
# 
# Simular o Buyer-Seller Game para todos os valores de lambda
# e modelos de aprendizado.
# ------------------------------------------------------------

results_bsg <- sim_lambda(
  matriz = matriz_bsg,
  game_label = "BSG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 500,
  n_periods = 500
)


# ------------------------------------------------------------
# Simulação da matriz 2 - MEG
# 
# Simular o Market Entry Game para todos os valores de lambda
# e modelos de aprendizado.
# ------------------------------------------------------------

results_meg <- sim_lambda(
  matriz = matriz_meg,
  game_label = "MEG",
  learning_types = learning_types,
  lambda_values = lambda_values,
  n_samples = 500,
  n_periods = 500
)


# ------------------------------------------------------------
# Extração e processamento
# 
# Transformar os resultados simulados em data frames organizados
# para análise, visualização e exportação.
# ------------------------------------------------------------

# Constrói a base final do BSG.
bsg_df <- build_simulation_df(results_bsg, matriz_bsg, jogo = "BSG")

# Constrói a base final do MEG.
meg_df <- build_simulation_df(results_meg, matriz_meg, jogo = "MEG")

# Adiciona os rótulos de lambda ao BSG e posiciona a variável após sim_id.
bsg_df <- bsg_df |>
  dplyr::mutate(lambda = base::factor(
    sim_id,
    levels = 1:10,
    labels = base::paste("λ = ", stringr::str_replace(lambda_values, "\\.", ",")))
  ) |>
  dplyr::relocate(lambda, .after = sim_id)

# Adiciona os rótulos de lambda ao MEG e posiciona a variável após sim_id.
meg_df <- meg_df |>
  dplyr::mutate(lambda = base::factor(
    sim_id,
    levels = 1:10,
    labels = base::paste("λ = ", stringr::str_replace(lambda_values, "\\.", ",")))
  ) |>
  dplyr::relocate(lambda, .after = sim_id)


# ------------------------------------------------------------
# Visualização das simulações
# 
# Gerar os gráficos das simulações para cada valor de lambda.
# ------------------------------------------------------------

# Cria a sequência de simulações a serem plotadas.
sim_ids <- 1:ifelse( 
                max(bsg_df$sim_id) == max(bsg_df$sim_id),
                max(bsg_df$sim_id), NA
             ) 

# Gera uma lista de gráficos, nomeada como sim1, sim2, ..., sim10.
plots <- stats::setNames(
  base::lapply(sim_ids, function(sim) {
    sim_plot(df_m1 = bsg_df, df_m2 = meg_df,
             matrix_m1 = matriz_bsg, matrix_m2 = matriz_meg,
             sim_id = sim)
  }),
  base::paste0("sim", sim_ids)
)

# Exibe todos os gráficos no console/visualizador.
purrr::walk(plots, ~ print(.x$plot))


# ------------------------------------------------------------
# Análise descritiva e resumo dos dados
# 
# Calcular medidas-resumo, tabelas de frequência e probabilidades
# para os dados simulados.
# ------------------------------------------------------------

# Calcula medidas-resumo para BSG e MEG.
mr_lista <- base::list(
  mr_bsg = summary_statistics(df = bsg_df, sufixo = "m1"),
  mr_meg = summary_statistics(df = meg_df, sufixo = "m2"))

# Organiza as tabelas descritivas por modelo e por jogo.
resumo_dados <- base::list(
  tab_4 = summary_tables(modelo = "EWA", lista_resumo = mr_lista),
  tab_5 = summary_tables(modelo = "RL", lista_resumo = mr_lista),
  tab_6 = summary_tables(modelo = "BL", lista_resumo = mr_lista),
  tab_7 = frequency_probabilities(df = bsg_df),
  tab_8 = frequency_probabilities(df = meg_df))


# ------------------------------------------------------------
# Tabelas de contingência
# 
# Cruzar modelos e estratégias escolhidas para avaliar a distribuição
# das escolhas nos jogos BSG e MEG.
# ------------------------------------------------------------

# Gera tabelas e gráficos de contingência para os dois jogos.
contigencia <- contingency_plots(
    data_m1 = bsg_df,
    data_m2 = meg_df,
    matrix_m1 = matriz_bsg,
    matrix_m2 = matriz_meg,
    tag_label_m1 = "(a) BSG",
    tag_label_m2 = "(b) MEG"
  )


# ------------------------------------------------------------
# Teste qui-quadrado - Jogo A (BSG)
# 
# Comparar frequências observadas e esperadas das estratégias
# escolhidas no BSG.
# ------------------------------------------------------------

chi_tbl9a <- chisq.test(contigencia$tables$tab_abs_m1)

chi_tbl9a$expected      # frequências esperadas
chi_tbl9a$residuals     # resíduos de Pearson com sinal
chi_tbl9a$stdres        # resíduos padronizados com sinal

tabela_9a <- list(
  prob = contigencia$tables$tab_rel_total_m1,
  obs = contigencia$tables$tab_abs_m1 |> addmargins(),
  esp = chi_tbl9a$expected |> addmargins(),
  res = (contigencia$tables$tab_abs_m1[1:3, 1:4] -
         chi_tbl9a$expected[1:3, 1:4])^2 /
         chi_tbl9a$expected[1:3, 1:4],
  res_pad = chi_tbl9a$stdres
)


# ------------------------------------------------------------
# Teste qui-quadrado - Jogo B (MEG)
# 
# Comparar frequências observadas e esperadas das estratégias
# escolhidas no MEG.
# ------------------------------------------------------------

chi_tbl9b <- chisq.test(contigencia$tables$tab_abs_m2)

chi_tbl9b$expected      # frequências esperadas
chi_tbl9b$residuals     # resíduos de Pearson com sinal
chi_tbl9b$stdres        # resíduos padronizados com sinal

# melhor
chi_tbl9b$stdres


tabela_9b <- list(
  prob = contigencia$tables$tab_rel_total_m2,
  obs = contigencia$tables$tab_abs_m2 |> addmargins(),
  esp = chi_tbl9b$expected |> addmargins(),
  res = (contigencia$tables$tab_abs_m2[1:3, 1:4] -
         chi_tbl9b$expected[1:3, 1:4])^2 /
         chi_tbl9b$expected[1:3, 1:4],
  res_pad = chi_tbl9b$stdres
)


# ------------------------------------------------------------
# Organização das tabelas
# 
# Reordenar colunas por jogador e remover totais da matriz
# de valores esperados.
# ------------------------------------------------------------

# Reordena colunas na estrutura P1, P2 e soma.
tabela_9a <- order_table_p1_p2(tabela_9a)
tabela_9b <- order_table_p1_p2(tabela_9b)

# Remove linha e coluna "Sum" da matriz de valores esperados.
tabela_9a <- drop_sum_esp(tabela_9a)
tabela_9b <- drop_sum_esp(tabela_9b)


# ------------------------------------------------------------
# Visualização rápida das tabelas
# 
# Exibir no console as probabilidades, frequências observadas,
# frequências esperadas e desvios em diferentes escalas.
# ------------------------------------------------------------

# Junta as tabelas dos dois jogos em uma lista nomeada.
tables_9 <- list(
  "Jogo A (BSG)" = tabela_9a,
  "Jogo B (MEG)" = tabela_9b
)

# Imprime as tabelas no console.
purrr::iwalk(tables_9, print_table)

# ------------------------------------------------------------
# Cálculo das proporções por simulação
# 
# Gerar as bases de proporções para o BSG e o MEG antes da análise
# de estabilidade.
# ------------------------------------------------------------

# Calcula as proporções de escolha das estratégias no BSG.
prop_bsg <- get_props(bsg_df)

# Calcula as proporções de escolha das estratégias no MEG.
prop_meg <- get_props(meg_df)


# ------------------------------------------------------------
# Definição dos limiares de estabilidade
# 
# Testar a sensibilidade dos resultados para diferentes valores
# de variação máxima permitida entre períodos.
# ------------------------------------------------------------

epslons <- seq(0.01, 0.05, by = 0.01)


# ------------------------------------------------------------
# Análise de estabilidade para todos os valores de epsilon e lambda
# 
# Aplicar a função get_stability() às 10 simulações para cada
# limiar de estabilidade.
# ------------------------------------------------------------

estabilidades_eps <- purrr::map(
  
  # Cria lista nomeada:
  # eps_0.01, eps_0.02, ..., eps_0.05
  rlang::set_names(epslons, paste0("eps_", epslons)),
  
  ~ purrr::map(
    
    # Cria sequência nomeada:
    # lambda_0.1, lambda_0.2, ..., lambda_1
    rlang::set_names(1:10, paste0("lambda_", seq(0.1, 1, by = 0.1))),
    
    function(i) {
      get_stability(
        
        # Seleciona a base do BSG correspondente à simulação atual.
        bsg = prop_bsg[[paste0("sim", i)]],
        
        # Seleciona a base do MEG correspondente à simulação atual.
        meg = prop_meg[[paste0("sim", i)]],
        
        # Parâmetros da análise de estabilidade.
        limiar = .x,
        janela = 10,
        min_estavel = 0.8
      )
    }
  )
)

estabilidades_eps$eps_0.01$lambda_0.1
estabilidades_eps$eps_0.03$lambda_0.5
estabilidades_eps$eps_0.05$lambda_1

purrr::map(estabilidades_eps$eps_0.01, ~ .x$resumo)
purrr::map(estabilidades_eps$eps_0.02, ~ .x$resumo)
purrr::map(estabilidades_eps$eps_0.03, ~ .x$resumo)
purrr::map(estabilidades_eps$eps_0.04, ~ .x$resumo)
purrr::map(estabilidades_eps$eps_0.05, ~ .x$resumo)

# ------------------------------------------------------------
# Seleção do limiar principal adotado no trabalho
# 
# O limiar de 0.03 é usado como referência principal da análise.
# Os demais valores servem para análise de sensibilidade.
# ------------------------------------------------------------

estabilidades <- estabilidades_eps$eps_0.03


# ------------------------------------------------------------
# Visualização dos resumos de estabilidade
# ------------------------------------------------------------

lapply(estabilidades, function(x) x$resumo)


# ------------------------------------------------------------
# Extração dos resumos em uma lista nomeada
# ------------------------------------------------------------

props_listas <- setNames(
  lapply(estabilidades, function(x) x$resumo),
  names(estabilidades)
)


# ------------------------------------------------------------
# Seleção de colunas principais
# ------------------------------------------------------------

props <- props_listas |> 
  purrr::map(~ .x |> 
        dplyr::select(
          jogo:estrategia_escolhida,
          periodo_estabilidade,
          prop_media_estavel
        )
      )


# ------------------------------------------------------------
# Seleção de informações sobre estabilidade temporal
# ------------------------------------------------------------

purrr::map(estabilidades, ~ dplyr::select(
  .x$resumo, 
  jogo, 
  modelo, 
  jogador, 
  periodo_estabilidade, 
  variacao_media_estavel
))

# ------------------------------------------------------------
# Função para construir tabela de estabilidade
# ------------------------------------------------------------

build_stab_tab <- function(props_listas) {
  
  props_listas |>
    purrr::imap_dfr(~ {
      .x |>
        dplyr::mutate(
          lambda = .y,
          lambda = stringr::str_remove(lambda, "lambda_"),
          lambda = stringr::str_replace(lambda, "\\.", ","),
          lambda = paste0("λ = ", lambda),
          type = modelo,
          media_prob = taxa_estabilidade,
          prop_n = taxa_estabilidade * 100,
          estrategia_escolhida = jogador
        )
    }) |>
    dplyr::select(
      jogador = jogo,                  
      type,
      lambda,
      estrategia_escolhida,
      media_prob,
      prop_n
    )
}


# ------------------------------------------------------------
# Tabela principal de estabilidade
# 
# Esta tabela usa o limiar principal epsilon = 0.03.
# ------------------------------------------------------------

tab_3 <- build_stab_tab(props_listas)


# ------------------------------------------------------------
# Tabelas de sensibilidade
# 
# Exclui epsilon = 0.03, pois ele já corresponde à tab_3.
# ------------------------------------------------------------

tabelas_sensibilidade <- estabilidades_eps[names(estabilidades_eps) != "eps_0.03"] |>
  purrr::map(~ {
    
    props_listas_eps <- purrr::map(
      .x,
      ~ .x$resumo
    )
    
    build_stab_tab(props_listas_eps)
  })


# ------------------------------------------------------------
# Tabela longa de sensibilidade
# 
# Junta epsilon = 0.01, 0.02, 0.04 e 0.05 em uma única tabela.
# ------------------------------------------------------------

tab_sensibilidade <- estabilidades_eps[names(estabilidades_eps) != "eps_0.03"] |>
  purrr::imap_dfr(~ {
    
    props_listas_eps <- purrr::map(
      .x,
      ~ .x$resumo
    )
    
    build_stab_tab(props_listas_eps) |>
      dplyr::mutate(
        epsilon = .y,
        epsilon = stringr::str_remove(epsilon, "eps_"),
        epsilon = stringr::str_replace(epsilon, "\\.", ","),
        epsilon = paste0("ε = ", epsilon),
        .before = lambda
      )
  })

#--------------------------------------------------
# Definição dos pares estratégicos comparados
#--------------------------------------------------
# Nesta lista são especificados os dois pares de comparação entre os jogos:
#
# p1:
#   - BSG: Comprador escolhendo "Rejeitar"
#   - MEG: Empresa A escolhendo "Entrar"
#
# p2:
#   - BSG: Vendedor escolhendo "Preço Alto"
#   - MEG: Empresa B escolhendo "Entrar"
#
# A ideia é comparar distribuições de probabilidade entre papéis
# estrategicamente análogos, considerando estratégias associadas
# ao equilíbrio teórico dos jogos.
pares <- list(
  p1 = list(
    comparacao = "Comprador (BSG) vs Empresa A (MEG)",
    jogador_bsg = "Comprador",
    estrategia_bsg = "Rejeitar",
    jogador_meg = "Empresa A",
    estrategia_meg = "Entrar"
  ),
  p2 = list(
    comparacao = "Vendedor (BSG) vs Empresa B (MEG)",
    jogador_bsg = "Vendedor",
    estrategia_bsg = "Preço Alto",
    jogador_meg = "Empresa B",
    estrategia_meg = "Entrar"
  )
)

#--------------------------------------------------
# Construção das bases de probabilidades
#--------------------------------------------------
# Para cada par estratégico definido acima, esta etapa:
#   - filtra os dados relevantes no BSG e no MEG
#   - organiza as probabilidades de escolha
#   - padroniza a estrutura para comparação direta
#
# O resultado é uma lista com dois elementos:
#   - probs_lista$p1
#   - probs_lista$p2
probs_lista <- purrr::imap(
  pares,
  ~ build_probs_compare(
    bsg_df = bsg_df,
    meg_df = meg_df,
    jogador_bsg = .x$jogador_bsg,
    estrategia_bsg = .x$estrategia_bsg,
    jogador_meg = .x$jogador_meg,
    estrategia_meg = .x$estrategia_meg,
    par_label = .y
  )
)

# ------------------------------------------------------------
# Preparação dos dados para os gráficos ECDF
# Prepara uma versão amostrada e organizada das bases de
# probabilidades para construção das curvas ECDF.
# ------------------------------------------------------------

set.seed(05052026)

# Reduz a quantidade de observações por grupo para deixar
# os gráficos mais leves e legíveis.
probs_plot_lista <- map(
  probs_lista,
  ~ prep_ecdf(.x, n_por_grupo = 200000)
)

# Acesso opcional às bases preparadas:
# probs_plot_lista$p1
# probs_plot_lista$p2


# ------------------------------------------------------------
# Construção dos painéis ECDF do par 1
# Gera os painéis (a1), (a2) e (a3), comparando BSG e MEG
# para o primeiro par estratégico.
# ------------------------------------------------------------

comparacao_p1 <- make_ecdf_subpatch(
  df_probs = probs_lista$p1,
  prefixo = "a",
  pal_jogo = c(BSG = color_main[1], MEG = color_main[2]),
  fonte_base = fonte_base,
  base_size = 16,
  n_por_grupo = 200000,
  preparar = TRUE
)


# ------------------------------------------------------------
# Construção dos painéis ECDF do par 2
# Gera os painéis (b1), (b2) e (b3), comparando BSG e MEG
# para o segundo par estratégico.
# ------------------------------------------------------------

comparacao_p2 <- make_ecdf_subpatch(
  df_probs = probs_lista$p2,
  prefixo = "b",
  pal_jogo = c(BSG = color_main[1], MEG = color_main[2]),
  fonte_base = fonte_base,
  base_size = 16,
  n_por_grupo = 200000,
  preparar = TRUE
)


# ------------------------------------------------------------
# Distribuição das probabilidades por intervalos
#
# Classificar as probabilidades dos pares estratégicos em faixas
# de amplitude 0,1 e calcular a participação de cada faixa dentro
# de cada combinação de modelo, jogo, jogador e estratégia.
# ------------------------------------------------------------


# ------------------------------------------------------------
# Distribuição das probabilidades do par 1
#
# O primeiro par compara:
# - Comprador no BSG, com a estratégia Rejeitar;
# - Empresa A no MEG, com a estratégia Entrar.
#
# As probabilidades são distribuídas em dez intervalos entre
# 0 e 1. Em seguida, calcula-se a frequência absoluta e relativa
# de observações em cada faixa.
# ------------------------------------------------------------

probs_plot_lista$p1 |>
  dplyr::mutate(
    
    # Divide as probabilidades em intervalos de amplitude 0,1.
    faixa_prob = base::cut(
      prob,
      breaks = base::seq(0, 1, by = 0.1),
      include.lowest = TRUE
    )
  ) |>
  
  # Conta as observações em cada faixa de probabilidade.
  dplyr::count(
    modelo,
    jogo,
    jogador,
    estrategia,
    faixa_prob
  ) |>
  
  # Define os grupos usados no cálculo das proporções.
  dplyr::group_by(
    modelo,
    jogo,
    jogador,
    estrategia
  ) |>
  
  # Calcula a frequência relativa e o percentual de cada faixa.
  dplyr::mutate(
    prop = n / base::sum(n),
    prop_pct = prop * 100
  ) |>
  
  # Remove o agrupamento da tabela final.
  dplyr::ungroup() |>
  
  # Exibe até 60 linhas no console.
  print(n = 60)


# ------------------------------------------------------------
# Distribuição das probabilidades do par 2
#
# O segundo par compara:
# - Vendedor no BSG, com a estratégia Preço Alto;
# - Empresa B no MEG, com a estratégia Entrar.
#
# A estrutura do cálculo é igual à aplicada ao primeiro par.
# ------------------------------------------------------------

probs_plot_lista$p2 |>
  dplyr::mutate(
    
    # Divide as probabilidades em intervalos de amplitude 0,1.
    faixa_prob = base::cut(
      prob,
      breaks = base::seq(0, 1, by = 0.1),
      include.lowest = TRUE
    )
  ) |>
  
  # Conta as observações em cada faixa de probabilidade.
  dplyr::count(
    modelo,
    jogo,
    jogador,
    estrategia,
    faixa_prob
  ) |>
  
  # Define os grupos usados no cálculo das proporções.
  dplyr::group_by(
    modelo,
    jogo,
    jogador,
    estrategia
  ) |>
  
  # Calcula a frequência relativa e o percentual de cada faixa.
  dplyr::mutate(
    prop = n / base::sum(n),
    prop_pct = prop * 100
  ) |>
  
  # Remove o agrupamento da tabela final.
  dplyr::ungroup() |>
  
  # Exibe até 60 linhas no console.
  print(n = 60)


# ------------------------------------------------------------
# Classificação ampliada das probabilidades do par 1
#
# Reúne as probabilidades em quatro categorias interpretativas:
# baixa, média-baixa, média-alta e alta. A classificação facilita
# a leitura da concentração das probabilidades em partes mais
# amplas do intervalo entre 0 e 1.
# ------------------------------------------------------------

comparacao_p1$dados_plot |>
  dplyr::mutate(
    
    # Classifica as probabilidades em quatro faixas.
    faixa_prob = dplyr::case_when(
      prob < 0.25                    ~ "Baixa (< 0,25)",
      prob >= 0.25 & prob < 0.50    ~ "Média-baixa (0,25–0,50)",
      prob >= 0.50 & prob < 0.75    ~ "Média-alta (0,50–0,75)",
      prob >= 0.75                   ~ "Alta (≥ 0,75)",
      TRUE                           ~ NA_character_
    )
  ) |>
  
  # Conta as observações em cada categoria.
  dplyr::count(
    modelo,
    jogo,
    jogador,
    estrategia,
    faixa_prob
  ) |>
  
  # Define os grupos usados no cálculo das proporções.
  dplyr::group_by(
    modelo,
    jogo,
    jogador,
    estrategia
  ) |>
  
  # Calcula a participação de cada faixa no respectivo grupo.
  dplyr::mutate(
    prop_faixa = n / base::sum(n),
    prop_faixa_pct = prop_faixa * 100
  ) |>
  
  # Remove o agrupamento da tabela final.
  dplyr::ungroup()

#--------------------------------------------------
# Teste KS principal entre jogos
#--------------------------------------------------
# Aplica o teste de Kolmogorov-Smirnov para comparar as distribuições
# entre BSG e MEG, agregando os resultados por modelo.
ks_results_all <- purrr::imap_dfr(
  probs_lista,
  ~ run_ks_by_model(
    .x,
    comparacao_label = pares[[.y]]$comparacao
  )
)

ks_results_all


#--------------------------------------------------
# Testes KS entre pares de modelos dentro de cada jogo
#--------------------------------------------------
# Para cada par estratégico:
#   - compara EWA vs RL, EWA vs BL e RL vs BL
#   - separadamente para BSG e MEG
#   - para cada valor de lambda
#
# Também aplica correção de Bonferroni para múltiplos testes.
ks_lambda_all <- purrr::imap_dfr(
  probs_lista,
  function(probs_par, nome_par) {
    
    bind_rows(
      run_ks_all_pairs_lambda(probs_par, "BSG"),
      run_ks_all_pairs_lambda(probs_par, "MEG")
    ) %>%
      mutate(
        comparacao_par = pares[[nome_par]]$comparacao,
        p_value_adj = p.adjust(p_value, method = "bonferroni")
      )
  }
)

ks_lambda_all |> 
  print(n=nrow(ks_lambda_all))

probs_lista$p2 |>
  dplyr::filter(modelo == "RL") |>
  dplyr::group_by(jogo) |>
  dplyr::summarise(
    n = dplyr::n(),
    media = mean(prob),
    mediana = median(prob),
    prop_0_01 = mean(prob <= 0.01),
    prop_099_1 = mean(prob >= 0.99),
    prop_0_01_a_01 = mean(prob > 0.01 & prob <= 0.1),
    prop_09_a_099 = mean(prob >= 0.9 & prob < 0.99)
  )

#--------------------------------------------------
# Visualização dos resultados KS por lambda
#--------------------------------------------------
# Gera gráfico com a evolução das distâncias KS ao longo de lambda.
p16 <- plot_ks_lambda(
  ks_lambda_all = ks_lambda_all,
  fonte_base = fonte_base
)

p16


#--------------------------------------------------
# Resumo das distâncias KS por lambda
#--------------------------------------------------
# Consolida as estatísticas KS em formato resumido para análise.
resumo_ks_lambda <- summarise_ks_lambda(ks_lambda_all)

resumo_ks_lambda


#--------------------------------------------------
# Simulações para análise de convergência
#--------------------------------------------------
# Aqui são definidos os nomes dos objetos de simulação:
# sim1, sim2, ..., sim10
#
# Cada simulação está associada a um valor de lambda.
sim_nomes <- paste0("sim", 1:10)

# Para cada simulação e seu respectivo lambda:
#   - calcula-se o resumo de convergência
#   - calcula-se a trajetória temporal da distância ao alvo teórico
#
# A função sim_convergence retorna uma lista com:
#   - summary : métricas-resumo da convergência
#   - path    : trajetória temporal
convergencias <- purrr::map2(
  plots[sim_nomes],
  lambda_values,
  sim_convergence
)

# Junta todos os resumos em uma base única
conv_full <- purrr::map_dfr(convergencias, "summary")

# Junta todas as trajetórias em uma base única
conv_trajeto <- purrr::map_dfr(convergencias, "path")


#--------------------------------------------------
# Base agregada para o gráfico de convergência
#--------------------------------------------------
# A base conv_full é resumida por:
#   - jogo
#   - modelo
#   - lambda
#
# Aqui é calculado o erro médio para cada combinação.
# Em seguida, ajusta-se a ordem dos fatores para:
#   - modelos: EWA, RL, BL
#   - jogos: BSG, MEG
conv_plot <- conv_full |>
  dplyr::group_by(jogo, type, lambda) |>
  dplyr::summarise(
    erro_medio = mean(mean_error, na.rm = TRUE),
    erro_final = mean(final_error, na.rm = TRUE),
    erro_ultimos_10 = mean(last10_error, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    type = factor(type, levels = c("EWA", "RL", "BL")),
    jogo = factor(jogo, levels = c("BSG", "MEG"))
  )


#--------------------------------------------------
# Painel de convergência para o BSG
#--------------------------------------------------
# Gera o gráfico do jogo BSG com escala y específica.
# A função plot_base() aplica o padrão visual principal.
conv_m1 <- plot_base(
  conv_plot |> filter(jogo == "BSG"),
  "(a) BSG"
) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.01),
    limits = c(0.2, 0.55),
    breaks = seq(0.2, 0.55, .05)
  )


#--------------------------------------------------
# Painel de convergência para o MEG
#--------------------------------------------------
# Gera o gráfico do jogo MEG com escala y específica.
conv_m2 <- plot_base(
  conv_plot |> filter(jogo == "MEG"),
  "(b) MEG"
) +
  scale_y_continuous(
    labels = label_number(decimal.mark = ",", big.mark = ".", accuracy = 0.01),
    limits = c(0.15, 0.35)
  )


#--------------------------------------------------
# Figura final de convergência
#--------------------------------------------------
# Junta os dois gráficos em uma única figura:
#   (a) BSG
#   (b) MEG
#
# O argumento axis_titles = "collect" compartilha os títulos dos eixos.
p17 <- conv_m1 + conv_m2 +
  plot_layout(axis_titles = "collect")

p17


# ------------------------------------------------------------
# Exportação de figuras e tabelas
# Salva os gráficos e tabelas gerados ao longo da análise nos
# diretórios definidos do projeto.
# ------------------------------------------------------------
# Figuras
# Exporta as figuras principais e complementares em formato PDF.


# ------------------------------------------------------------
# Figuras 4 e 5 — Equilíbrios de Nash
# Salva os gráficos dos equilíbrios de Nash dos jogos BSG e MEG.
# ------------------------------------------------------------

# Figura 4 (BSG)
ggplot2::ggsave(
  plot = p4, filename = "figuras/figura_4.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)

# Figura 5 (MEG)
ggplot2::ggsave(
  plot = p5, filename = "figuras/figura_5.pdf",
  width = 10.81, height = 7.75, units = "in", device = cairo_pdf
)


# ------------------------------------------------------------
# Figuras 8 a 12 — Lambdas principais
# Exporta os gráficos das simulações para os valores principais
# de lambda: 0,1; 0,3; 0,5; 0,8; 1.
# ------------------------------------------------------------

# lambdas principais: figuras 8 a 12
sim_principais <- c(1, 3, 5, 8, 10)

fig_principais <- 8:12

purrr::walk2(sim_principais, fig_principais, \(i, fig) {
  ggplot2::ggsave(
    plot = plots[[base::paste0("sim", i)]]$plot,
    filename = base::paste0("figuras/figura_", fig, ".pdf"),
    width = 12.13, height = 9.02, units = "in",
    device = cairo_pdf
  )
})

# ------------------------------------------------------------
# Figuras 20 a 24 — Demais lambdas
# Exporta os gráficos das simulações para os demais valores:
# 0,2; 0,4; 0,6; 0,7; 0,9.
# ------------------------------------------------------------

# demais lambdas: 0,2; 0,4; 0,6; 0,7; 0,9
sim_outros <- c(2, 4, 6, 7, 9)

fig_outros <- 20:24

purrr::walk2(sim_outros, fig_outros, \(i, fig) {
  ggplot2::ggsave(
    plot = plots[[base::paste0("sim", i)]]$plot,
    filename = base::paste0("figuras/figura_", fig, ".pdf"),
    width = 12.13, height = 9.02, units = "in",
    device = cairo_pdf
  )
})


# ------------------------------------------------------------
# Figuras 13 e 14 — Distribuição das probabilidades
# Salva os histogramas das probabilidades simuladas para BSG e MEG.
# ------------------------------------------------------------

# Figuras 13 e 14 (análise da distribuição dos dados -- histograma)

plot_histogram(
  data = bsg_df,
  bins = grDevices::nclass.Sturges(bsg_df$probabilidade),
  file_name = "figuras/figura_13.pdf"
)

plot_histogram(
  data = meg_df,
  bins = grDevices::nclass.Sturges(meg_df$probabilidade),
  file_name = "figuras/figura_14.pdf"
)


# ------------------------------------------------------------
# Figura 15 — Curvas ECDF
# Salva a figura consolidada das distribuições acumuladas empíricas.
# ------------------------------------------------------------

# Salva a figura consolidada no diretório "figuras".
ggplot2::ggsave(
  filename = "figuras/figura_15.pdf",
  plot = p15,
  width = 12.13, 
  height = 9.02,
  units = "in",
  device = cairo_pdf
)


# ------------------------------------------------------------
# Figura 16 — Distâncias KS por lambda
# Salva o gráfico das distâncias de Kolmogorov-Smirnov ao longo
# dos valores de lambda.
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = "figuras/figura_16.pdf",
  plot = p16,
  width = 10.81, 
  height = 7.75,
  units = "in",
  device = cairo_pdf
)


# ------------------------------------------------------------
# Figura 17 — Convergência ao alvo teórico
# Salva o gráfico da distância média ao equilíbrio teórico.
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = "figuras/figura_17.pdf",
  plot = p17,
  width = 10.81, 
  height = 7.75,
  units = "in",
  device = cairo_pdf
)


# ------------------------------------------------------------
# Figura 18 — Equilíbrios de Nash no BSG
# Figura complementar destinada ao Apêndice A.
# ------------------------------------------------------------

# Figura 18 (compilação dos Equilíbrios de Nash no BSG) -- APÊNDICE A
ggplot2::ggsave(plot = p18,
                filename = "figuras/figura_18.pdf",
                width = 10.81, height = 7.75, units = "in",
                device = cairo_pdf)


# ------------------------------------------------------------
# Figura 19 — Tabelas de contingência
# Salva a visualização combinada das tabelas de contingência.
# ------------------------------------------------------------

# Figura 19 (plotagem dos dados da tabela 9 de contigência)

ggplot2::ggsave(plot = contigencia$plots$combined_plot,
                filename = "figuras/figura_19.pdf",
                width = 14, height = 9.02, units = "in",
                device = cairo_pdf)


# ------------------------------------------------------------
# Tabelas
# Geração, exportação e formatação das tabelas utilizadas no TCC,
# incluindo matrizes dos jogos, medidas-resumo e probabilidades.
# ------------------------------------------------------------


# ------------------------------------------------------------
# Tabelas 1 e 2 — Matrizes dos jogos
# Exporta as matrizes de payoff do BSG e MEG em formato PDF.
# ------------------------------------------------------------

export_game_table(
  game_matrix = matriz_bsg,
  title = "Tabela 1: Matriz de ganhos do jogo BSG",
  file = "tabelas/tabela_1.pdf",
  height = 7,
  width = 10,
  overwrite = TRUE
)

export_game_table(
  game_matrix = matriz_meg,
  title = "Tabela 2: Matriz de ganhos do jogo MEG",
  file = "tabelas/tabela_2.pdf",
  height = 7,
  width = 10,
  overwrite = TRUE
)


# ------------------------------------------------------------
# Tabela 3 — Taxa de estabilidade
# Gera tabela em PNG e versão LaTeX com base nos resultados
# de estabilidade para BSG e MEG.
# ------------------------------------------------------------

avg_prob_table(
  data      = tab_3,
  file      = "tabelas/tabela_3.png",
  p1_player = "BSG",
  p1_pair   = c("Comprador", "Vendedor"),
  p2_player = "MEG",
  p2_pair   = c("Empresa A", "Empresa B"),
  title     = "Tabela 3: Taxa de estabilidade em porcentagem nos jogos BSG e MEG por modelo"
)

avg_prob_tabletex(
  data      = tab_3,
  p1_player = "BSG",
  p1_pair   = c("Comprador", "Vendedor"),
  p2_player = "MEG",
  p2_pair   = c("Empresa A", "Empresa B"),
  p1_label  = "P1",
  p2_label  = "P2",
  caption   = "Taxa de estabilidade em porcentagem nos jogos BSG e MEG por modelo",
  label     = "tabela_3",
  font_size = 10,
  file_tex  = "tabelas/tabela_3.tex"
)


# ------------------------------------------------------------
# Tabelas 4 a 6 — Medidas-resumo por modelo
# Exporta estatísticas descritivas das atrações para EWA, RL e BL.
# ------------------------------------------------------------

# EWA
model_stats_table(
  data   = resumo_dados$tab_4,
  file   = "tabelas/tabela_4.png",
  modelo = "EWA",
  title  = "Tabela 4: Medidas-resumo das atrações no modelo EWA para variações de λ entre 0,1 e 1"
)

model_stats_tabletex(
  data     = resumo_dados$tab_4,
  modelo   = "EWA",
  caption  = "Medidas-resumo das atrações no modelo EWA para variações de $\\lambda$ entre 0,1 e 1",
  label    = "tabela_4",
  file_tex = "tabelas/tabela_4.tex"
)

# RL
model_stats_table(
  data   = resumo_dados$tab_5,
  file   = "tabelas/tabela_5.png",
  modelo = "RL",
  title  = "Tabela 5: Medidas-resumo das atrações no modelo RL para variações de λ entre 0,1 e 1"
)

model_stats_tabletex(
  data     = resumo_dados$tab_5,
  modelo   = "RL",
  caption  = "Medidas-resumo das atrações no modelo RL para variações de $\\lambda$ entre 0,1 e 1",
  label    = "tabela_5",
  file_tex = "tabelas/tabela_5.tex"
)

# BL
model_stats_table(
  data   = resumo_dados$tab_6,
  file   = "tabelas/tabela_6.png",
  modelo = "BL",
  title  = "Tabela 6: Medidas-resumo das atrações no modelo BL para variações de λ entre 0,1 e 1"
)

model_stats_tabletex(
  data     = resumo_dados$tab_6,
  modelo   = "BL",
  caption  = "Medidas-resumo das atrações no modelo BL para variações de $\\lambda$ entre 0,1 e 1",
  label    = "tabela_6",
  file_tex = "tabelas/tabela_6.tex"
)


# ------------------------------------------------------------
# Tabelas 7 e 8 — Probabilidades médias
# Exporta probabilidades médias por estratégia para BSG e MEG.
# ------------------------------------------------------------

# BSG
avg_prob_table(
  data      = resumo_dados$tab_7,
  file      = "tabelas/tabela_7.png",
  p1_player = "Vendedor",
  p1_pair   = c("Preço Alto", "Preço Baixo"),
  p2_player = "Comprador",
  p2_pair   = c("Aceitar", "Rejeitar"),
  title     = "Tabela 7: Probabilidades médias das estratégias no BSG segundo modelo de aprendizagem e λ"
)

avg_prob_tabletex(
  data      = resumo_dados$tab_7,
  p1_player = "Vendedor",
  p1_pair   = c("Preço Alto", "Preço Baixo"),
  p2_player = "Comprador",
  p2_pair   = c("Aceitar", "Rejeitar"),
  p1_label  = "P1",
  p2_label  = "P2",
  caption   = "Probabilidades médias das estratégias no BSG segundo modelo de aprendizagem e $\\lambda$",
  label     = "tabela_7",
  font_size = 10,
  file_tex  = "tabelas/tabela_7.tex"
)

# MEG
avg_prob_table(
  data      = resumo_dados$tab_8,
  file      = "tabelas/tabela_8.png",
  p1_player = "Empresa A",
  p1_pair   = c("Entrar", "Não Entrar"),
  p2_player = "Empresa B",
  p2_pair   = c("Entrar", "Não Entrar"),
  title     = "Tabela 8: Probabilidades médias das estratégias no MEG segundo modelo de aprendizagem e λ"
)

avg_prob_tabletex(
  data      = resumo_dados$tab_8,
  p1_player = "Empresa A",
  p1_pair   = c("Entrar", "Não Entrar"),
  p2_player = "Empresa B",
  p2_pair   = c("Entrar", "Não Entrar"),
  p1_label  = "P1",
  p2_label  = "P2",
  caption   = "Probabilidades médias das estratégias no MEG segundo modelo de aprendizagem e $\\lambda$",
  label     = "tabela_8",
  font_size = 10,
  file_tex  = "tabelas/tabela_8.tex"
)


# ------------------------------------------------------------
# Tabela 9 — Distribuição por estratégia e modelo
# Consolida probabilidades dos dois jogos em formato amplo.
# ------------------------------------------------------------

table_wide(
  prob_bsg = tabela_9a$prob,
  prob_meg = tabela_9b$prob,
  file     = "tabelas/tabela_9.png",
  title    = "Tabela 9: Distribuição percentual das estratégias nos jogos BSG e MEG segundo modelo de aprendizagem",
  digits   = 1
)

write_probtex(
  tabela_9a$prob,
  tabela_9b$prob,
  file    = "tabelas/tabela_9.tex",
  caption = "Distribuição percentual das estratégias nos jogos BSG e MEG segundo modelo de aprendizagem",
  label   = "tabela_9"
)


# ------------------------------------------------------------
# Tabela 10 — Frequências esperadas e resíduos padronizados
# Exporta tabela LaTeX com valores principais e resíduos entre parênteses.
# ------------------------------------------------------------

writeLines(
  tex_matrix(
    tabela_9a$esp / 1e6,
    tabela_9a$res_pad,
    tabela_9b$esp / 1e6,
    tabela_9b$res_pad,
    caption = "Frequências esperadas e resíduos padronizados do teste $\\chi^2$ nos jogos BSG e MEG",
    label   = "tabela_10",
    digits_main = 1,
    digits_paren = 1,
    decimal = ",",
    show_paren_in_total = FALSE
  ),
  "tabelas/tabela_10.tex"
)


# ------------------------------------------------------------
# Controle das tabelas de sensibilidade
# 
# Tabelas 11 a 14, excluindo epsilon = 0,03,
# pois este já corresponde à Tabela 3.
# ------------------------------------------------------------

controle_sensibilidade <- tibble::tibble(
  eps_nome   = c("eps_0.01", "eps_0.02", "eps_0.04", "eps_0.05"),
  eps_label  = c("0,01", "0,02", "0,04", "0,05"),
  num_tabela = 11:14
)


# ------------------------------------------------------------
# Tabelas 11 a 14 — Sensibilidade do critério de estabilidade
# Exportação das tabelas de sensibilidade em PNG.
# ------------------------------------------------------------

purrr::pwalk(
  controle_sensibilidade,
  function(eps_nome, eps_label, num_tabela) {
    
    avg_prob_table(
      data      = tabelas_sensibilidade[[eps_nome]],
      file      = paste0("tabelas/tabela_", num_tabela, ".png"),
      p1_player = "BSG",
      p1_pair   = c("Comprador", "Vendedor"),
      p2_player = "MEG",
      p2_pair   = c("Empresa A", "Empresa B"),
      title     = paste0(
        "Tabela ", num_tabela,
        ": Taxa de estabilidade nos jogos BSG e MEG com ε = ",
        eps_label
      )
    )
  }
)


# ------------------------------------------------------------
# Tabelas 11 a 14 — Sensibilidade do critério de estabilidade
# Exportação das tabelas de sensibilidade em LaTeX.
# ------------------------------------------------------------

purrr::pwalk(
  controle_sensibilidade,
  function(eps_nome, eps_label, num_tabela) {
    
    avg_prob_tabletex(
      data      = tabelas_sensibilidade[[eps_nome]],
      p1_player = "BSG",
      p1_pair   = c("Comprador", "Vendedor"),
      p2_player = "MEG",
      p2_pair   = c("Empresa A", "Empresa B"),
      p1_label  = "P1",
      p2_label  = "P2",
      caption   = paste0(
        "Taxa de estabilidade nos jogos BSG e MEG com $\\epsilon = ",
        eps_label,
        "$"
      ),
      label     = paste0("tabela_", num_tabela),
      font_size = 10,
      file_tex  = paste0("tabelas/tabela_", num_tabela, ".tex")
    )
  }
)

# ------------------------------------------------------------
# Dashboard das simulações
# Estrutura geral do app Shiny usado para simular, visualizar e
# explorar os resultados dos modelos EWA, RL e BL nos jogos BSG e MEG.
# ------------------------------------------------------------


# ------------------------------------------------------------
# 1) Pacotes
# Carrega os pacotes necessários para interface, simulação,
# manipulação de dados, gráficos e exportação.
# ------------------------------------------------------------


# ------------------------------------------------------------
# 2) Objetos do projeto
# Define os objetos centrais usados pelo app, como matrizes dos jogos,
# paleta de cores, fonte base, valores de lambda e modelos de aprendizado.
# ------------------------------------------------------------


# ------------------------------------------------------------
# 3) Funções do projeto
# Carrega funções gerais do projeto usadas também fora do app.
# Exemplos:
#   - sim_lambda()
#   - build_simulation_df()
#   - data_extraction()
#   - player_render()
#   - remove_cols()
#   - sim_plot()
#   - demais funções auxiliares compartilhadas.
# ------------------------------------------------------------


# ------------------------------------------------------------
# 4) Helpers do app
# Define funções específicas do dashboard, usadas para organizar dados,
# padronizar saídas e gerar visualizações dentro da interface.
# Exemplos:
#   - order_strats()
#   - standardize_and_prop()
#   - plot_model()
#   - run_sim()
# ------------------------------------------------------------


# ------------------------------------------------------------
# 5) Interface do usuário (UI)
# Define a estrutura visual do dashboard, incluindo página principal,
# barra lateral, cards, botões, inputs e áreas de saída.
# Exemplos:
#   - page_fluid()
#   - layout_sidebar()
#   - card()
#   - plotOutput()
#   - reactableOutput()
# ------------------------------------------------------------


# ------------------------------------------------------------
# 6) Servidor
# Define a lógica reativa do app: leitura dos inputs, execução das
# simulações, geração dos gráficos, tabelas e downloads.
# ------------------------------------------------------------

# server <- function(input, output, session) {
#   ...
# }


# ------------------------------------------------------------
# 7) Execução do aplicativo
# Inicializa o dashboard a partir dos objetos ui e server.
# ------------------------------------------------------------

# shinyApp(ui, server)