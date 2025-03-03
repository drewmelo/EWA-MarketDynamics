### ============================ PACOTES =====================================

## Lista de pacotes necessários --------------------------------------------

pacotes <- c(
  # Pacotes do universo tidy
  "tidyverse",
  # Adicionais para manipulação de dados
  "janitor",
  # Customização do plot
  "scales",
  "patchwork",
  "paletteer",
  # Temas para gráficos
  "beautyxtrar",
  # Fontes e tabelas
  "extrafont",
  "kableExtra",
  "webshot",
  "flextable",
  "grid",
  # Teoria dos Jogos
  "rgamer"
)

# Verificar quais pacotes já estão instalados
pacotes_instalados <- pacotes %in% base::rownames(utils::installed.packages())

## Instalar pacotes que não estão instalados ----------------------------------

# Instalar pacotes do CRAN que não estão instalados
if (base::any(!pacotes_instalados & pacotes != "beautyxtrar")) {
  utils::install.packages(pacotes[!pacotes_instalados & pacotes != "beautyxtrar"])
}

# Instalar pacote beautyxtrar do GitHub se não estiver instalado
if (!"beautyxtrar" %in% base::rownames(utils::installed.packages())) {
  if (!base::requireNamespace("devtools", quietly = T)) {
    utils::install.packages("devtools")
  }
  devtools::install_github("drewmelo/beautyxtrar", force = T)
}

# Função para carregar pacotes silenciosamente e verificar carregamento
carregar_pacote <- function(pacote) {
  base::suppressPackageStartupMessages({
    base::suppressMessages({
      base::suppressWarnings({
        base::library(pacote, character.only = T)
      })
    })
  })
}

# Carregar todos os pacotes e verificar sucesso
pacotes_carregados <- base::sapply(pacotes, function(pacote) {
  carregar_pacote(pacote)
  pacote %in% base::.packages()
})

# Imprimir resultado do carregamento
if (base::all(pacotes_carregados)) {
  base::print("Carregamento dos pacotes concluído com sucesso.")
} else {
  pacotes_falha <- pacotes[!pacotes_carregados]
  mensagem_falha <- base::paste("Falha ao carregar os seguintes pacotes:",
                                base::paste(pacotes_falha, collapse = ", "))
  base::print(mensagem_falha)
}

# Carregando fontes do dispositivo
extrafont::loadfonts(device = "win", quiet = T)

# Verificar se a fonte "Times New Roman" está disponível
if ("Times New Roman" %in% extrafont::fonts()) {
  base::print("A fonte 'Times New Roman' foi carregada com sucesso.")
} else {
  base::print("A fonte 'Times New Roman' não está disponível.")
}
