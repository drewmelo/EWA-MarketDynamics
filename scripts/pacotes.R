# ===== PACOTES (instalação opcional + carregamento) =====

# Instalar pacotes em runtime?
# Localmente deixe TRUE; em deploy (shinyapps.io/Connect) use FALSE e confie em renv.
INSTALL_MISSING <- interactive()

quiet_lib <- function(pkg) {
  suppressPackageStartupMessages(suppressWarnings(require(pkg, character.only = TRUE)))
}

install_if_missing <- function(pkgs) {
  if (!INSTALL_MISSING) return(invisible())
  miss <- pkgs[!(pkgs %in% rownames(installed.packages()))]
  if (length(miss)) {
    message("Instalando do CRAN: ", paste(miss, collapse = ", "))
    install.packages(miss)
  }
}

# Pacotes base do app (já usados no código)
base_pkgs <- c("shiny","bslib","ggplot2","dplyr","scales","tibble","withr","readr")
invisible(lapply(base_pkgs, quiet_lib))

# lista “completa”
extra_pkgs <- c(
  # tidyverse e data wrangling
  "tidyverse", "janitor", "glue",
  # visualização e layout
  "patchwork", "paletteer", "ggtext",
  # temas e fontes
  "beautyxtrar", "extrafont", "showtext", "sysfonts",
  # tabelas e exportação
  "kableExtra", "webshot", "webshot2", "flextable", "magick", "grid",
  # shiny e frontend
  "shiny", "bslib",
  # teoria dos jogos
  "rgamer"
)


# Instala os CRAN (exceto beautyxtrar, que é GitHub)
install_if_missing(setdiff(extra_pkgs, "beautyxtrar"))

# beautyxtrar via GitHub se faltar
if (!"beautyxtrar" %in% rownames(installed.packages())) {
  if (INSTALL_MISSING) {
    if (!quiet_lib("remotes")) install.packages("remotes")
    remotes::install_github("drewmelo/beautyxtrar", upgrade = "never")
  } else {
    message("Pacote 'beautyxtrar' ausente. Em produção, use renv para pré-instalar.")
  }
}

# Carrega todos os extras (silencioso)
invisible(lapply(extra_pkgs, quiet_lib))

# ===== FONTES (timesnewroman via showtext/sysfonts) =====
# Coloque seus .ttf na pasta do app (ex.: "assets/fontes/...").
# Em Shiny, o caminho é relativo ao diretório do app.
add_font_safe <- function(family, regular, bold = NULL, italic = NULL, bolditalic = NULL) {
  ok <- file.exists(regular)
  if (!ok) {
    message("Fonte não encontrada em: ", regular)
    return(invisible(FALSE))
  }
  sysfonts::font_add(family, regular = regular, bold = bold, italic = italic, bolditalic = bolditalic)
  showtext::showtext_auto(enable = TRUE)
  TRUE
}

font_ok <- add_font_safe(
  family = "timesnewroman",
  regular    = file.path("assets","fontes","timesnewroman-regular.ttf"),
  bold       = file.path("assets","fontes","timesnewroman-bold.ttf"),
  italic     = file.path("assets","fontes","timesnewroman-italic.ttf"),
  bolditalic = file.path("assets","fontes","timesnewroman-bolditalic.ttf")
)

if (font_ok) {
  message("✔️ A fonte 'timesnewroman' foi registrada via showtext.")
} else {
  message("❌ A fonte 'timesnewroman' não foi registrada (arquivos ausentes?).")
}

# (opcional) listar famílias registradas
# print(sysfonts::font_families())
