c <- 25

# === suas funções originais ===
func_payoff1 <- function(preco_vendedor, estrategia_comprador) {
  payoff <- 0
  if (estrategia_comprador == "Aceitar") {
    if (preco_vendedor == 45 || preco_vendedor == "Preço Alto")      p <- 45
    else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo") p <- 35
    else p <- 0
    payoff <- p - c
  }
  payoff
}

func_payoff2 <- function(preco_vendedor, estrategia_comprador) {
  v_min <- c + base::sample(1:10, 1)
  v_max <- 45
  v <- stats::runif(1, min = v_min, max = v_max)
  payoff <- 0
  if (estrategia_comprador == "Aceitar") {
    if (preco_vendedor == 45 || preco_vendedor == "Preço Alto")      p <- 45
    else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo") p <- 35
    else p <- 0
    payoff <- base::round(v - p, 2)
  }
  payoff
}

base::set.seed(06-08-2024)

# === Monte Carlo para as 4 células e salvar as MÉDIAS ===
N <- 100000
s1 <- c("Preço Alto","Preço Baixo")   # vendedor
s2 <- c("Aceitar","Rejeitar")         # comprador

grid <- expand.grid(preco_vendedor = s1,
                    estrategia_comprador = s2,
                    stringsAsFactors = FALSE)

mc_mean <- function(f, preco, estr, N) {
  mean(replicate(N, f(preco, estr)))
}

grid$mean1 <- mapply(function(p,e) mc_mean(func_payoff1, p, e, N), 
                     grid$preco_vendedor, grid$estrategia_comprador)
grid$mean2 <- mapply(function(p,e) mc_mean(func_payoff2, p, e, N), 
                     grid$preco_vendedor, grid$estrategia_comprador)

# === funções "lookup" que devolvem a média por célula ===
pay1_avg <- function(preco_vendedor, estrategia_comprador) {
  p <- as.character(preco_vendedor); e <- as.character(estrategia_comprador)
  grid$mean1[match(paste(p,e), paste(grid$preco_vendedor, grid$estrategia_comprador))]
}
pay2_avg <- function(preco_vendedor, estrategia_comprador) {
  p <- as.character(preco_vendedor); e <- as.character(estrategia_comprador)
  grid$mean2[match(paste(p,e), paste(grid$preco_vendedor, grid$estrategia_comprador))]
}