# ------------------------------------------------------------
# Parâmetro de custo
# ------------------------------------------------------------

# Custo do bem para o vendedor.
c <- 25


# ------------------------------------------------------------
# Função de payoff do vendedor
#
# O vendedor recebe a diferença entre o preço escolhido e o
# custo quando o comprador aceita a oferta. Em caso de rejeição,
# o payoff é igual a zero.
# ------------------------------------------------------------

func_payoff1 <- function(preco_vendedor, estrategia_comprador) {
  
  payoff <- 0
  
  if (estrategia_comprador == "Aceitar") {
    
    if (preco_vendedor == 45 || preco_vendedor == "Preço Alto")
      p <- 45
    
    else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo")
      p <- 35
    
    else
      p <- 0
    
    payoff <- p - c
  }
  
  payoff
}


# ------------------------------------------------------------
# Função de payoff do comprador
#
# A valoração do comprador é sorteada em cada execução. O limite
# inferior corresponde ao custo acrescido de um valor inteiro
# entre 1 e 10, enquanto o limite superior é igual a 45.
#
# Quando a oferta é aceita, o payoff corresponde à diferença
# entre a valoração sorteada e o preço cobrado. Em caso de
# rejeição, o payoff é igual a zero.
# ------------------------------------------------------------

func_payoff2 <- function(preco_vendedor, estrategia_comprador) {
  
  v_min <- c + base::sample(1:10, 1)
  v_max <- 45
  
  v <- stats::runif(
    1,
    min = v_min,
    max = v_max
  )
  
  payoff <- 0
  
  if (estrategia_comprador == "Aceitar") {
    
    if (preco_vendedor == 45 || preco_vendedor == "Preço Alto")
      p <- 45
    
    else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo")
      p <- 35
    
    else
      p <- 0
    
    payoff <- base::round(v - p, 2)
  }
  
  payoff
}


# ------------------------------------------------------------
# Reprodutibilidade
# ------------------------------------------------------------

base::set.seed(06-08-2024)


# ------------------------------------------------------------
# Simulação de Monte Carlo
#
# Calcula os payoffs médios das quatro combinações de estratégias
# do BSG por meio de 100 mil repetições em cada célula.
# ------------------------------------------------------------

N <- 100000

# Estratégias do vendedor.
s1 <- c("Preço Alto", "Preço Baixo")

# Estratégias do comprador.
s2 <- c("Aceitar", "Rejeitar")

# Cria todas as combinações entre as estratégias dos jogadores.
grid <- expand.grid(
  preco_vendedor = s1,
  estrategia_comprador = s2,
  stringsAsFactors = FALSE
)

# Calcula a média do payoff de uma função para uma combinação
# específica de estratégias.
mc_mean <- function(f, preco, estr, N) {
  mean(replicate(N, f(preco, estr)))
}

# Calcula o payoff médio do vendedor em cada célula da matriz.
grid$mean1 <- mapply(
  function(p, e) mc_mean(func_payoff1, p, e, N),
  grid$preco_vendedor,
  grid$estrategia_comprador
)

# Calcula o payoff médio do comprador em cada célula da matriz.
grid$mean2 <- mapply(
  function(p, e) mc_mean(func_payoff2, p, e, N),
  grid$preco_vendedor,
  grid$estrategia_comprador
)


# ------------------------------------------------------------
# Funções de consulta dos payoffs médios
#
# As funções localizam, na grade de combinações, o payoff médio
# correspondente às estratégias informadas. Esses valores são
# utilizados posteriormente na construção da matriz do BSG.
# ------------------------------------------------------------

# Retorna o payoff médio do vendedor.
pay1_avg <- function(preco_vendedor, estrategia_comprador) {
  
  p <- as.character(preco_vendedor)
  e <- as.character(estrategia_comprador)
  
  grid$mean1[
    match(
      paste(p, e),
      paste(
        grid$preco_vendedor,
        grid$estrategia_comprador
      )
    )
  ]
}

# Retorna o payoff médio do comprador.
pay2_avg <- function(preco_vendedor, estrategia_comprador) {
  
  p <- as.character(preco_vendedor)
  e <- as.character(estrategia_comprador)
  
  grid$mean2[
    match(
      paste(p, e),
      paste(
        grid$preco_vendedor,
        grid$estrategia_comprador
      )
    )
  ]
}