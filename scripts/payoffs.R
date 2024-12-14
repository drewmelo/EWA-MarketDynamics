## PAYOFFS DA MATRIZ 1 -----------------------------------------------------

# Definir o custo de produção globalmente
c <- 25

# Função para o payoff do Vendedor
func_payoff1 <- function(preco_vendedor, estrategia_comprador) {

  # Inicializar a variável de payoff
  payoff <- 0

  # Calcula o payoff do vendedor com base no preço e se o comprador aceitou a oferta
  if (estrategia_comprador == "Aceitar") {
    # Calcula o payoff do comprador com base no preço e se ele aceitou a oferta
    if (preco_vendedor == 45 || preco_vendedor == "Preço Alto") {
      p <- 45 # Preço Alto
    } else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo") {
      p <- 35  # Preço Baixo
    } else {
      p <- 0  # Preço não reconhecido
    }

    # Calcula o payoff para o vendedor se o comprador aceitar
    payoff <- p - c
  }

  return(payoff)
}



# Função para o payoff do Comprador
func_payoff2 <- function(preco_vendedor, estrategia_comprador) {
  # Definir um intervalo razoável para V
  v_min <- c + sample(1:10, 1) # Um valor mínimo superior ao custo
  v_max <- 45     # Um valor máximo razoável

  # Gerar um valor aleatório para o comprador dentro do intervalo
  v <- runif(1, min = v_min, max = v_max)

  # Inicializar a variável de payoff
  payoff <- 0

  if (estrategia_comprador == "Aceitar") {
    if (preco_vendedor == 45|| preco_vendedor == "Preço Alto") {
      p <- 45 # Preço Alto
    } else if (preco_vendedor == 35 || preco_vendedor == "Preço Baixo") {
      p <- 35  # Preço Baixo
    } else {
      p <- 0  # Preço não reconhecido
    }

    payoff <- round(v - p, 2)  # Comprador aceita o preço
  }

  return(payoff)
}
