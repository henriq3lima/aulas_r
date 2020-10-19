library(styler)
library(ggplot2)
library(dplyr)
setwd("~/Documentos/Git/aulas_R/amostragem")
# 1 ) #####
n <- 1000
N <- 36000

cadastro <- data.frame(
  n_operarios = c(451, 162, 187, 112, 49, 21, 5, 11, 2),
  n_faltas = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
)

ggplot(
  data.frame(cont = rep(cadastro$n_faltas, cadastro$n_operarios)),
  aes(cont)
) +
  geom_density()


# a seja x numero de faltas no i-essimo operario
sum_xi <- sum(cadastro$n_operarios * cadastro$n_faltas)
x_barra <- sum_xi / n
x_barra # estimativa media amostral

sum_xi2 <- sum(cadastro$n_faltas^2 * cadastro$n_operarios)

s2 <- ((sum_xi2 / n) - ((sum_xi / n)^2)) * (n / (n - 1))
s2
var_x <- ((N - n) / N) * (s2 / n)
var_x # estimativa para a variancia do numero de faltas

# b
z_alfa2 <- qnorm(0.025, lower.tail = F)
ic <- c(x_barra - z_alfa2 * sqrt(var_x), x_barra + z_alfa2 * sqrt(var_x))
ic # intervalo com chance de conter a verdadeira media amostral

# c
err <- (ic[2] - ic[1])
err # erro anterior
n_err <- err / 4 # novo erro P(|x_barra - E(X)|<n_err)

novo_n <- (z_alfa2^2 * N * s2) / (z_alfa2^2 * s2 + N * n_err^2)
ceiling(novo_n) # novo numero de funcionarios para fazer a amostra

rm(list = ls())
# 2 ) #####
# a)
set.seed(999)
hajek <- data.frame(
  empresa = c(1:10), aleatorio = runif(10, 0, 1),
  receita = c(
    8000, 12000, 6000, 10000, 5000,
    18000, 18000, 4000, 9000, 8000
  )
)
empresas <- dplyr::arrange(hajek, aleatorio)

ggplot(
  data.frame(cont = empresas$receita),
  aes(cont)
) +
  geom_density()

n <- 3 # tamanho da amostra
N <- nrow(empresas) # tamanho populacao
amostra <- empresas[1:n, ]

# b
x_barra <- mean(amostra$receita)
x_barra # estimativa da receita media das empresas

x_total <- N * x_barra
x_total # estimativa para o total de receita na populacao

# c
sum_xi <- x_barra * n
sum_xi2 <- sum(amostra$receita^2)

s2 <- ((sum_xi2 / n) - (x_barra^2)) * (n / (n - 1))
s2

var_xbarra <- ((N - n) / N) * (s2 / n)
var_xbarra # estimativa para a variancia do numero de faltas
cov_xbarra <- sqrt(var_xbarra) / x_barra * 100
cov_xbarra # estimativa do coeficiente de variacao da media pupulacional

var_xtotal <- N * (N - 1) * s2 / n
var_xtotal # estimativa para a variancia do total
cov_xtotal <- sqrt(var_xtotal) / x_total * 100
cov_xtotal # estimativa para o coeficiente de variacao do total pop

rm(list = ls())
# 3 ) ####
N <- 100
n <- 10
amostra <- c(2, 5, 1, 4, 4, 3, 2, 5, 2, 3)
sum_xi <- sum(amostra)
sum_xi2 <- sum(amostra^2)

ggplot(
  data.frame(cont = amostra),
  aes(cont)
) +
  geom_density()


# a
x_barra <- sum_xi / n
x_barra # estimador da media populacional

s2 <- (n / (n - 1)) * (1 / n * sum_xi2 - x_barra^2)
s2
var_xbarra <- ((N - n) / (N * n)) * s2
var_xbarra # variancia do estimador da media no plano AASsr

cov_xbarra <- sqrt(var_xbarra) / x_barra * 100
cov_xbarra # coeficiente de variacao do estimador da media


# b
x_total <- N * x_barra
x_total

var_xtotal <- N * (N - n) * s2 / n
var_xtotal # variancia do estimador do total no plano AASsr
cov_xtotal <- sqrt(var_xtotal) / x_total * 100
cov_xtotal # coeficiente de variacao do estimador do total


save.image("~/Documentos/Git/aulas_R/amostragem/exercicio1list.RData")
