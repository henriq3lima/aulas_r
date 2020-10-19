library(styler)
library(ggplot2)
library(dplyr)
setwd("~/Documentos/Git/aulas_R/amostragem")

#1 ) #####
n <- 1000
N <- 36000

cadastro <- data.frame(
  n_operarios = c(451, 162, 187, 112, 49, 21, 5, 11, 2),
  n_faltas = c(0, 1, 2, 3, 4, 5, 6, 7, 8)
)

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
plot_ic

ggplot(NULL, aes(c(ic[1] - 10 * var_x, ic[2] + 10 * var_x))) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = x_barra, sd = sqrt(var_x)),
    fill = "red",
    xlim = c(ic[1] - 20 * var_x, ic[1])
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = x_barra, sd = sqrt(var_x)),
    fill = "red",
    xlim = c(ic[2], ic[2] + 20 * var_x)
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = x_barra, sd = sqrt(var_x)),
    fill = "blue",
    xlim = c(ic[1], ic[2]),
    alpha = 0.7
  ) # grafico do intervalo de confianca

# c
err <- (ic[2] - ic[1])
err # erro anterior
n_err <- err / 4 # novo erro P(|x_barra - E(X)|<n_err)

novo_n <- (z_alfa2^2 * N * s2) / (z_alfa2^2 * s2 + N * n_err^2)
ceiling(novo_n) # novo numero de funcionarios para fazer a amostra

rm(list = ls())
#2 ) #####
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
n <- 3 # tamanho da amostra
N <- nrow(empresas) # tamanho populacao
amostra <- empresas[1:n,]

#b
x_barra <- mean(amostra$receita)
x_barra # estimativa da receita media das empresas

total_x <- N*sum(amostra$receita)
total_x # estimativa para o total de receita na populacao
sum(empresas$receita)
