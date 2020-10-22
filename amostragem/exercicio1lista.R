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

rm(list = ls())

# 4 ) #####
N <- 14848
n <- 30
amostra <- c(
  5, 6, 3, 3, 2, 3, 3, 3, 4, 4,
  3, 2, 7, 4, 3, 5, 4, 4, 3, 3,
  4, 3, 3, 1, 2, 4, 3, 4, 2, 4
)
sum_xi <- sum(amostra)
sum_xi2 <- sum(amostra^2)

# a
x_barra <- sum_xi / n # estimador da media populacional
s2 <- (sum_xi2 / n - (sum_xi / n)^2) * n / (n - 1)
var_xbarra <- (N - n) / (N * n) * s2 # variancia do estimador da media populacional

# b
z_alfa <- qnorm(.05, lower.tail = F)
erro_amostral <- z_alfa * sqrt(var_xbarra)
ic <- c(x_barra - erro_amostral, x_barra + erro_amostral)
ic # intervalo de confianca para o verdadeiro parametro da media populacional
# com confianca de 90%

# c
erro_novo <- erro_amostral / 2
d2 <- (erro_novo / z_alfa)^2
n <- N / (d2 * N / s2 + 1)

rm(list = ls())

# 5 ) ####
n <- 5
N <- 250
amostra <- c(12, 100, 120, 48, 60)
sum_xi <- sum(amostra)
sum_xi2 <- sum(amostra^2)
s2 <- (sum_xi2 / n - (sum_xi / n)^2) * n / (n - 1)

# a
x_barra <- sum_xi / n
t_chapeu <- N * x_barra
t_chapeu # estimador do total populacional
var_tchapeu <- N^2 * (N - n) / (N * n) * (s2)
cv_tchapeu <- (sqrt(var_tchapeu) / t_chapeu) * 100
cv_tchapeu # Coeficiente de variacao de t_chapeu
z_alfa <- qnorm(0.025, lower.tail = F)
erro_amostral <- z_alfa * sqrt(t_chapeu)
ic <- c(t_chapeu - erro_amostral, t_chapeu + erro_amostral)
ic # Intervalo de confiança para o verdadeiro parametro do total populacional

# b
erro_novo <- cv_tchapeu * sqrt(t_chapeu)
d2 <- (erro_novo / z_alfa)^2
n <- N / (d2 / (N * s2) + 1)
ic_novo <- c(t_chapeu - erro_novo, t_chapeu + erro_novo)
ic_novo # Intervalo de confiança para o verdadeiro parametro do total populacional


rm(list = ls())

# 6 ) ####
n <- 3
N <- 10
escola <- tibble(
  aleatorio = c(0.282, 0.051, 0.490, 0.132, 0.349, 0.114, 0.498, 0.275, 0.651, 0.751),
  id_escola = 1:10,
  n_alunos = c(250, 350, 175, 310, 160, 350, 375, 150, 275, 240),
  n_professores = c(8, 12, 6, 10, 5, 18, 18, 4, 9, 8)
)

# a
amostra <- (escola %>% dplyr::arrange(aleatorio))[1:3, ]

# b referente a numero de alunos
sum_xi <- sum(amostra$n_alunos)
sum_xi2 <- sum(amostra$n_alunos^2)
s2 <- (sum_xi2 / n - (sum_xi / n)^2) * (n / (n - 1))
t_chapeu <- N * sum_xi / n
t_chapeu # estimador para o total populacional
var_tchapeu <- N^2 * (N - n) / (N * n) * s2
cv_tchapeu <- sqrt(var_tchapeu) / t_chapeu * 100
cv_tchapeu # coeficiente de variacao para o estimador t_chapeu
cv_tchapeu / 2
# c
z_alfa <- qnorm(.025, lower.tail = F)
erro_amostral <- z_alfa * sqrt(var_tchapeu)
ic <- c(t_chapeu - erro_amostral, t_chapeu + erro_amostral)
ic # intervalo de confianca para o parametro total populacional

rm(list = ls())

# 7 ) ####
# 8 ) ####
N <- 1500
n <- 10
set.seed(999)
hajek <- data.frame(
  empresa = c(1:10), aleatorio = runif(10, 0, 1),
  receita = c(
    8000, 12000, 6000, 10000, 5000,
    18000, 18000, 4000, 9000, 8000
  )
)
empresas <- dplyr::arrange(hajek, aleatorio)

sum_xi <- sum(empresas$receita)
sum_xi2 <- sum(empresas$receita^2)
x_barra <- sum_xi / n # estimativa da receita media das empresas
s2 <- ((sum_xi2 / n) - (x_barra^2)) * (n / (n - 1))
var_xbarra <- ((N - n) / N) * (s2 / n) # variancia do estimador da media populacional
cv_xbarra <- sqrt(var_xbarra) / x_barra * 100 # covarianca de X_barra

z_alfa <- qnorm(0.025, lower.tail = F)
erro_amostral <- (cv_xbarra) * (z_alfa)
d2 <- (erro_amostral / z_alfa)^2
n_novo <- N / (d2 * N / s2 + 1)

rm(list = ls())

# 9 ) ####
N <- 6
n <- 2
pop <- c(8, 3, 1, 11, 4, 7)
y_barra <- tibble(
  valores = c(2, 5 / 2, 7 / 2, 4, 9 / 2, 5, 11 / 2, 6, 7, 15 / 2, 9, 19 / 2),
  p = c(2, 2, 2, 2, 2, 2, 4, 4, 2, 4, 2, 2) / 30
)
s2 <- function(arg1, arg2, n) {
  return((sum(arg1^2, arg2^2) - n * (sum(arg1, arg2) / n)^2) / (n - 1))
}

i <- indice <- 1
vetor <- c()
while (i <= length(pop) - 1) {
  j <- i + 1
  while (j <= length(pop)) {
    vetor[indice] <- s2(pop[i], pop[j], 2)
    indice <- indice + 1
    j <- j + 1
  }
  i <- i + 1
}
s2_vector <- tibble(valores = vetor, p = rep(2 / 30, 15))
s2 <- sum(s2_vector$valores * s2_vector$p)
sum_xi <- sum(pop)
sum_xi2 <- sum(pop^2)
S2 <- (sum_xi2 - N * (sum_xi / N)^2) / (N - 1)

round(s2, 5) == round(S2, 5)

rm(list = ls())

# 10 ) ####
N <- 1000
x_barra_inicial <- 2.2
amostra <- c(0, 4, 2, 3, 2, 0, 3, 4, 1, 1)
n <- length(amostra)
x_barra_novo <- mean(amostra)
var_amostra_novo <- var(amostra) / n
(sum(amostra^2) - n * (mean(amostra)^2)) / (n - 1)

z_alfa <- qnorm(0.025, lower.tail = F)
erro <- z_alfa * sqrt(var_amostra_novo)
ic <- c(x_barra_novo - erro, x_barra_novo + erro)

# ha indicios que que a media de carie das criancas nao tenha alterado com
# uma certeza de 95 %

rm(list = ls())

# 11 ) ####
n <- 15
amostra <- c(
  200, 300, 450, 560, 389, 700, 650, 700,
  420, 400, 280, 360, 240, 200, 410
)
sum_xi <- sum(amostra)
sum_xi2 <- sum(amostra^2)
x_barra <- sum_xi / n
x_barra # renda media na comunidade
s2 <- var(amostra)
var_xbarra <- s2 / n
var_xbarra # variancia do estimador da renda media da comunidade

# b
z_alfa <- qnorm(.025, lower.tail = F)
erro <- z_alfa * sqrt(var_xbarra)
ic <- c(x_barra - erro, x_barra + erro)
ic # intervalo de confinca com 95% de certeza de conter o verdadeiro
# parementro da renda media na comunidade

# c
erro # esre foi o erro cometido na construcao do intervalo de confianca

# d
erro_novo <- 50
d2 <- (erro_novo / z_alfa)^2
n <- floor(s2 / d2)
n # numero da amoastra para se obter resultados com erro menor q o
# esperado

rm(list = ls())

# 12 )####
n <- 100
x_barra <- 47.31
s2 <- 17936.43
var_xbarra <- s2 / n
z_alfa <- qnorm(0.025, lower.tail = F)
erro <- z_alfa * sqrt(var_xbarra)
ic <- c(x_barra - erro, x_barra + erro)
ic # intervalo de confiança para o verdadeiro
# parametro o media do consumo de agua

rm(list = ls())

# 13 ) ####
N <- 4
pop <- c(8, 2, 4, 6)
n <- 2
indice = i <-1
vetor <- c()
while (i <= length(pop)){
  j=1
  while (j <= length(pop)){
    vetor[indice] <- sum(pop[i],pop[j])/2
    j=j+1
    indice <- indice + 1
  }
  i=i+1
}
sort(vetor)

indice = i <-1
vetor <- c()
while (i <= length(pop)){
  j=1
  while (j <= length(pop)){
    am <- c(pop[i],pop[j])
    vetor[indice] <- var(am)
    j=j+1
    indice <- indice + 1
  }
  i=i+1
}
sum(sort(vetor)*1/16)
var(pop)*(N-1)/N
rm(list = ls())
save.image("~/Documentos/Git/aulas_R/amostragem/exercicio1list.RData")

