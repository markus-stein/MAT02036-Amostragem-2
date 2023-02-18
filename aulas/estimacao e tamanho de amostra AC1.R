## estimacao e tamanho de amostra AC1
## material disponibilizado pela Profa. Vanessa

## carregando banco de dados
library(TeachingSampling)

data(Lucy)

## variavel `Zone` definindo conglomerados
i = Lucy$Zone       # conglomerados
table(i)

## parametros
Ni = table(i)       # tamanho dos conglomerados
Nbarra = mean(Ni)   # tamanho medio dos conglomerados
M = length(Ni)      # numero de conglomerados
N = M * Nbarra      # tamanho da populacao

## variavel de interesse num. de empregados
variavel = Lucy$Employees
Ybarra = mean(variavel)          # media = Ty / N

## selecao de uma unica amostra e obter estimativas
library(sampling)
m = 2                 # num. conglomerados na amostra

set.seed(2255)
am_cl = sampling::cluster(data = Lucy, clustername = "Zone", size = m, method = "srswor")
dados = getdata(Lucy, am_cl)
n = dim(dados)[1]

## Obtendo estimativas de media e total usando o pacote `survey`
library(survey)

dcluslucy <- svydesign( id = ~ Zone, data = dados, probs = ~ Prob, fpc = rep(M,n)) # probs = m/M pois selecao sem reposicao # fpc necessario na sem reposicao
svymean( ~ Employees, dcluslucy)
svytotal( ~ Employees, dcluslucy)

## estimadores nao viciados - na amostragem sem reposicao
totaisam = tapply( dados$Employees, dados$Zone, sum)
totaisam
tbarra = mean( totaisam, na.rm=T)
s2t = var( totaisam, na.rm=T)

## conferir!!!
tauch = M * tbarra
mich = M * tbarra/N                   # sum( totaisam, na.rm=T) / n???

s2tauch = M * (M - m) * s2t / m
s2mich = (1/Nbarra)^2*(1-m/M)*s2t/m

## estimando a proporcao
variavel = Lucy$SPAM
prop.table(table(variavel))

svymean(~SPAM, dcluslucy)
svytotal(~SPAM, dcluslucy)

## IC
confint( svymean(~SPAM, dcluslucy))

## calculando numero de conglomerados necessarios para sortear

# Funcao do epiR 0.9-99
epi.cluster1size = function (n, mean, var, epsilon.r, method = "mean", conf.level = 0.95) 
{
  N. <- 1 - ((1 - conf.level)/2)
  z <- qnorm(N., mean = 0, sd = 1)
  if (method == "total") {
    Vsq <- var/mean^2
    numerator <- z^2 * n * Vsq
    denominator <- z^2 * Vsq + (n - 1) * epsilon.r^2
    rval <- round(numerator/denominator, digits = 0)
  }
  if (method == "mean") {
    Vsq <- var/mean^2
    numerator <- z^2 * n * Vsq
    denominator <- z^2 * Vsq + (n - 1) * epsilon.r^2
    rval <- round(numerator/denominator, digits = 0)
  }
  if (method == "mean.per.unit") {
    Vsq <- var/mean^2
    numerator <- z^2 * n * Vsq
    denominator <- z^2 * Vsq + (n - 1) * epsilon.r^2
    rval <- round(numerator/denominator, digits = 0)
  }
  if (method == "proportion") {
    if (length(var) != 2) 
      stop("Error: var must be of length 2")
    if (length(mean) != 2) 
      stop("Error: mean must be of length 2")
    rval <- "Not implemented yet!"
  }
  return(rval)
}

#### Exemplo 1
# Suponha que se deseja estimar a media de renda familiar em certa cidade com um erro nao-maior que 10%
# e 99.73% de confianca (z=3). Para tanto, vai se fazer AC em um estagio de bairros da cidade (M = 5). De estudos
# anteriores, sabe-se que variancia entre totais = 6.8 e total medio = 34.

M = 5
s2.entre.totais = 6.8
total.medio = 34
sqrt(s2.entre.totais)/total.medio  #CV
conf = 0.9973
erro.rel = 0.1

epi.cluster1size(n = A, mean = total.medio, var = s2.entre.totais, epsilon = erro.rel, 
                 method = "mean.per.unit", conf.level = conf)


# OBS: Para selecao com reposicao, colocar M 'grande'

# Este erro relativo de 10% equivale a qual erro absoluto para a media? Precisa saber o N. Digamos N = 5000
N = 5000
M = 5
Nbarra = N/M
mi = total.medio/Nbarra
erro.rel*mi # erro absoluto


#### Exemplo 2
# Deseja-se fazer uma AC de 1 estagio de fazendas criadoras de gado de certa regiao. O ojetivo e estimar
# a prevalencia de uma doenca. Em uma regiao similar, sabe-se que a prevalencia da doenca e 10% e rhoint = 0.1225. Quantas fazendas
# devem ser pesquisadas, considerando que as fazendas tem em media 50 animais, e = 0.01 e 95% de confianca?

# Valores previos
pi = 0.1
rhoint = 0.1225
conf = 0.95
erro.abs = 0.01
erro.rel = erro.abs/pi
Nbarra = 50

# usando pacote - precisa ser o erro relativo

library(epiR)
epi.ssclus1estb(b = Nbarra, Py = pi, rho = rhoint, epsilon.r = erro.rel, conf.level = conf)

# OBS1: No argumento b pode-se informar um vetor de tamanho 2 onde o primeiro elemento é Nbarra e 
# o segundo é o desvio-padrão dos tamanhos dos conglomerados

# OBS2: para estimação de média o epiR atual também tem a função epi.ssclus1estc