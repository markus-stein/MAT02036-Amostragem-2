## parametros e sorteio na AC1S
## material disponibilizado pela Profa. Vanessa

## carregando banco de dados
library(TeachingSampling)

data(Lucy)

## populacao
## variavel `Zone` definindo conglomerados
i = Lucy$Zone       # conglomerados
table(i)

## parametros
Ni = table(congls)  # tamanho dos conglomerados
Nbarra = mean(Ni)   # tamanho medio dos conglomerados
M = length(Ni)      # numero de conglomerados
N = M * Nbarra      # tamanho da populacao

## variavel de interesse num. de empregados
variavel = Lucy$Employees

## parametros por conglomerado 
Ti = tapply(variavel,congls,sum)        # totais dos conglomerados
Ybarrai = tapply(variavel,congls,mean)  # médias dos conglomerados
varpop = function(x){                   # funcao calcula Var de S2
  var(x)*(length(x)-1)/length(x)
}
Vari = tapply(variavel,congls,varpop) # variancia dos conglomerados Var_i,y

## parametros globais
Ty = sum(variavel)               # total populacional
Ybarra = mean(variavel)          # media = Ty / N
Ybarrabarra = mean(Ybarrai)      # media das medias  = sum(Ybarrai) / M
YbarraC = mean(Ti)               # media por conglomerado - Ty / M
Vary = varpop(variavel)          # variancia populacional - Var_y
## decomposicao da variancia Var_y
Vardc = sum( (Ni/Nbarra) * Vari) / M      # variancia dentro de conglomerados
Varec = sum( (Ni/Nbarra) * (Ybarrai - Ybarra)^2) / M  # variancia entre conglomerados
Vardc + Varec                                         # igual a Var_y???
Var_ecT = varpop(Ti)             # Variancia entre totais

#### Como fazer a selecao de uma unica amostra
m = 3

library(sampling)
am_cl = cluster( data=Lucy, clustername=c("Zone"), m, method="srswor")
dados = getdata(Lucy, am_cl)
n = dim(dados)[1]

#### Exercicios: 
# a. Trocar a variavel que define os conglomerados para Level, comparar as duas variaveis
# b. Fazer os exercicios da lista 2...