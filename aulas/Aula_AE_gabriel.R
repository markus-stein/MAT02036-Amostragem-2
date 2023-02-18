
# Amostragem Estratificada ------------------------------------------------
## Autor: Gabriel Fagundes - https://github.com/gabefagundes/Amostragem/blob/main/Estratificada.R

library(TeachingSampling)
library(tidyverse)

data(Lucy)
head(Lucy)

# Variável de interesse: Income;
# Variável estratificadora: Zone


# Parâmetros --------------------------------------------------------------

H = Lucy %>%
    select(Zone) %>%
    unique() %>%
    nrow() # Número de Estratos

Nh = Lucy %>% 
     select(Zone) %>%
     table() # Tamanho de cada estrato.

N = sum(Nh)

Wh = Nh/N

# Parâmetros por Estratos

mih = tapply(Lucy$Income, Lucy$Zone, mean) #Calcula a média de income por cada zona.

varpop = function(x){
    (length(x) - 1)*var(x)/length(x)
}

sigma2h = tapply(Lucy$Income, Lucy$Zone, varpop)

# Parãmetros da população

mi = mean(Lucy$Income)

sigma2 = varpop(Lucy$Income)

sigma2d = sum(Wh*sigma2h)
sigma2e = sum(Wh*(mih-mi)^2)


# Distribuição Amostral da média na AE e na AAS ---------------------------

# Supondo AASc de 30 empresas em cada estrato, vamos calcular a variância do estimador da média

nh = rep(30,5)
varxbarrah = sigma2h/nh
varmich = sum(Wh^2*varxbarrah)


# Sorteio de uma AE -------------------------------------------------------

library(sampling)

dados = tibble(
    zona = as.numeric(Lucy$Zone),
    receita = Lucy$Income,
    spam = Lucy$SPAM
) %>%
    arrange(zona)


idamostra = strata( # AASs dentro de cada estrato
    dados, 
    stratanames = c("zona"), 
    size = nh, 
    method = c("srswor")
    ) #dados devem estar ordenados
    

amostra = getdata(dados, idamostra)

amostra = amostra %>%
    mutate(tam_estrato = rep(Nh, each = 30))


# Estimativas de média e proporção com pacote survey ----------------------

library(survey)

# Estimativas considerando AASs dentro de cada estrato
dstrats = svydesign(id=~1, strata = ~Stratum, probs = ~Prob, data = amostra, fpc =~tam_estrato)
svymean(~receita, dstrats)
confint(svymean(~receita, dstrats))
svyciprop(~spam, dstrats)


dstratc = svydesign(id=~1, strata = ~Stratum, probs = ~Prob, data = amostra)
svymean(~receita, dstratc)
confint(svymean(~receita, dstratc))
svyciprop(~spam, dstratc)


# Estimativas considerando AASc dentro de cada estrato

# Efeitos das probabilidades desiguais em gráficos ------------------------


par(mfrow=c(1,3))
boxplot(Lucy$Income, ylim = c(0,2500), main= "População")
boxplot(amostra$receita, ylim = c(0,2500), main = "Desconsiderando Pesos")
svyboxplot(receita~1, dstrats, ylim = c(0,2500), main = "Considerando pesos")




# Estimativa do deff ------------------------------------------------------

svymean(~receita, dstrats, deff = T) #compara AE sem reposição com AASs
svymean(~receita, dstratc, deff = "replace") #compara AE sem reposição com AASc
