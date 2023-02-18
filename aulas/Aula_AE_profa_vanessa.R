#### AMOSTRAGEM ESTRATIFICADA ####
## Autora: Profa. Vanesa B. Leotti

# Vamos utilizar um banco de dados populacional hipotético

library(TeachingSampling)

data(Lucy)
head(Lucy)

# Vamos considerar Income como variável de interesse e Zone como variável estratificadora.
# Vamos calcular alguns parâmetros.

#### Parâmetros ####

H = length(unique(Lucy$Zone)) #Número de estratos
Nh = table(Lucy$Zone) #Tamanho dos estratos
N = sum(Nh) #Tamanho da população
Wh = Nh/N #Pesos dos estratos na população
Wh

# Parâmetros por estratos
mih = tapply(Lucy$Income,Lucy$Zone,mean) #Médias
mih

varpop = function(x){
  (length(x)-1)*var(x)/(length(x))
}
sigma2h = tapply(Lucy$Income,Lucy$Zone,varpop) #Variâncias
sigma2h

# Parâmetros da população geral
mi = mean(Lucy$Income)#Média
mi

sigma2 = varpop(Lucy$Income) # Variância
sigma2

sigma2d = sum(Wh*sigma2h) # Variância dentro
sigma2d
sigma2e = sum(Wh*(mih-mi)^2) # Variância entre
sigma2e
sigma2e + sigma2d

#### EXERCÍCIO 1 ####
# Repetir usando Level como variável estratificadora
# Adicionar comandos para obter totais
# Verificar que, com Level: sigma2D = 17097.52 e sigma2E = 54150.62

#### Distribuição amostral da média na AE e na AAS ####

# Supondo AASc de 30 empresas em cada estrato, vamos calcular a variância do estimador da média
nh = rep(30,5)
varxbarrah = sigma2h/nh
varxbarrah
varmich = sum(Wh^2*varxbarrah)
varmich

# Qual a variância do estimador da média se AASc tradicional de 150 empresas?
sigma2/150

#### Sorteio de uma AE ####

library(sampling)

dados = data.frame(zona=as.numeric(Lucy$Zone), receita=Lucy$Income, spam=Lucy$SPAM)
dados = dados[order(dados$zona), ]
idamostra = sampling::strata(data=dados, stratanames=c("zona"), size=c(nh), method=c("srswor")) #AASs dentro de cada estrato, para AASc "srswr"
idamostra
30/307 # probabilidade de seleção de um elemento no primeiro estrato com AASs dentro de cada estrato
amostra = getdata(dados,idamostra)
amostra
amostra$tam.estrato = rep(Nh,each=30)
amostra


#### Estimativas de média e proporção com pacote survey ####

library(survey)

# Estimativas considerando AASs dentro de cada estrato
dstrats <- svydesign(id=~1,strata=~Stratum, probs=~Prob, data=amostra, fpc=~tam.estrato)
svymean(~receita, dstrats)
confint(svymean(~receita, dstrats))
svyciprop(~spam, dstrats)

# Estimativas considerando AASc dentro de cada estrato
dstratc <- svydesign(id=~1,strata=~Stratum, probs=~Prob, data=amostra)
svymean(~receita, dstratc)
confint(svymean(~receita, dstratc))
svyciprop(~spam, dstratc)

#### Efeito das probabilidades desiguais em gráficos ####

par(mfrow=c(1,3))
boxplot(Lucy$Income, ylim=c(0,2500), main="População")
boxplot(amostra$receita, ylim=c(0,2500), main="Desconsiderando pesos")
svyboxplot(receita~1, dstrats, ylim=c(0,2500), main="Considerando pesos")


#### Estimativa do deff ####

svymean(~receita, dstrats, deff = T) #comparando AE sem reposição com AASs
svymean(~receita, dstratc, deff = "replace") #comparando AE com reposição com AASc

#### EXERCÍCIO 2 ####
# Repetir tudo que foi realizado após exercício 1 usando Level como variável estratificadora

#### Cálculo de tamanho de amostra para estimar média ####

# Apostila - pg 15

H = 3
Nh = c(600,300,100)
N = 1000
Wh = Nh/N
sigmah = c(20,30,50)

library(samplingbook)

stratasize(e = 3, Nh= Nh, Sh = sigmah, level = 0.9973, type="prop") #AEpr
stratasamp(n=434, Nh= Nh, Sh = sigmah, type="prop") #AEpr

stratasize(e = 3, Nh= Nh, Sh = sigmah, level = 0.9973, type="opt") #AEne
stratasamp(n=385, Nh= Nh, Sh = sigmah, type="opt") #AEne

# Apostila - pg 16

H = 2
Nh = c(500,500)
N = 1000
Wh = Nh/N
pih = c(20/40, 40/60)
sigmah = sqrt(pih*(1-pih))

library(samplingbook)

stratasize(e = 0.1, Nh= Nh, Sh = sigmah, level = 0.99, type="prop") #AEpr
stratasamp(n = 136, Nh= Nh, Sh = sigmah, type="prop") #AEpr

stratasize(e = 0.1, Nh= Nh, Sh = sigmah, level = 0.99, type="opt") #AEne
stratasamp(n = 136, Nh= Nh, Sh = sigmah, type="opt") #AEne
