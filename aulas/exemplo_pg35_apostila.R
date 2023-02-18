#### Ex pagina 35

pop = c(12, 7,9,14, 8, 10)

congls = c(1,2,2,2,3,3)

A = 3
Balfas = table(congls)
Bbarra = mean(Balfas)
N = 6

#### Calculo de CCI pela definição
#Cria os pares

library(gtools)
pares1.pop = permutations( length(pop[congls == 1]), 2, pop[congls == 1] ,set=F) # dá erro pois não há pares distintos no congl 1
pares2.pop = permutations( length(pop[congls == 2]), 2, pop[congls == 2] ,set=F)
pares3.pop = permutations( length(pop[congls == 3]), 2, pop[congls == 3] ,set=F)

pares.pop = rbind(pares2.pop,pares3.pop)

# Correlação intraclasse (correlação de pearson dos pares)

rho = cor(pares.pop[,1],pares.pop[,2])
rho

#### Calculo de CCI pela formula aproximada

mi.alfas = tapply(pop, congls, mean)
mi = mean(pop)

s2.eq = sum((Balfas/Bbarra)^2*(mi.alfas - mi)^2)/A

varpop = function(x){
  var(x)*(length(x)-1)/length(x)
}
vars_congl =  tapply(pop, congls, varpop)

vars_congl[1] = 0

sigma2dc = sum((Balfas/Bbarra)*vars_congl)/A

rho = (s2.eq -sigma2dc/(Bbarra-1))/(s2.eq + sigma2dc)
