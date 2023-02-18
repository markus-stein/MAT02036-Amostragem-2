#### Divisao A

pop = c(7,8,9,10,12,14)

congls = c(1,1,2,2,3,3)
A = 3
B = 2
N = 6

library(gtools)

# Cria os pares

pares1.pop = permutations( length(pop[congls == 1]), 2, pop[congls == 1] ,set=F)
pares2.pop = permutations( length(pop[congls == 2]), 2, pop[congls == 2] ,set=F)
pares3.pop = permutations( length(pop[congls == 3]), 2, pop[congls == 3] ,set=F)

pares.pop = rbind(pares1.pop, pares2.pop,pares3.pop)


# Correlação intraclasse (correlação de pearson dos pares)

rho = cor(pares.pop[,1],pares.pop[,2])
rho

#### Divisao B
congls = c(1,2,3,1,2,3)
pares1.pop = permutations( length(pop[congls == 1]), 2, pop[congls == 1] ,set=F)
pares2.pop = permutations( length(pop[congls == 2]), 2, pop[congls == 2] ,set=F)
pares3.pop = permutations( length(pop[congls == 3]), 2, pop[congls == 3] ,set=F)

pares.pop = rbind(pares1.pop, pares2.pop,pares3.pop)

# Correlação intraclasse (correlação de pearson dos pares)

rho = cor(pares.pop[,1],pares.pop[,2])
rho

#### Divisao C
congls = c(1,2,3,3,2,1)
pares1.pop = permutations( length(pop[congls == 1]), 2, pop[congls == 1] ,set=F)
pares2.pop = permutations( length(pop[congls == 2]), 2, pop[congls == 2] ,set=F)
pares3.pop = permutations( length(pop[congls == 3]), 2, pop[congls == 3] ,set=F)

pares.pop = rbind(pares1.pop, pares2.pop,pares3.pop)

# Correlação intraclasse (correlação de pearson dos pares)

rho = cor(pares.pop[,1],pares.pop[,2])
rho


################### Banco Lucy

library(TeachingSampling)
data(Lucy)

# Verdadeiro rho se Zone como conglomerados

pares1.pop = permutations( length(Lucy$Employees[Lucy$Zone == "A"]), 2, Lucy$Employees[Lucy$Zone == "A"] ,set=F)
pares2.pop = permutations( length(Lucy$Employees[Lucy$Zone == "B"]), 2, Lucy$Employees[Lucy$Zone == "B"] ,set=F)
pares3.pop = permutations( length(Lucy$Employees[Lucy$Zone == "C"]), 2, Lucy$Employees[Lucy$Zone == "C"] ,set=F)
pares4.pop = permutations( length(Lucy$Employees[Lucy$Zone == "D"]), 2, Lucy$Employees[Lucy$Zone == "D"] ,set=F)
pares5.pop = permutations( length(Lucy$Employees[Lucy$Zone == "E"]), 2, Lucy$Employees[Lucy$Zone == "E"] ,set=F)

pares.pop = rbind(pares1.pop,pares2.pop,pares3.pop,pares4.pop,pares5.pop)

cor(pares.pop[,1],pares.pop[,2])
#.08690413

# Verdadeiro rho se Level como conglomerados

pares1.pop = permutations( length(Lucy$Employees[Lucy$Level == "Small"]), 2, Lucy$Employees[Lucy$Level == "Small"] ,set=F)
pares2.pop = permutations( length(Lucy$Employees[Lucy$Level == "Big"]), 2, Lucy$Employees[Lucy$Level == "Big"] ,set=F)
pares3.pop = permutations( length(Lucy$Employees[Lucy$Level == "Medium"]), 2, Lucy$Employees[Lucy$Level == "Medium"] ,set=F)

pares.pop = rbind(pares1.pop,pares2.pop,pares3.pop)

cor(pares.pop[,1],pares.pop[,2])
#.1798228