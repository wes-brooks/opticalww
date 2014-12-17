library(gbm)
library(dplyr)

source("R/import.r")

#Constants used to smiplify the code:
B = 20 #Number of ways to slice the data
n = nrow(ssum) #number of total observations
h = 10 #Number of held-out observations in each slice of the data

#Settings for GBM modeling:
n.trees = 3000
n.minobsinnode = 4
interaction.depth = 4

#Extract the summary variables:
ssum2 = ssum[,47:ncol(ssum)]

#Get a list of the summary variables in alphabetical order:
vars = colnames(ssum2)
vars = sort(vars)
inf = matrix(NA, 0, length(vars))
colnames(inf) = vars

#Add the response variable:
ssum2$Lachno.2 = ssum$Lachno.2

#Slice up the data in B different ways:
held = lapply(1:B, function(b) sample(1:n, h))
indx = lapply(1:B, function(b) (1:n)[-held[[b]]])

#Model the response on each slice of the data, recording the ordering of the covariates:
for (i in 1:B) {
    boot = ssum2[indx[[i]],]
    boot$Lachno.2[boot$Lachno.2==0]=0.001
    m = gbm(log(Lachno.2)~., data=boot, n.trees=n.trees, n.minobsinnode=n.minobsinnode, interaction.depth=interaction.depth)
    nt = gbm.perf(m)
    summ = summary(m, n.trees=nt, plotit=FALSE)
    
    ord = order(as.character(summ$var))
    inf = rbind(inf, ord)
}

#Compute the mean ordering:
infmean = colMeans(inf)
ord = order(infmean)


