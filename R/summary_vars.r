library(gbm)
library(dplyr)
library(survival)

source("R/import.r")

#Constants used to simplify the code:
B = 50 #Number of ways to slice the data
n = nrow(ssum) #number of total observations
h = 10 #Number of held-out observations in each slice of the data

#Settings for GBM modeling:
n.trees = 3000
n.minobsinnode = 4
interaction.depth = 3
responses = c("Lachno.2", "Bac.human", "FC")

#Extract the summary variables:
ssum2 = ssum[,47:ncol(ssum)]

gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#Get a list of the summary variables in alphabetical order:
vars = colnames(ssum2)
vars = sort(vars)
inf = list()
ord = list()
infmean = list()
ordmean = list()
nothing = list()

#Slice up the data in B different ways:
indx = lapply(1:B, function(b) sample(1:n, replace=TRUE))

for (r in responses) {
    tmp = ssum2
    
    #Add the response to ssum2:
    tmp$r = ssum[[r]]
    inf[[r]] = matrix(NA, 0, length(vars))
    ord[[r]] = matrix(NA, 0, length(vars))
    colnames(inf[[r]]) = vars
    
    #Model the response on each slice of the data, recording the ordering of the covariates:
    for (i in 1:B) {
        boot = tmp[indx[[i]],]
        m = gbm(Surv(time=ifelse(r==0,0,-log(r)), event=r!=0, type='right')~., data=boot, n.trees=n.trees, n.minobsinnode=n.minobsinnode, interaction.depth=interaction.depth, distribution='coxph', cv.folds=5)
        nt = gbm.perf(m)
        summ = summary(m, n.trees=nt, plotit=FALSE)
        
        o = order(as.character(summ$var))
        inf[[r]] = rbind(inf[[r]], summ$rel.inf[o])
        ord[[r]] = rbind(ord[[r]], o)
    }
    
    #Compute the mean ordering:
    ordmean[[r]] = colMeans(ord[[r]])
    infmean[[r]] = colMeans(inf[[r]])
    
    nothing[[r]] = sapply(1:ncol(ord[[r]]), function(j) all(ord[[r]][,order(ordmean[[r]])][,j]==j)) %>% which
}


#Remove the useless columns:
for (x in vars[order(ordmean[[r]])][nothing[[r]]]) {
    ssum2[[x]] = NULL
}

topnames = list()
for (r in responses) {
    print(r)
    apply(inf[[r]][,ord[[r]]], 2, function(x) 1 %in% x | 2 %in% x) %>% which %>% max -> idr
    infmean[[r]][ord[[r]]][1:idr] %>% names -> topnames[[r]]
}

