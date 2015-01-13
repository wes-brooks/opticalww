library(gbm)
library(dplyr)
library(survival)
library(censReg)

source("R/import.r")

#Constants used to simplify the code:
B = 5 #Number of ways to slice the data
n = nrow(ssum) #number of total observations
h = 10 #Number of held-out observations in each slice of the data

#Settings for GBM modeling:
n.trees = 20000
n.minobsinnode = 4
interaction.depth = 3
responses = c("Lachno.2", "Bac.human")#, "FC", "mei", "modmtec")
lim.detect = c(Lachno.2=225, Bac.human=225)#, FC=225, mei=225, mdomtec=1)

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
m = list()

inf_untrans = list()
ord_untrans = list()
infmean_untrans = list()
ordmean_untrans = list()
nothing_untrans = list()
m_untrans = list()

#Slice up the data in B different ways:
indx = lapply(1:B, function(b) sample(1:n, replace=TRUE))

for (r in responses) {
    #Add the response to ssum2:
    tmp = ssum2
    tmp$r = ssum[[r]]
    
    #Set up objects to hold the results:
    inf[[r]] = matrix(NA, 0, length(vars))
    ord[[r]] = matrix(NA, 0, length(vars))
    colnames(inf[[r]]) = vars
    m[[r]] = list()
    m_untrans[[r]] = list()
    
    inf_untrans[[r]] = matrix(NA, 0, length(vars))
    ord_untrans[[r]] = matrix(NA, 0, length(vars))
    colnames(inf_untrans[[r]]) = vars
    
    #Model the response on each slice of the data, recording the ordering of the covariates:
    for (i in 1:B) {
        min.detect = lim.detect[[r]]
        boot = tmp[indx[[i]],]
        m[[r]][[i]] = gbm(Surv(time=ifelse(r<=min.detect, -log(min.detect), -log(r)), event=(r>min.detect), type='right')~.,
                          data=train, n.trees=n.trees, n.minobsinnode=n.minobsinnode, shrinkage=0.0005,
                          interaction.depth=interaction.depth, distribution='coxph',
                          cv.folds=5)
        nt = gbm.perf(m[[r]][[i]])
        summ = summary(m[[r]][[i]], n.trees=nt, plotit=FALSE)
        
        o = order(as.character(summ$var))
        inf[[r]] = rbind(inf[[r]], summ$rel.inf[o])
        ord[[r]] = rbind(ord[[r]], o)
    }
    
    #Compute the mean ordering:
    ordmean[[r]] = colMeans(ord[[r]])
    infmean[[r]] = colMeans(inf[[r]])
    nothing[[r]] = sapply(1:ncol(ord[[r]]), function(j) all(ord[[r]][,order(ordmean[[r]])][,j]==j)) %>% which
    
    #Model the response on each slice of the data, recording the ordering of the covariates:
    for (i in 1:B) {
        boot = tmp[indx[[i]],]
        m_untrans[[r]][[i]] = gbm(r~., data=boot, n.trees=n.trees, n.minobsinnode=n.minobsinnode, interaction.depth=interaction.depth, cv.folds=5)
        nt = gbm.perf(m_untrans[[r]][[i]])
        summ = summary(m_untrans[[r]][[i]], n.trees=nt, plotit=FALSE)
        
        o = order(as.character(summ$var))
        inf_untrans[[r]] = rbind(inf_untrans[[r]], summ$rel.inf[o])
        ord_untrans[[r]] = rbind(ord_untrans[[r]], o)
    }
    
    #Compute the mean ordering:
    ordmean_untrans[[r]] = colMeans(ord_untrans[[r]])
    infmean_untrans[[r]] = colMeans(inf[[r]])
    nothing_untrans[[r]] = sapply(1:ncol(ord_untrans[[r]]), function(j) all(ord_untrans[[r]][,order(ordmean_untrans[[r]])][,j]==j)) %>% which
}


#Normalize the data:
ss.summ.normed = sweep(ssum2, 2, colMeans(ssum2))
colnorm = apply(ss.summ.normed, 2, function(x) sqrt(sum(x**2)/ncol(ss.summ.normed)))
ss.summ.normed = sweep(ss.summ.normed, 2, colnorm^(-1), '*')

#Objects to hold the results:
ll = list()
coefs = list()
res = list()
ll.pred = list()

for (b in 1:B) {
    boot = ss.summ.normed[indx[[b]],]
    validation = ss.summ.normed[-indx[[b]],]
    
    coefs[[b]] = list()
    ll[[b]] = list()
    res[[b]] = list()
    ll.pred[[b]] = list()
    
    for (r in responses) { 
        resp = log(ssum[[r]][indx[[b]]])
        resp.validation = log(ssum[[r]][-indx[[b]]])
        limdetect = log(lim.detect[r])
        
        coefs[[b]][[r]] = vector()
        ll[[b]][[r]] = vector()
        res[[b]][[r]] = vector()
        ll.pred[[b]][[r]] = vector()
        
        for (i in 1:ncol(ssum2)) {    
            m = censReg(resp~boot[,i], left=limdetect)
            coef.m = coef(m, logSigma=FALSE)
            sigma = coef.m['sigma']
            pred = rowSums(sweep(cbind(1,validation[,i]), 2, coef.m[1:2], '*'))
            
            llpred.here = ifelse(resp.validation>limdetect, dnorm(resp.validation, mean=pred, sd=sigma), 1-pnorm(pred, mean=limdetect, sd=sigma))
            
            ll.pred[[b]][[r]] = c(ll.pred[[b]][[r]], sum(llpred.here))
            ll[[b]][[r]] = c(ll[[b]][[r]], summary(m)$loglik)
            coefs[[b]][[r]] = c(coefs[[b]][[r]], coef(m)[2])
        }
        names(ll[[b]][[r]]) = names(ll.pred[[b]][[r]]) = names(coefs[[b]][[r]]) = colnames(ssum2)
        res[[b]][[r]] = data.frame(cbind(ll[[b]][[r]], coefs[[b]][[r]], ll.pred[[b]][[r]])[rev(order(ll.pred[[b]][[r]])),])
        colnames(res[[b]][[r]]) = c("loglik", "coefficient", "loglik.pred")
        res[[b]][[r]]$rank.pred = 1:nrow(res[[b]][[r]])
        res[[b]][[r]]$rank.ll = rev(order(res[[b]][[r]]$loglik))
        res[[b]][[r]]$name = rownames(res[[b]][[r]])
    }
}



#Compare on the basis of predictive ranks:
comprehensive = data.frame(matrix(NA, 0, 2))
for (b in 1:B) {
    combo = data.frame(matrix(NA, 0, 6))
    for (r in responses) {
        combo = rbind(combo, cbind(r, res[[b]][[r]]))
    }
    comprehensive = rbind(comprehensive, combo %>% group_by(name) %>% summarize(mean(rank.pred)))
}
colnames(comprehensive)[2] = 'rank.pred'
comprehensive %>% group_by(name) %>% summarize(mean(rank.pred)) -> comp



#Remove the useless columns:
for (x in vars[order(ordmean[[r]])][nothing[[r]]]) {
    ssum2[[x]] = NULL
}

topnames = list()
for (r in responses) {
    print(r)
    apply(ord[[r]], 2, function(x) 1 %in% x | 2 %in% x) %>% which %>% max -> idr
    infmean[[r]][ord[[r]]][1:idr] %>% names -> topnames[[r]]
}

