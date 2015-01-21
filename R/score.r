models = list()

for (ev in 1:4) {
    #This is temporary, for LOO-CV:
    train = ssum2[ssum$Event!=ev,]
    test = ssum2[ssum$Event==ev,]
    drop = c("!")
    mm = list()
    iter = 0
    
    #Loop through creating models as long as there are variables with no practical influence on the response:
    while(ncol(train) > 2) {
        iter = iter+1
        print(iter)
        
        m = gbm(log10(response) ~., data=train, n.trees=5000, shrinkage=0.005,
                n.minobsinnode=8, interaction.depth=4, cv.folds=nrow(train),
                n.cores=6)
        nt = gbm.perf(m)
        influence = summary(m, n.trees=nt, plotit=FALSE)
        
        drop = as.character(influence$var[influence$rel.inf==0])
        if (length(drop>0))
            influence = influence[-which(as.character(influence$var) %in% drop),]
        nvar = length(influence$var)
        drop = c(drop, as.character(rev(influence$var)[1:(nvar %/% 10)]))
        print(paste("drop: ", length(drop), sep=""))
    
        train = train[,-which(colnames(train) %in% drop)]
    
        print(paste("keep: ", ncol(train) - 1, sep=""))
        mm[[iter]] = m
    }
    
    models[[i]] = mm    
}

