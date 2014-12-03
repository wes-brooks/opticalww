library(ThreeWay)

source("R/import.r")

CP(a)


fit = rep(NA, 55)
fold.id = sample(rep(1:5, 11))

for (fold in 1:5) {
    indx = which(fold.id==fold)
    estimate = df[-indx,]
    validate = df[indx,]
    
    m.cv = gbm(log.bac.human~., data=estimate, n.trees=10000, n.minobsinnode=5, interaction.depth=3, bag.fraction=0.5)
    n.trees = gbm.perf(m.cv, plot.it=FALSE)
    fit[indx] = predict(m.cv, newdata=validate, n.trees=n.trees)
}