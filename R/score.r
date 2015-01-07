#Combine the different contaminants:

responses = c("Lachno.2", "Bac.human", "FC", "mei", "modmtec")
lim.detect = c(Lachno.2=225, Bac.human=225, FC=225, mei=225, modmtec=1)

score = rep(0, nrow(ssum))
cens = rep(0, nrow(ssum))
for (r in responses) {
    score = score + (ssum[[r]] - mean(ssum[[r]], na.rm=TRUE)) / sd(ssum[[r]], na.rm=TRUE)
    cens = cens + ifelse(is.na(ssum[[r]]), 0, as.numeric(ssum[[r]] <= lim.detect[r]))
}
ssum$score = score
ssum$cens = cens


indx = which(!is.na(ssum$score))
drop = c("!")

#Loop through creating models as long as there are variables with no practical influence on the response:
while(length(drop) > 0) {
    m = gbm(ssum$score[indx] ~., data=tmp[indx,], n.trees=n.trees, n.minobsinnode=n.minobsinnode, interaction.depth=interaction.depth, cv.folds=5)
    nt = gbm.perf(m)
    influence = summary(m, n.trees=nt, plotit=FALSE)
    max.influence = influence$rel.inf[1]
    threshold = max.influence / 100
    drop = as.character(influence$var[which(influence$rel.inf < threshold)])
    tmp = tmp[,-which(colnames(tmp) %in% drop)]
}