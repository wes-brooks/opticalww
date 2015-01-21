#Assume a Gaussian distribution for log(Lachno.2)
cens.indx.L = which(glri$Lachno.2.cn.100ml <= 111)
mu.Lachno = mean(log(glri$Lachno.2.cn.100ml[-cens.indx.L]), na.rm=TRUE)
sd.Lachno = sd(log(glri$Lachno.2.cn.100ml[-cens.indx.L]), na.rm=TRUE)

#compute the mean of the censored Lachno observations via rejection sampling:
propose.L = rnorm(100000, mean=mu.Lachno, sd=sd.Lachno)
mu.cens.L = mean(propose.L[propose.L<log(111)])

#Assume a Gaussian distribution for log(Bachum)
cens.indx.B = which(glri$BACHUM.cn.100mls <= 127)
mu.Bachum = mean(log(glri$BACHUM.cn.100mls[-cens.indx.B]), na.rm=TRUE)
sd.Bachum = sd(log(glri$BACHUM.cn.100mls[-cens.indx.B]), na.rm=TRUE)

#compute the mean of the censored bacteroides observations via rejection sampling:
propose.B = rnorm(100000, mean=mu.Bachum, sd=sd.Bachum)
mu.cens.B = mean(propose.B[propose.B<log(127)])

#Now impute the censored means for the censored observations:
glri$Lachno.2.cn.100ml[cens.indx.L] = exp(mu.cens.L)
glri$BACHUM.cn.100mls[cens.indx.B] = exp(mu.cens.B)



g2 = glri[,vs]
g2$response = glri$Lachno.2.cn.100ml + glri$BACHUM.cn.100mls
g2 = g2[!is.na(g2$response),]

indx = sample(1:nrow(g2), nrow(g2) %/% 2, replace=FALSE)
train = g2[indx,]
test = g2[-indx,]
mod=gbm(log10(response) ~ ., data=train, n.minobsinnode=10, interaction.depth=5, n.trees=10000, shrinkage=0.01, cv.folds=5)
