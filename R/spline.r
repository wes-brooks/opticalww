library(mgcv)

#for observation i, extract the values of the excitation-emission spectrum
cc = matrix(NA,0,100)
m = list()
for (i in 51:55) {
    cat(paste(i, "\n", sep=""))
    
    eem = cbind(freqs, t(ss[i,indx]))
    rownames(eem) = NULL
    colnames(eem)[3] = 'val'

    anomaly = eem$val - rm
    anomaly = cbind(freqs, anomaly)
    
    m[[i]] = gam(anomaly~s(excite, emit, k=100), data=anomaly, family=gaussian)
    cc = rbind(cc, coef(m[[i]]))
}
colnames(cc) = NULL

#Center and scale the columns:
cm = colMeans(cc)
cc = sweep(cc, 2, cm, '-')

norm = apply(cc, 2, function(x) sum(x**2))
cc = sweep(cc, 2, norm**(-0.5), '*')

y = log(ss$Bac.human+1)[1:50]
correlations = abs(y %*% cc)
indx = order(correlations, decreasing=TRUE)

#Add the BacHum counts:
#cc = cbind(log(ss$Bac.human[1:10]), cc)
#colnames(cc)[1] = "Bac.human"

