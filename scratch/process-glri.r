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



not.na = which(!is.na(glri$Lachno.2.cn.100ml) & !is.na(glri$BACHUM.cn.100mls))
glri = glri[not.na,]

#Augment GLRI dataset with some important signals from the SS dataset:
glri$rT_S1.25 = glri$T/glri$S1.25
glri$rS3.25_F = glri$S3.25/glri$F
glri2 = glri[,102:ncol(glri)]

urban = which(glri$Site %in% c("Rouge", "Milwaukee"))

glri2.urb = glri2[urban,]
