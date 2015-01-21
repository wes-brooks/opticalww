not.na = which(!is.na(glri$Lachno.2.cn.100ml) & !is.na(glri$BACHUM.cn.100mls))
glri = glri[not.na,]

#Augment GLRI dataset with some important signals from the SS dataset:
glri$rT_S1.25 = glri$T/glri$S1.25
glri$rS3.25_F = glri$S3.25/glri$F
glri2 = glri[,102:ncol(glri)]

urban = which(glri$Site %in% c("Rouge", "Milwaukee"))

glri2.urb = glri2[urban,]
