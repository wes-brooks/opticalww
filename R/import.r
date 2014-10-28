library(dplyr)
library(reshape2)

#import the SS data
ss = read.csv("data/SSvectorized.csv")

#find the columns corresponding to excitation-emission data
indx = grepl("f(\\d{3})\\.(\\d{3})", colnames(ss)) %>% which

#set up a data frame with the excitation-emission frequencies
matches = gregexpr("\\d{3}", colnames(ss)[indx])
freqs = regmatches(colnames(ss)[indx], matches) %>% as.data.frame %>% t %>% as.data.frame
rownames(freqs) = NULL
colnames(freqs) = c("excite", "emit")
freqs = within(freqs, {
    excite <- as.numeric(levels(excite)[excite])
    emit <- as.numeric(levels(emit)[emit])
})

#for observation i, extract the values of the excitation-emission spectrum
eem = matrix(NA,0,3)
for (i in 1) {
    temp = cbind(freqs, t(ss[i,indx]))
    rownames(temp) = NULL
    colnames(temp)[3] = 'val'
    eem = rbind(eem, temp)
}

eem2 = freqs
for (i in 1:55) {
    temp = t(ss[i,indx])
    eem2 = cbind(eem2, temp)
}
rm = rowMeans(eem2[,3:57])


anomaly = eem$val - rm
anomaly = cbind(freqs, anomaly)



wide = acast(anomaly, excite~emit)
