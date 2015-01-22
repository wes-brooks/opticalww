#find the columns corresponding to excitation-emission data
indx = grepl("f(\\d{3})\\.(\\d{3})", colnames(ss.eem)) %>% which

#set up a data frame with the excitation-emission frequencies
matches = gregexpr("\\d{3}", colnames(ss.eem)[indx])
freqs = regmatches(colnames(ss.eem)[indx], matches) %>% as.data.frame %>% t %>% as.data.frame
rownames(freqs) = NULL
colnames(freqs) = c("excite", "emit")
freqs = within(freqs, {
    excite <- as.numeric(levels(excite)[excite])
    emit <- as.numeric(levels(emit)[emit])
})

#for observation i, extract the values of the excitation-emission spectrum
a = array(NA, c(55,41,156))
eem = matrix(NA,0,3)
for (i in 1:55) {
    temp = cbind(freqs, t(ss.eem[i,indx]))
    rownames(temp) = NULL
    colnames(temp)[3] = 'val'
    #eem = rbind(eem, temp)
    
    wide = acast(temp, excite~emit)
    a[i,,] = wide
}


eem2 = freqs
for (i in indx4) {
    temp = t(ss.eem[i,indx])
    eem2 = cbind(eem2, temp)
}
r.mean = cbind(freqs, rowMeans(eem2[,3:ncol(eem2)]))
colnames(r.mean)[3] = 'val'
wide4 = acast(r.mean, excite~emit)

zz = range(c(wide1, wide2, wide3, wide4), na.rm=TRUE)


anomaly = sweep(eem2[,3:ncol(eem2)], 1, r.mean$val, '-')
anomaly = cbind(freqs, anomaly)

an1 = cbind(freqs, anomaly[,1])

