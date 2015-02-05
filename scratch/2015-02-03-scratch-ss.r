load("data/dfOptAnalysisDataSSJan2015.RData")
ss.sum = dfOptSumAll
predictors = names(ss.sum)[which(names(ss.sum)=="OB1"):ncol(ss.sum)]

#Find the mean illumination of the EEM spectrum:
ss.full = read.csv("data/SSvectorized.csv")
mm = rowMeans(ss.full[,566:5068])
ss.sum$mm = mm

ss.sum$human.tot = ss.sum$Bac.human + ss.sum$Lachno.2
lim.detect = c(Lachno.2=225, Bac.human=225)

#Set the range in which this observations's count may lie:
bacteroides.censored = ifelse(ss.sum$Bac.human <= lim.detect['Bac.human'], TRUE, FALSE)
lachno.censored = ifelse(ss.sum$Lachno.2 <= lim.detect['Lachno.2'], TRUE, FALSE)
event = ifelse(bacteroides.censored & lachno.censored, 2,
               ifelse(bacteroides.censored | lachno.censored, 3, 1))
right = ifelse(bacteroides.censored & lachno.censored, lim.detect['Bac.human']+lim.detect['Lachno.2'],
               ifelse(bacteroides.censored, ss.sum$Lachno.2 + lim.detect['Bac.human'],
                      ifelse(lachno.censored, ss.sum$Bac.human + lim.detect['Lachno.2'],
                             ss.sum$Bac.human+ss.sum$Lachno.2 )))
left = ifelse(bacteroides.censored & lachno.censored, lim.detect['Bac.human']+lim.detect['Lachno.2'],
              ifelse(bacteroides.censored, ss.sum$Lachno.2,
                     ifelse(lachno.censored, ss.sum$Bac.human,
                            ss.sum$Bac.human+ss.sum$Lachno.2)))

z.score.sssum = list()

for (ev in unique(ss.sum$event)) {
    z.score.sssum[[ev]] = list()
    indx = which(ss.sum$event==ev)
    for (p in predictors) {
        #z.score.sssum[[ev]][[p]] = summary(survreg(Surv(log10(left[indx]), log10(right[indx]), event=event[indx], type='interval')~ss.sum[[p]][indx] * log10(ss.sum$DOCResult[indx]), dist='gaussian'))$table[2,'z']
        z.score.sssum[[ev]][[p]] = summary(survreg(Surv(log10(left[indx]), log10(right[indx]), event=event[indx], type='interval')~ss.sum[[p]][indx] * ss.sum$mm[indx], dist='gaussian'))$table[2,'z']
    }
}

zscore.sssum = matrix(NA, 0, length(predictors))
for(ev in unique(ss.sum$event))
    zscore.sssum = rbind(zscore.sssum, unlist(z.score.sssum[[ev]]))


predrank = matrix(NA, 0, length(predictors))
for(ev in unique(ss.sum$event))
    predrank = rbind(predrank, rank(abs(unlist(z.score.sssum[[ev]]))))