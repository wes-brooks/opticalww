#load("data/dfOptAnalysisDatass.fullJan2015.RData")
#ss.full.full = dfOptSumAll
predictors = names(ss.full)[which(names(ss.full)=="A200"):ncol(ss.full)]

ss.full$human.tot = ss.full$Bac.human + ss.full$Lachno.2
lim.detect = c(Lachno.2=225, Bac.human=225)

#Set the range in which this observations's count may lie:
bacteroides.censored = ifelse(ss.full$Bac.human <= lim.detect['Bac.human'], TRUE, FALSE)
lachno.censored = ifelse(ss.full$Lachno.2 <= lim.detect['Lachno.2'], TRUE, FALSE)
event = ifelse(bacteroides.censored & lachno.censored, 2,
               ifelse(bacteroides.censored | lachno.censored, 3, 1))
right = ifelse(bacteroides.censored & lachno.censored, lim.detect['Bac.human']+lim.detect['Lachno.2'],
               ifelse(bacteroides.censored, ss.full$Lachno.2 + lim.detect['Bac.human'],
                      ifelse(lachno.censored, ss.full$Bac.human + lim.detect['Lachno.2'],
                             ss.full$Bac.human+ss.full$Lachno.2 )))
left = ifelse(bacteroides.censored & lachno.censored, 450,
              ifelse(bacteroides.censored, ss.full$Lachno.2,
                     ifelse(lachno.censored, ss.full$Bac.human,
                            ss.full$Bac.human+ss.full$Lachno.2)))

z.score = list()

for (ev in unique(ss.full$event)) {
    z.score[[ev]] = list()
    indx = which(ss.full$event==ev)
    for (p in predictors)
        z.score[[ev]][[p]] = summary(survreg(Surv(log10(left[indx]), log10(right[indx]), event=event[indx], type='interval')~ss.full[[p]][indx], dist='gaussian'))$table[2,'z']
}

zscore = matrix(NA, 0, length(predictors))
for(ev in unique(ss.full$event))
    zscore = rbind(zscore, unlist(z.score[[ev]]))


predrank = matrix(NA, 0, length(predictors))
for(ev in unique(ss.full$event))
    predrank = rbind(predrank, rank(abs(unlist(z.score[[ev]]))))