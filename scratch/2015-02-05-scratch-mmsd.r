load("data/dfOptAnalysisDataMMSDJan2015.RData")
mmsd = dfOptSumAll
predictors = names(mmsd)[which(names(mmsd)=="OB1"):ncol(mmsd)]

mmsd$human.tot = mmsd$BacHumancen + mmsd$lachnocen
lim.detect = c(lachno=200, bachum=200)

#Set the range in which this observations's count may lie:
bacteroides.censored = ifelse(mmsd$BacHumancen <= lim.detect['bachum'], TRUE, FALSE)
lachno.censored = ifelse(mmsd$lachnocen <= lim.detect['lachno'], TRUE, FALSE)
event = ifelse(bacteroides.censored & lachno.censored, 2,
               ifelse(bacteroides.censored | lachno.censored, 3, 1))
right = ifelse(bacteroides.censored & lachno.censored, lim.detect['bachum']+lim.detect['lachno'],
               ifelse(bacteroides.censored, mmsd$lachnocen + lim.detect['bachum'],
                      ifelse(lachno.censored, mmsd$BacHumancen + lim.detect['lachno'],
                             mmsd$BacHumancen+mmsd$lachnocen )))
left = ifelse(bacteroides.censored & lachno.censored, lim.detect['bachum']+lim.detect['lachno'],
              ifelse(bacteroides.censored, mmsd$lachnocen,
                     ifelse(lachno.censored, mmsd$BacHumancen,
                            mmsd$BacHumancen+mmsd$lachnocen)))

z.score.mmsd = list()

for (ev in unique(mmsd$event)) {
    z.score.sssum[[ev]] = list()
    indx = which(mmsd$event==ev)
    for (p in predictors)
        z.score.mmsd[[ev]][[p]] = summary(survreg(Surv(log10(left[indx]), log10(right[indx]), event=event[indx], type='interval')~mmsd[[p]][indx] * log10(mmsd$DOC[indx]), dist='gaussian'))$table[2,'z']
}

zscore.sssum = matrix(NA, 0, length(predictors))
for(ev in unique(mmsd$event))
    zscore.mmsd = rbind(zscore.mmsd, unlist(z.score.mmsd[[ev]]))


predrank = matrix(NA, 0, length(predictors))
for(ev in unique(mmsd$event))
    predrank = rbind(predrank, rank(abs(unlist(z.score.sssum[[ev]]))))