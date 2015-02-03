
#```{r echo=FALSE}
load("data/GLRIOptSummaryJan072015.RData")
glri = dfOptSumAll
predictors = names(glri)[which(names(glri)=="OB1"):ncol(glri)]

glri$human.tot = glri$BACHUM.cn.100mls + glri$Lachno.2.cn.100ml
lim.detect = list(lachno=225, bachum=225)

#Set the range in which this observations's count may lie:
bacteroides.censored = ifelse(glri$BACHUM.cn.100mls <= lim.detect$bachum, TRUE, FALSE)
lachno.censored = ifelse(glri$Lachno.2.cn.100ml <= lim.detect$lachno, TRUE, FALSE)
event = ifelse(bacteroides.censored & lachno.censored, 2,
               ifelse(bacteroides.censored | lachno.censored, 3, 1))

right = ifelse(bacteroides.censored & lachno.censored, lim.detect$bachum+lim.detect$lachno,
               ifelse(bacteroides.censored, glri$Lachno.2.cn.100ml + lim.detect$bachum,
                      ifelse(lachno.censored, glri$BACHUM.cn.100mls + lim.detect$lachno,
                             glri$BACHUM.cn.100mls + glri$Lachno.2.cn.100ml )))

left = ifelse(bacteroides.censored & lachno.censored, lim.detect$bachum+lim.detect$lachno,
              ifelse(bacteroides.censored, glri$Lachno.2.cn.100ml,
                     ifelse(lachno.censored, glri$BACHUM.cn.100mls,
                            glri$BACHUM.cn.100mls + glri$Lachno.2.cn.100ml)))


z.score = list()
t.score = list()
chi2.score = list()
for (ev in unique(glri$Site)) {
    z.score[[ev]] = list()
    #t.score[[ev]] = list()
    chi2.score[[ev]] = list()
    indx = which(glri$Site==ev)
    for (p in predictors) {
        m = survreg(Surv(log10(left[indx]), log10(right[indx]), event=event[indx], type='interval')~glri[[p]][indx]*log10(glri$DOCResult[indx]), dist='gaussian')
        z.score[[ev]][[p]] = summary(m)$table[2,'z']
        #t.score[[ev]][[p]] = summary(lm(glri$DOCResult[indx]~glri[[p]][indx]))$coefficients[2,'t value']
        chi2.score[[ev]][[p]] = summary(m)$chi
    }
}


zscore = matrix(NA, 0, length(predictors))
for(ev in unique(glri$Site))
    zscore = rbind(zscore, unlist(z.score[[ev]]))


# tscore = matrix(NA, 0, length(predictors))
# for(ev in unique(glri$Site))
#     tscore = rbind(tscore, unlist(t.score[[ev]]))

chi2score = matrix(NA, 0, length(predictors))
for(ev in unique(glri$Site))
    chi2score = rbind(chi2score, unlist(chi2.score[[ev]]))




predrank = matrix(NA, 0, length(predictors))
for(ev in unique(glri$Site))
    predrank = rbind(predrank, rank(abs(unlist(z.score[[ev]]))))

