ss0 = ss[47:ncol(ss)]
ss0 = sweep(ss0, 1, ss$DOCResult, '*')
ss0$response = ss$response

ss1 = ss0[ss$Event=='01',]
ss2 = ss0[ss$Event=='02',]
ss3 = ss0[ss$Event!="04",]

m3 = gbm(log10(response)~.,
         data=ss3,
         interaction.depth=5,
         bag.fraction=0.8,
         n.trees=8000,
         n.minobsinnode=5,
         cv.folds=nrow(ss1),
         shrinkage=0.0005)

plot(predict(m1, ss2), log10(ss2$response))

plot(predict(m3, ss0[ss$Event=="04",]), log10(ss0$response[ss$Event=="04"]))
plot(predict(m3, ss3), log10(ss3$response))
