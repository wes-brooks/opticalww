library(dplyr)
library(gbm)

mmsd = read.csv("data/MMSDvectorized.csv")
glri = read.csv("data/GLRIvectorized.csv")
ss = read.csv("data/SSvectorized.csv")


pred = as.data.frame(cbind(mmsd$Human_virus, mmsd[,24:ncol(mmsd)]))
names(pred)[1] = "Human_virus"

gbm(Human_virus~.,
    data = pred,
    distribution = "gaussian",
    n.trees = 2000,
    interaction.depth = 5,
    n.minobsinnode = 3,
    shrinkage = 0.001,
    bag.fraction = 0.7    
)

