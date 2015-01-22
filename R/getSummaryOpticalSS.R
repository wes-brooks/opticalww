# Script with workflow for adding summary optical data from vectorized
# fluorescence and full absorbance scans for storm sewer samples from Tosa 
# and Madison 2013 sampling 

###################
#Set R directory
SNsys <- system("wmic bios get serialnumber", intern = TRUE, show.output.on.console = FALSE)
SN <- gsub("\\s","",SNsys)[2]
if(SN == "PB519R1") {Rlocal <- "D:/srcldata/R" #Laptop
                     Project <- "D:/srcldata/GLRI toxics/Shared_optical_data/MMSD/Storm Sewer/Final data"
}else {Rlocal <- "//igsarmewwssrc/SRCdata/R"
       Project <- "M:/QW Monitoring Team/GLRI toxics/Shared_optical_data/MMSD/Storm Sewer/Final data"} #Network
setwd(Project)
library(USGSHydroOpt)




checkDups <- function(df,parm){
  df[duplicated(df[,parm]),parm]
}

# Load 2-D and 3-D fluorescence data and absorbance data
load("AbsFlSSdata.RData")



df <- dfSB
names(df)
df <- df[,-c(21:84)]
dfIDs <- dfLabIDs

#Reformat a few things 

names(dfIDs)[grep("GRnum",names(dfIDs))] <- "GRnumber"
names(df)[grep("GRnum",names(df))] <- "GRnumber"


# Read lab IDs
dfOpt <- dfIDs[!is.na(dfIDs$GRnumber),]


# Read summary signals to extract from Fl and abs info
dfFlSignals <- read.csv("../../../Summary variables/ex_ems_means.csv",as.is=TRUE)
dfAbsSignals <- read.csv("../../../Summary variables/abs_wavs.csv",as.is=TRUE)
AbsSignals <- as.numeric(dfAbsSignals[,1])
dfSagSignals <- read.csv("../../../Summary variables/SagWaves.csv",as.is=TRUE)
ratioSignalsAbs <- read.csv("../../../Summary variables/ratioSignalsAbs.csv",as.is=TRUE)
ratioSignalsAbs <- ratioSignalsAbs[which(ratioSignalsAbs[2]>0),1]
ratioSignalsSr <- read.csv("../../../Summary variables/ratioSignalsSrCA.csv",as.is=TRUE)
ratioSignalsSr <- ratioSignalsSr[which(ratioSignalsSr[2]>0),1]
ratioSignalsSniff <- read.csv("../../../Summary variables/ratioSignalsSniff.csv",as.is=TRUE)
ratioSignalsSniff <- ratioSignalsSniff[which(ratioSignalsSniff[2]>0),1]
logSignals <- read.csv("../../../Summary variables/logSignals.csv",as.is=TRUE)[,1]


##############################################################################################
######### Add summary variables to summary data frame #######################################

#Fluorescence pairs and means
dfOpt2 <- getMeanFl(a=SS3DEEMs,signals=dfFlSignals,Peak="Peak",Ex1="Ex1",Ex2="Ex2",Em1="Em1",Em2="Em2",dataSummary=dfOpt,grnum="GRnumber")

#HIX, FI, Freshness 
dfOpt2 <- getIndexes(a=SS3DEEMs,dataSummary=dfOpt2,grnum="GRnumber")

#Single absorbance signals
dfOpt2 <- getAbs(dataAbs=dfabs,waveCol=,"Wavelength",wavs=dfAbsSignals[,1],colSubsetString="gr",dataSummary=dfOpt2,grnum="GRnumber")

#Spectral slopes
dfOpt2 <- getSag(dataAbs=dfabs,waveCol=,"Wavelength",sag=dfSagSignals,colSubsetString="gr",dataSummary=dfOpt2,grnum="GRnumber")

#deviance of abs from exponential regression in the wastewater area
dfOpt2 <- getExpResid(wavelength=267,rangeReg=c(240,340),rangeGap=c(255,300),dataAbs=dfabs,waveCol="Wavelength",colSubsetString="gr",dataSummary=dfOpt2,grnum="GRnumber")

#Ratios of a few things
dfOpt2 <- getRatios(dataSummary=dfOpt2,sigs=ratioSignalsAbs,grnum="GRnumber")
dfOpt2 <- getRatios(dataSummary=dfOpt2,sigs=ratioSignalsSr,grnum="GRnumber")
dfOpt2 <- getRatios(dataSummary=dfOpt2,sigs=ratioSignalsSniff,grnum="GRnumber")

#log transform where it makes sense
dfOpt2 <- getLog10(dataSummary=dfOpt2,signals=logSignals,grnum="GRnumber")

##############################################################################################
##############################################################################################


##########
# Merge with dataframe that has summary data from CA and microbiology data

dfOptSumAll <- merge(df,dfOpt2,by=names(dfOpt2)[1:4],)
IVs <- names(dfOpt2)[5:209]
names(dfOptSumAll)

checkDups(dfOptSumAll,"GRnumber")
save(dfOptSumAll,file="dfOptAnalysisDataSSJan2015.RData")
write.csv(dfOptSumAll,"dfOptAnalysisDataSSJan2015.csv",row.names=FALSE)

plot(log10(dfOptSumAll$Bac.human+200)~dfOptSumAll$M,col=as.factor(dfOptSumAll$event))

summary(lm(log10(dfOptSumAll$Bac.human+200)~dfOptSumAll$M+dfOptSumAll$rA254_A290+dfOptSumAll$rMrange.50_F))
