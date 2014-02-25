#Establish the file of labels:
sink("labels.txt", append=FALSE)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Find which samples have both a spectrum and an entry in dfAll
response = "BACHUM.cn.100mls"
gr = trim(colnames(a[1,,]))
for (g in gr) {
    if(g %in% dfAll$GRnumber && !is.na(dfAll[[response]][which(dfAll$GRnumber==g)])) {
        #The trailing [1] is because a few samples matched duplicate gr-numbers
        indx1 = which(dfAll$GRnumber==g)[1]
        indx2 = which(colnames(a[1,,])==g)[1]
        
        #Plot the EEM spectrum
        png(paste(g, ".png", sep=''))
        image(a[,,indx2])
        dev.off()
            
        #Write the response variable to the output file
        cat(paste(dfAll[[response]][indx1], "\n", sep=""))
    }
}

#Close the output file
sink()
