load("data/dfAll.RData")
absorb = read.csv("data/compiled_absorbance_Sept2013.csv", header=TRUE, row.names='wavelength')
wavelength = as.numeric(rownames(absorb))
absorb2 = list()

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Find which samples have both a spectrum and an entry in dfAll
response = "BACHUM.cn.100mls"
gr = trim(colnames(absorb))
for (g in gr) {
    if(g %in% dfAll$GRnumber && !is.na(dfAll[[response]][which(dfAll$GRnumber==g)])) {
        #The trailing [1] is because a few samples matched duplicate gr-numbers
        indx1 = which(dfAll$GRnumber==g)[1]
        indx2 = which(colnames(absorb)==g)[1]
        
        #Populate the list of data
        absorb2[[g]] = list(spectrum=data.frame(wavelength=wavelength, absorbance=absorb[,indx2]), response=dfAll[[response]][indx1])
    }
}