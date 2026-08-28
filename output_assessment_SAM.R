## Extract results of interest, write TAF output tables

## Before: sam fit object in model/assessment
## After: csv tables of assessment output in output/assessment

rm(list=ls())

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("output/assessment")

# load fit
load("model/assessment/fit.rds", verbose = TRUE)

# load retro
load("model/assessment/retro_fit.rds", verbose = TRUE)

#==================================================================
# Make tables - assessment
#==================================================================

# Model Parameters
partab <- partable(fit)

# Fs
fatage <- faytable(fit)
fatage <- fatage[, -1]
fatage <- as.data.frame(fatage)

# Ns
natage <- as.data.frame(ntable(fit))

# Catch
catab <- as.data.frame(catchtable(fit))
colnames(catab) <- c("Catch", "Catch Low", "Catch High")


# Summary Table
tab.summary <- cbind(as.data.frame(summary(fit)))
colnames(tab.summary)[2:3] <- c("R Low","R High")
colnames(tab.summary)[5:6] <- c("SSB Low","SSB High")
colnames(tab.summary)[8:9] <- c("Fbar Low","Fbar High")

if (max(row.names(tab.summary)) > max(row.names(catab))) {
  # Separate assessment year summary results for stocks with an in-year survey
  intyr.row <- cbind(tab.summary[nrow(tab.summary), ],
    data.frame(Catch = NA,"Catch Low" = NA,"Catch High" = NA))
  
  colnames(intyr.row) <- gsub("\\.", " ", colnames(intyr.row))
  
  tab.summary <- cbind(tab.summary[-nrow(tab.summary), ],catab)
  tab.summary <- cbind(tab.summary, intyr.row)
  
} else {
  tab.summary <- cbind(tab.summary, catab)
}


mohns_rho <- mohn(retro_fit)
mohns_rho <- as.data.frame(t(mohns_rho))

## Write tables to output directory
write.taf(
  c("partab", "tab.summary", "natage", "fatage", "mohns_rho"),
  dir = "output/assessment", row.names = TRUE,
)
