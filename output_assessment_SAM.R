## Extract results of interest, write TAF output tables

## Before: sam fit object in model/assessment
## After: csv tables of assessment output in output/assessment

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("output/assessment")

# load fit
fit <- readRDS("model/assessment/fit.rds")

# load retro
RETRO <- readRDS("model/assessment/retro_fit.rds")

#==================================================================
# Make tables - assessment
#==================================================================

# Model Parameters
partab <- partable(fit)

# Fs
fatage <- faytable(fit)
fatage <- as.data.frame(fatage)

# Ns
natage <- as.data.frame(ntable(fit))

# Catch
catab <- as.data.frame(catchtable(fit))
colnames(catab) <- c("Catch", "Catch Low", "Catch High")


# Summary Table
# summary(fit) rounds its values, so the tables are used at full precision
tab.summary <- as.data.frame(cbind(rectable(fit), ssbtable(fit), fbartable(fit)))
colnames(tab.summary)[c(1, 4, 7)] <- colnames(summary(fit))[c(1, 4, 7)]
colnames(tab.summary)[2:3] <- c("R Low","R High")
colnames(tab.summary)[5:6] <- c("SSB Low","SSB High")
colnames(tab.summary)[8:9] <- c("Fbar Low","Fbar High")

if (max(row.names(tab.summary)) > max(row.names(catab))) {
  # Separate assessment year summary results for stocks with an in-year survey
  intyr.row <- cbind(tab.summary[nrow(tab.summary), ],
    data.frame(Catch = NA,"Catch Low" = NA,"Catch High" = NA))
  
  colnames(intyr.row) <- gsub("\\.", " ", colnames(intyr.row))
  
  tab.summary <- cbind(tab.summary[-nrow(tab.summary), ],catab)
  tab.summary <- rbind(tab.summary, intyr.row)
  
} else {
  tab.summary <- cbind(tab.summary, catab)
}


# Both lags, as the retro plot in the report uses lag 1 for R and Fbar and
# lag 0 for SSB
mohns_rho <- data.frame(lag = c(0, 1),
                        rbind(mohn(RETRO, lag = 0), mohn(RETRO, lag = 1)),
                        check.names = FALSE)

# Model table: log-likelihood, number of parameters, AIC
modtab <- as.data.frame(modeltable(fit))

# Survey catchability at age, with standard deviation
qtab <- qtable(fit)
qtab <- data.frame(Fleet = rownames(qtab), as.data.frame(unclass(qtab)),
                   check.names = FALSE)
qtab_sd <- attr(qtable(fit), "sd")
qtab_sd <- data.frame(Fleet = rownames(qtab_sd), as.data.frame(qtab_sd),
                      check.names = FALSE)

# Row names as a named first column
partab <- data.frame(Parameter = rownames(partab), partab, check.names = FALSE)
tab.summary <- data.frame(Year = rownames(tab.summary), tab.summary,
                          check.names = FALSE)
natage <- data.frame(Year = rownames(natage), natage, check.names = FALSE)
fatage <- data.frame(Year = rownames(fatage), fatage, check.names = FALSE)

## Write tables to output directory
write.taf(c("modtab", "qtab", "qtab_sd"), dir = "output/assessment")

write.taf(
  c("partab", "tab.summary", "natage", "fatage", "mohns_rho"),
  dir = "output/assessment"
)
