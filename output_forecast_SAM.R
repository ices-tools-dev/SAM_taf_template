## Extract results of interest, write TAF output tables

## Before: SAM forecast object in model/forecast
## After: forecast results in .txt files in output/forecast

rm(list=ls())

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("output/forecast")

# load forecast object
load("model/forecast/forecast.RData", verbose = TRUE)

# load reference points - only Blim needed
refpts_path <- "boot/data/reference_points/refpts.RData" ## path to the reference points
if (file.exists(refpts_path)) {
  load(file = refpts_path)
} else {
  ## Reference points input values
  #Blim =   #just need Blim for this script
}

# Settings - years for indexing
data.yr <- 2025
assess.yr <- 2026
advice.yr <- 2027

# update scenarios that hit SSB targets with results from optimiser (if needed)
scen_num <- which(grepl(paste0("then SSB(", advice.yr+1, ") = "),
                        names(FC), fixed = TRUE))
FC[scen_num] <- FC2

#==================================================================
# Write out forecast scenario tables
#==================================================================

writeLines("", con = "output/forecast/tab_forecasts.txt", sep = "\t")
for(i in seq(FC)){
  f <- FC[[i]]
  fc_tab <- attr(f, "tab")
  fc_lab <- attr(f, "label")
  tmp <- as.data.frame(fc_tab)
  tmp <- cbind(data.frame("scenario" = fc_lab), tmp)
  tmp <- xtab2taf(tmp)
 
  fc_tab <- xtab2taf(fc_tab)
  fc_lab <- gsub(pattern = '*', replacement = "star", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = ",", replacement = "", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "+", replacement = "plus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "-", replacement = "minus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "=", replacement = "equals", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "%", replacement = "perc", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = " ", replacement = "_", x = fc_lab, fixed = TRUE)
  fname <- paste0("tab_fc_", fc_lab, ".csv")
  
  write.taf(fc_tab, file.path("output/forecast", fname))
  
  write.table(x = paste("\n", attr(f,"label")), file = "output/forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  write.table(x = fc_tab, file="output/forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}

#==================================================================
# Calculate probability of falling below Blim
#==================================================================

yrs <- unlist(lapply(FC[[1]],function(x){return(x$year)}))

tmp <- lapply(names(FC),function(x){
  idx <- which(yrs %in% assess.yr)
  pr.intyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[2]]$ssb)
  
  idx <- which(yrs %in% advice.yr)
  pr.tacyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[3]]$ssb)
  
   idx <- which(yrs %in% (advice.yr+1))
  pr.ssbyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[4]]$ssb)
  
  df<-data.frame(year=assess.yr:(assess.yr+2),prob=c(pr.intyr,pr.tacyr,pr.ssbyr))
})
names(tmp) <- names(FC)

ssb_blim_prob <- do.call(rbind,tmp)
ssb_blim_prob$scenario <- gsub(".{2}$", "", row.names(ssb_blim_prob))
row.names(ssb_blim_prob) <- NULL

# reorder
ssb_blim_prob <- ssb_blim_prob[,c("scenario","year","prob")]

write.taf(c("ssb_blim_prob"), dir = "output/forecast", row.names = FALSE, quote=TRUE)

#==================================================================
# Export forecast results for MAP with step 0.1:
#==================================================================
# Table to detail catch options for F=0 to F=Fmsy at 0.1 invervals.
# Might not be needed for every stock.

mapIdx <- grepl(pattern = "^Fsq, then FMSY = [.[:digit:]]+$",
                names(FC))

res <- lapply(FC[mapIdx],
              function(x)
              {
                fc_tab <- attr(x, "tab")
                fc_lab <- attr(x, "label")
                tmp <- as.data.frame(fc_tab)
                tmp <- cbind(data.frame("scenario" = fc_lab),
                             year = row.names(tmp),
                             tmp)
                row.names(tmp) <- NULL
                return(tmp)
              })#, simplify = FALSE, USE.NAMES = FALSE)
resFmap <- do.call(rbind, res)

row.names(resFmap) <- NULL

write.csv(resFmap[resFmap$year %in% advice.yr, ],
          file = file.path("output/forecast", "Large_F_range_forecast.csv"))
