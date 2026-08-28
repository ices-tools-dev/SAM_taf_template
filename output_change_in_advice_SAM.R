## Extract results of interest, write TAF output tables

## Before: sam fit and forecast objects in model/assessment and model/forecast
##         sam fit and forecast object from last year's WG in boot/data
## After: tales in .RData file in output/change_in_advice

rm(list=ls())

# Libraries
library(TAF)
library(stockassessment)


# source function
source("utilities_get_advice_change_tables_SAM.R") # add to utilities/a package?

# Set assessment year
ay <- 2026 # this year's assessment/intermediate year

#==================================================================
# load assessments and forecasts 
#==================================================================

# load this year's assessment and forecast
load("model/assessment/fit.rds", verbose = TRUE)
load("model/forecast/forecast.RData", verbose = TRUE)
fit_new <- fit
fc_new <- FC[[1]] # headline advice scenario

# load last year's assessment and forecast (could also be an alternative fit and forecast)
load("boot/data/fit_old.rds", verbose = TRUE)
load("boot/data/forecast_old.RData", verbose = TRUE) # suggest that this object contains only the scenario you want to compare to save space
fit_old <- fit
fc_old <- FC1 # headline advice scenario

#==================================================================
# Get change in advice tables 
#==================================================================
# run function on first assessment 

res_new <- get_advice_change_tables_SAM(WG="WGNSSK 2026",fit=fit_new,fc=fc_new,
                                 data.yr=ay-1,assess.yr=ay,advice.yr= ay+1,
                                 Ry=2000:(ay-1),By=(ay-3):(ay-1))

# run assessment on second assessment
res_old <- get_advice_change_tables_SAM(WG="WGNSSK 2025",fit=fit_old,fc=fc_old,
                                 data.yr=ay-2,assess.yr=ay-1,advice.yr= ay,
                                 Ry=2000:(ay-2),By=(ay-4):(ay-2))

# save results 
save(list=c("res_new","res_old"),file="output/change_in_advice.RData")
