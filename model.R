## Run analysis, write model results

## Before: data/data.stock.rds
## After: model/assessment/fit.rds, retro_fit.rds, leaveoneout.rds,
##        residuals.rds, procres.rds (jitter.rds if run)

library(TAF)

mkdir("model")

source("model_assessment_SAM.R")

# Not yet part of the template:
# source("model_forecast_SAM.R")
