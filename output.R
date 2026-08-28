## Extract results of interest, write TAF output tables

## Before:
## After:

library(TAF)

mkdir("output")

source("output_assessment_SAM.R")
source("output_forecast_SAM.R")
source("output_FLStock_SAM.R")
source("output_change_in_advice_SAM.R")
# source("output_SAG_ASD_SAM.R")