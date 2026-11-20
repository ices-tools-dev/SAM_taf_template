## Extract results of interest, write TAF output tables

## Before: model/assessment
## After: output/assessment: partab, tab_summary, natage, fatage, mohns_rho,
##        modtab, qtab, qtab_sd (csv); output/forecast; FLStock objects;
##        output/sag: SAG upload file; output/asd: ASD upload file

library(TAF)

mkdir("output")

source("output_assessment_SAM.R")
source("output_forecast_SAM.R")
source("output_FLStock_SAM.R")
source("output_SAG_SAM.R")
source("output_ASD_SAM.R")

# Not yet part of the template:
# source("output_change_in_advice_SAM.R")
