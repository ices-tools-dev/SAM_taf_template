## Prepare plots and tables for report

## Before: model/assessment, output/assessment, output/forecast, data
## After: report/assessment, report/forecast: plots (png) and tables (csv)

library(TAF)

mkdir("report")

source("report_assessment_SAM.R")
source("report_forecast_SAM.R")

# Not yet part of the template:
# source("report_change_in_advice_SAM.R")
