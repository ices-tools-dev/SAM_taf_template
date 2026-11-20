# Prepare data, write TAF data tables

# Before: sam_data/ (boot/data)
# After:  input tables (CSV) and data.stock.rds (data)

library(TAF)

mkdir("data")

source("data_SAM.R")
