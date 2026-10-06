# Download SAM assessment from stockassessment.org

# 

# Before: 
# After: model/assessment/fit.rds etc

rm(list=ls())
graphics.off()

library(icesTAF)
library(stockassessment)

mkdir("model/assessment")

#current run name on stockassessment.org
stockname<-as.character(substitute(NSwhiting_2026n))  # change NSwhiting_2026n to new current run name!

options(download.file.method = "wininet")

download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/model.RData"),destfile="model/assessment/fit.rds")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/data.RData"),destfile="model/assessment/data.rds")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/leaveout.RData"),destfile="model/assessment/leaveoneout.rds")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/residuals.RData"),destfile="model/assessment/residuals.rds")
download.file(url=sub("SN",stockname , "https://stockassessment.org/datadisk/stockassessment/userdirs/user3/SN/run/retro.RData"),destfile="model/assessment/retro_fit.rds")

