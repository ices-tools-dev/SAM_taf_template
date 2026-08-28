## Extract results of interest, write TAF output tables

## Before:
## After:

rm(list=ls())

library(icesTAF)
library(stockassessment)
library(FLCore)
library(FLfse)


# load fit
load("model/assessment/fit.rds", verbose = TRUE)

# ====================================================================
# make model estimate object
# ====================================================================

stock <- SAM2FLStock(fit, catch_estimate=TRUE)
# if assessment uses GMRF for any biologicals then use:
# stock <- SAM2FLStock(fit,catch_estimate=TRUE, mat_est=TRUE,stock.wt_est=TRUE,
#                      catch.wt_est=TRUE,m_est=TRUE)

# desc
stock@desc <- "xxx.27.46a7d3a - FLStock created from SAM model fit. catches = model estimates"
stock@name <- "xx.27.46a7d3a"

# set units
nmes <- names(units(stock))
un.lst <- as.list(c(rep(c("tonnes","thousands","kg"),4),rep("NA",2),"f",rep("NA",2)))
names(un.lst) <- nmes
units(stock) <- un.lst


save(stock,file="output/xxx_27_46a7d3a_FLStock_object_model_estimates.Rdata")

# ====================================================================
# make stock data object 
# ====================================================================

stock.data <- SAM2FLStock(fit, catch_estimate=FALSE)

# empty assessment estimate slots
stock.data@stock.n[] <- NA
stock.data@stock[] <- NA
stock.data@harvest[] <- NA

# desc
stock.dat@desc <- "xxx.27.46a7d3a - FLStock created from input data. catches = observations"
stock.data@name <- "xx.27.46a7d3a"

# set units
nmes <- names(units(stock.data))
un.lst <- as.list(c(rep(c("tonnes","thousands","kg"),4),rep("NA",2),"f",rep("NA",2)))
names(un.lst) <- nmes
units(stock.data) <- un.lst

# check range
range(stock.data)

save(stock.data,file="output/xxx_27_46a7d3a_FLStock_object_input_data.Rdata")

