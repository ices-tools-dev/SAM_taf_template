## Extract results of interest, write TAF output tables

## Before: model/assessment/fit.rds, boot/data/stock_info.csv
## After: output/<stock>_FLStock_object_model_estimates.Rdata,
##        output/<stock>_FLStock_object_input_data.Rdata

library(TAF)
library(stockassessment)
library(FLCore)
library(FLfse)


# load fit
fit <- readRDS("model/assessment/fit.rds")

info_path <- taf.data.path("stock_info.csv")
if (file.exists(info_path)) {
  stock_code <- read.taf(info_path)$StockCode
} else {
  stock_code <- "ple.27.7d" # PLACEHOLDER, the stock code
}
file_code <- gsub(".", "_", stock_code, fixed = TRUE)

# ====================================================================
# make model estimate object
# ====================================================================

stock <- SAM2FLStock(fit, catch_estimate=TRUE)
# if assessment uses GMRF for any biologicals then use:
# stock <- SAM2FLStock(fit,catch_estimate=TRUE, mat_est=TRUE,stock.wt_est=TRUE,
#                      catch.wt_est=TRUE,m_est=TRUE)

# desc
stock@desc <- paste(stock_code, "- FLStock created from SAM model fit. catches = model estimates")
stock@name <- stock_code

# set units
nmes <- names(units(stock))
un.lst <- as.list(c(rep(c("tonnes","thousands","kg"),4),rep("NA",2),"f",rep("NA",2)))
names(un.lst) <- nmes
units(stock) <- un.lst


save(stock,file=file.path("output", paste0(file_code, "_FLStock_object_model_estimates.Rdata")))

# ====================================================================
# make stock data object 
# ====================================================================

stock.data <- SAM2FLStock(fit, catch_estimate=FALSE)

# empty assessment estimate slots
stock.data@stock.n[] <- NA
stock.data@stock[] <- NA
stock.data@harvest[] <- NA

# desc
stock.data@desc <- paste(stock_code, "- FLStock created from input data. catches = observations")
stock.data@name <- stock_code

# set units
nmes <- names(units(stock.data))
un.lst <- as.list(c(rep(c("tonnes","thousands","kg"),4),rep("NA",2),"f",rep("NA",2)))
names(un.lst) <- nmes
units(stock.data) <- un.lst

# check range
range(stock.data)

save(stock.data,file=file.path("output", paste0(file_code, "_FLStock_object_input_data.Rdata")))

