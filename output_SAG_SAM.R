## Write the SAG upload file

## Before: model/assessment/fit.rds, model/forecast/forecast.RData (optional),
##         data/catage.csv, wcatch.csv, wlandings.csv, landfrac.csv,
##         boot/data/stock_info.csv, boot/data/reference_points.csv
## After: output/sag/<stock>_sag_upload.csv

# The file is written, not uploaded. To upload, after checking it:
# icesSAG::uploadStock("output/sag/<stock>_sag_upload.csv", upload = TRUE)

library(TAF)
library(stockassessment)
library(icesSAG)

mkdir("output/sag")

fit <- readRDS("model/assessment/fit.rds")
info_path <- taf.data.path("stock_info.csv")
refpts_path <- taf.data.path("reference_points.csv")
if (file.exists(info_path)) {
  info <- as.list(read.taf(info_path))
} else {
  ## Stock information (PLACEHOLDERS, replace with the stock's)
  info <- list(StockCode = "ple.27.7d", AssessmentYear = 2026,
               ContactPerson = "assessor@example.org", StockCategory = 1,
               ModelType = "A", ModelName = "SAM", Purpose = "Advice",
               CatchesLandingsUnits = "t", RecruitmentUnits = "NE3",
               StockSizeUnits = "t", ConfidenceIntervalDefinition = "95%")
}
if (file.exists(refpts_path)) {
  refpts <- read.taf(refpts_path)
} else {
  ## Reference points (PLACEHOLDERS, replace with the stock's)
  refpts <- data.frame(Fmsy = 0.30, Fpa = 0.45, Flim = 0.60,
                       Blim = 9000, Bpa = 12500, MSYBtrigger = 12500)
}

info$RecruitmentAge <- fit$conf$minAge
info$Fage <- paste(fit$conf$fbarRange, collapse = "-")
info$Blim <- refpts$Blim
info$Bpa <- refpts$Bpa
info$MSYBtrigger <- refpts$MSYBtrigger
info$Flim <- refpts$Flim
info$Fpa <- refpts$Fpa
info$FMSY <- refpts$Fmsy

# Model estimates
est <- function(x) as.data.frame(x)[c("Low", "Estimate", "High")]
rec <- est(rectable(fit))
ssb <- est(ssbtable(fit))
tsb <- est(tsbtable(fit))
fbar <- est(fbartable(fit))
years <- as.integer(rownames(ssbtable(fit)))

fishdata <- data.frame(
  Year = years,
  Low_Recruitment = rec$Low, Recruitment = rec$Estimate, High_Recruitment = rec$High,
  Low_StockSize = ssb$Low, StockSize = ssb$Estimate, High_StockSize = ssb$High,
  Low_TBiomass = tsb$Low, TBiomass = tsb$Estimate, High_TBiomass = tsb$High,
  Low_FishingPressure = fbar$Low, FishingPressure = fbar$Estimate,
  High_FishingPressure = fbar$High
)

# Observed catches in tonnes, assuming numbers in thousands and weights in kg
catage <- read.taf("data/catage.csv")
cn <- as.matrix(catage[-1])
cw <- as.matrix(read.taf("data/wcatch.csv")[-1])
lw <- as.matrix(read.taf("data/wlandings.csv")[-1])
lf <- as.matrix(read.taf("data/landfrac.csv")[-1])
obs <- data.frame(Year = catage$Year,
                  Catches = rowSums(cn * cw),
                  Landings = rowSums(cn * lf * lw))
obs$Discards <- obs$Catches - obs$Landings
fishdata <- merge(fishdata, obs, all.x = TRUE)

# Intermediate year: recruitment and SSB from the forecast, when it was run
if (file.exists("model/forecast/forecast.RData")) {
  load("model/forecast/forecast.RData")
  tab <- attr(ls.forecast[["MSY_approach"]], "tab")
  int.yr <- as.character(max(years) + 1)
  if (int.yr %in% rownames(tab)) {
    fishdata <- merge(fishdata, data.frame(
      Year = as.integer(int.yr),
      Low_Recruitment = tab[int.yr, "rec:low"],
      Recruitment = tab[int.yr, "rec:median"],
      High_Recruitment = tab[int.yr, "rec:high"],
      Low_StockSize = tab[int.yr, "ssb:low"],
      StockSize = tab[int.yr, "ssb:median"],
      High_StockSize = tab[int.yr, "ssb:high"]), all = TRUE)
  }
}

file <- file.path("output/sag", paste0(info$StockCode, "_sag_upload.csv"))
writeSAG(info, fishdata, file = file)
