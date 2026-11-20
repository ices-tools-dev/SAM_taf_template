## Write the ASD upload file (catch scenarios for the Advice and Scenarios
## Database)

## Before: output/forecast/catch_options.csv, model/assessment/fit.rds,
##         boot/data/stock_info.csv
## After: output/asd/<stock>_asd_upload.xml

# The file is written, not uploaded. Check it, and edit the scenario labels and
# notes, before uploading it to https://asd.ices.dk

library(TAF)
library(icesASD)

mkdir("output/asd")

info_path <- taf.data.path("stock_info.csv")
if (file.exists(info_path)) {
  stock_info <- read.taf(info_path)
} else {
  ## Stock information (PLACEHOLDERS, replace with the stock's)
  stock_info <- data.frame(StockCode = "ple.27.7d", AssessmentYear = 2026,
                           ContactPerson = "assessor@example.org",
                           Purpose = "Advice")
}
info <- list(stockCode = stock_info$StockCode,
             AssessmentYear = stock_info$AssessmentYear,
             Purpose = stock_info$Purpose,
             contact = stock_info$ContactPerson)

catch_options <- read.taf("output/forecast/catch_options.csv")
data.yr <- max(readRDS("model/assessment/fit.rds")$data$years)
advice.yr <- data.yr + 2

# Columns of the catch scenario table: one per column of catch_options. Text
# is not escaped by createASDxml, so no < > or & in labels and notes
scenarioColumns <- data.frame(
  AK_ID = 1:8,
  AK_Label = c("Total catch", "Projected landings", "Projected discards",
               "Ftotal", paste("SSB", advice.yr + 1), "% SSB change",
               "% TAC change", paste("Probability SSB", advice.yr + 1, "below Blim")),
  YearLabel = c(rep(advice.yr, 4), advice.yr + 1, advice.yr + 1, advice.yr,
                advice.yr + 1),
  Unit = c("t", "t", "t", "ratio", "t", "%", "%", "%")
)

# Rows: one per forecast scenario. The MSY approach is the basis of the advice
labels <- c(
  MSY_approach = "MSY approach",
  Fmsy = "F = FMSY",
  MSY_approach_lw = "MSY approach: FMSY lower",
  Fmsy_lower = "F = FMSY lower",
  Fmsy_upper = "F = FMSY upper",
  "F=0" = "F = 0",
  Fpa = "F = Fpa",
  Flim = "F = Flim",
  "F=Fint" = paste0("F = F", data.yr + 1),
  "SSBintplus2=Blim" = paste0("SSB", advice.yr + 1, " = Blim"),
  "SSBintplus2=Bpa" = paste0("SSB", advice.yr + 1, " = Bpa"),
  "SSBintplus2=MSYBtrigger" = paste0("SSB", advice.yr + 1, " = MSY Btrigger")
)
scenarioRows <- data.frame(
  AR_ID = seq_len(nrow(catch_options)),
  CS_Purpose = ifelse(catch_options$scenario == "MSY_approach",
                      "BasisAdvice", "OtherScenarios"),
  CS_Basis = ifelse(catch_options$scenario == "MSY_approach", "MSY", "Other"),
  CS_Label = ifelse(catch_options$scenario %in% names(labels),
                    labels[catch_options$scenario], catch_options$scenario)
)

# Values: one per row and column, in the order of scenarioColumns
values <- as.matrix(catch_options[-1])
values[, ncol(values)] <- 100 * values[, ncol(values)]  # probability as %
scenarioValues <- data.frame(
  AR_ID = rep(scenarioRows$AR_ID, times = ncol(values)),
  AK_ID = rep(scenarioColumns$AK_ID, each = nrow(values)),
  Value = as.vector(values)
)

# Footnotes to the table: symbol and text, to be completed for the advice
scenarioNotes <- data.frame(
  Symbol = "*",
  Notes = "PLACEHOLDER: replace with the notes for the catch scenario table."
)

xml <- createASDxml(info, scenarioColumns, scenarioRows, scenarioValues,
                    scenarioNotes)
file <- file.path("output/asd", paste0(info$stockCode, "_asd_upload.xml"))
cat(xml, file = file)
