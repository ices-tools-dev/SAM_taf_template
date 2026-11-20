## Prepare forecast tables and plots for report

## Before: output/forecast/catch_options.csv, Large_F_range_forecast.csv
## After: report/forecast/catch_options.csv, catch_options_F_range.png

library(TAF)

mkdir("report/forecast")

catch_options <- read.taf("output/forecast/catch_options.csv")

# Rounding as in the advice sheets: tonnes to whole numbers, F with icesRound,
# percentages to whole numbers
co <- catch_options
tonnes <- grepl("^(catch|landings|discards|ssb)_[0-9]{4}$", names(co))
co[tonnes] <- lapply(co[tonnes], round)
fcol <- grepl("^fbar_", names(co))
co[fcol] <- lapply(co[fcol], function(x) as.character(icesAdvice::icesRound(x)))
pct <- grepl("_pct$", names(co))
co[pct] <- lapply(co[pct], round)
prob <- grepl("^prob_", names(co))
co[prob] <- lapply(co[prob], function(x) round(100 * x, 1))
names(co)[prob] <- paste0(names(co)[prob], "_pct")
write.taf(co, "catch_options.csv", dir = "report/forecast")

frange <- read.taf("output/forecast/Large_F_range_forecast.csv")
taf.png("report/forecast/catch_options_F_range")
plot(frange[["fbar:median"]], frange[["catch:median"]], type = "l", lwd = 2,
     xlab = paste0("Fbar ", frange$year[1]),
     ylab = paste0("Catch ", frange$year[1], " (t)"), bty = "l")
dev.off()
