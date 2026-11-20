## Extract results of interest, write TAF output tables

## Before: model/forecast/forecast.RData, forecast_F_range.RData,
##         model/assessment/fit.rds, boot/data/reference_points.csv, tac.csv
## After: output/forecast: tab_fc_<scenario>.csv, tab_forecasts.txt,
##        ssb_blim_prob.csv, catch_options.csv, Large_F_range_forecast.csv

# Libraries
library(TAF)
library(stockassessment)

# Directories
mkdir("output/forecast")

# load forecast object
load("model/forecast/forecast.RData", verbose = TRUE)
load("model/forecast/forecast_F_range.RData", verbose = TRUE)
FC <- Filter(Negate(is.null), ls.forecast) # scenarios from model_forecast_SAM.R

# load reference points - only Blim needed
refpts_path <- taf.data.path("reference_points.csv") ## path to the reference points
if (file.exists(refpts_path)) {
  Blim <- read.taf(refpts_path)$Blim
} else {
  ## Reference points input values
  Blim <- 9000 # PLACEHOLDER, just need Blim for this script
}

# Settings - years for indexing
data.yr <- max(readRDS("model/assessment/fit.rds")$data$years)
assess.yr <- data.yr + 1
advice.yr <- data.yr + 2

# update scenarios that hit SSB targets with results from optimiser (if needed)
scen_num <- which(grepl(paste0("then SSB(", advice.yr+1, ") = "),
                        names(FC), fixed = TRUE))
if (exists("FC2")) FC[scen_num] <- FC2

#==================================================================
# Write out forecast scenario tables
#==================================================================

writeLines("", con = "output/forecast/tab_forecasts.txt", sep = "\t")
for(i in seq(FC)){
  f <- FC[[i]]
  fc_tab <- attr(f, "tab")
  # File names from the scenario names: a reused scenario keeps its label
  fc_lab <- names(FC)[i]
  tmp <- as.data.frame(fc_tab)
  tmp <- cbind(data.frame("scenario" = fc_lab), tmp)
  tmp <- xtab2taf(tmp)
 
  fc_tab <- xtab2taf(fc_tab)
  fc_lab <- gsub(pattern = '*', replacement = "star", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = ",", replacement = "", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "+", replacement = "plus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "-", replacement = "minus", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "=", replacement = "equals", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = "%", replacement = "perc", x = fc_lab, fixed = TRUE)
  fc_lab <- gsub(pattern = " ", replacement = "_", x = fc_lab, fixed = TRUE)
  fname <- paste0("tab_fc_", fc_lab, ".csv")
  
  write.taf(fc_tab, file.path("output/forecast", fname))
  
  write.table(x = paste("\n", attr(f,"label")), file = "output/forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  write.table(x = fc_tab, file="output/forecast/tab_forecasts.txt", append = TRUE,
              row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
}

#==================================================================
# Calculate probability of falling below Blim
#==================================================================

yrs <- unlist(lapply(FC[[1]],function(x){return(x$year)}))

tmp <- lapply(names(FC),function(x){
  idx <- which(yrs %in% assess.yr)
  pr.intyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[2]]$ssb)
  
  idx <- which(yrs %in% advice.yr)
  pr.tacyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[3]]$ssb)
  
   idx <- which(yrs %in% (advice.yr+1))
  pr.ssbyr <- sum(FC[[x]][[idx]]$ssb < Blim)/length(FC[[x]][[4]]$ssb)
  
  df<-data.frame(year=assess.yr:(assess.yr+2),prob=c(pr.intyr,pr.tacyr,pr.ssbyr))
})
names(tmp) <- names(FC)

ssb_blim_prob <- do.call(rbind,tmp)
ssb_blim_prob$scenario <- gsub(".{2}$", "", row.names(ssb_blim_prob))
row.names(ssb_blim_prob) <- NULL

# reorder
ssb_blim_prob <- ssb_blim_prob[,c("scenario","year","prob")]

write.taf(c("ssb_blim_prob"), dir = "output/forecast", row.names = FALSE, quote=TRUE)

#==================================================================
# Catch options table, as in the advice sheet
#==================================================================

TAC_path <- taf.data.path("tac.csv")
TAC <- if (file.exists(TAC_path)) read.taf(TAC_path) else
  data.frame(Year = assess.yr, TAC = 3000) # PLACEHOLDER
TAC <- TAC$TAC[TAC$Year == assess.yr]

ay <- as.character(advice.yr)
ay1 <- as.character(advice.yr + 1)
catch_options <- do.call(rbind, lapply(names(FC), function(x) {
  tab <- attr(FC[[x]], "tab")
  prob <- ssb_blim_prob[ssb_blim_prob$scenario == x &
                        ssb_blim_prob$year == advice.yr + 1, "prob"]
  data.frame(scenario = x,
             catch = tab[ay, "catch:median"],
             landings = tab[ay, "Land:median"],
             discards = tab[ay, "Discard:median"],
             fbar = tab[ay, "fbar:median"],
             ssb = tab[ay1, "ssb:median"],
             ssb_change = 100 * (tab[ay1, "ssb:median"] / tab[ay, "ssb:median"] - 1),
             tac_change = 100 * (tab[ay, "catch:median"] / TAC - 1),
             prob_below_blim = prob)
}))
names(catch_options)[-1] <- c(paste0(c("catch_", "landings_", "discards_",
                                       "fbar_"), advice.yr),
                              paste0("ssb_", advice.yr + 1),
                              "ssb_change_pct", "tac_change_pct",
                              paste0("prob_ssb_below_blim_", advice.yr + 1))
write.taf(catch_options, dir = "output/forecast")

#==================================================================
# Export forecast results for MAP with step 0.1:
#==================================================================
# Table to detail catch options for F=0 to F=Fmsy at 0.1 invervals.
# Might not be needed for every stock.

mapIdx <- grepl(pattern = "^Fsq, then FMSY = [.[:digit:]]+$",
                names(FC))
# otherwise use the F range from model_forecast_SAM.R
FCmap <- if (any(mapIdx)) FC[mapIdx] else ls.forecast_F_range

res <- lapply(FCmap,
              function(x)
              {
                fc_tab <- attr(x, "tab")
                fc_lab <- attr(x, "label")
                tmp <- as.data.frame(fc_tab)
                tmp <- cbind(data.frame("scenario" = fc_lab),
                             year = row.names(tmp),
                             tmp)
                row.names(tmp) <- NULL
                return(tmp)
              })#, simplify = FALSE, USE.NAMES = FALSE)
resFmap <- do.call(rbind, res)

row.names(resFmap) <- NULL

write.taf(resFmap[resFmap$year %in% advice.yr, ],
          file = file.path("output/forecast", "Large_F_range_forecast.csv"))
