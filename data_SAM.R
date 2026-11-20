# Read the Lowestoft files, set up the SAM data, write TAF data tables

# Before: sam_data/*.dat or *.txt (boot/data)
# After:  catage.csv, wcatch.csv, wdiscards.csv, wlandings.csv, landfrac.csv,
#         wstock.csv, maturity.csv, natmort.csv, propf.csv, propm.csv,
#         survey_<name>.csv, data.stock.rds (data)

library(TAF)
taf.library(stockassessment)

mkdir("data")

# Two sets of Lowestoft files are accepted, as .dat or .txt:
# - the files SAM reads directly: cn, cw, dw, lw, lf, sw, mo, nm, pf, pm,
#   survey
# - landings and discards separately: ln, lw, la, dn, dw, sw, mo, nm, pf, pm,
#   tun. Landings numbers are SOP corrected and ages above plus_group are
#   added into the plus group.
plus_group <- 10

files <- dir(taf.data.path("sam_data"), pattern = "\\.(dat|txt)$",
             full.names = TRUE, ignore.case = TRUE)
names(files) <- tolower(sub("\\.[^.]*$", "", basename(files)))

survey_file <- intersect(c("survey", "tun"), names(files))[1]
if (is.na(survey_file)) stop("no survey or tun file in boot/data/sam_data")
surveys <- read.ices(files[[survey_file]])
names(surveys) <- trimws(names(surveys))

# read.ices cannot read files with a single age column, such as la
read_lowestoft <- function(file) {
  head <- scan(file, skip = 2, n = 4, quiet = TRUE)
  if (head[3] != head[4]) return(read.ices(file))
  years <- head[1]:head[2]
  x <- scan(file, skip = 5, n = length(years), quiet = TRUE)
  matrix(x, ncol = 1, dimnames = list(years, head[3]))
}
stock <- lapply(files[setdiff(names(files), survey_file)], read_lowestoft)

if ("cn" %in% names(stock)) {
  needed <- c("cn", "cw", "dw", "lw", "lf", "sw", "mo", "nm", "pf", "pm")
  missing <- setdiff(needed, names(stock))
  if (length(missing) > 0) stop("missing files: ", toString(missing))
  sam_input <- stock[needed]
} else {
  needed <- c("ln", "lw", "la", "dn", "dw", "sw", "mo", "nm", "pf", "pm")
  missing <- setdiff(needed, names(stock))
  if (length(missing) > 0) stop("missing files: ", toString(missing))

  sop <- rowSums(stock$ln * stock$lw) / stock$la[, 1]
  stock$ln <- sweep(stock$ln, 1, sop, "/")

  all_ages <- as.numeric(colnames(stock$ln))
  # Data that stop below plus_group already end in a plus group
  plus_group <- min(plus_group, max(all_ages))
  ages <- as.character(all_ages[all_ages <= plus_group])
  above <- as.character(all_ages[all_ages >= plus_group])
  pg <- as.character(plus_group)

  plus_n <- function(n) {
    x <- n[, ages, drop = FALSE]
    x[, pg] <- rowSums(n[, above, drop = FALSE])
    x
  }
  plus_w <- function(w, n) {
    x <- w[, ages, drop = FALSE]
    total <- rowSums(n[, above, drop = FALSE])
    # With no fish in the plus group, keep the weight given for that age
    x[, pg] <- ifelse(total == 0, w[, pg],
                      rowSums(w[, above, drop = FALSE] *
                              n[, above, drop = FALSE]) / total)
    x
  }

  ln <- plus_n(stock$ln)
  dn <- plus_n(stock$dn)
  lw <- plus_w(stock$lw, stock$ln)
  dw <- plus_w(stock$dw, stock$dn)
  cn <- ln + dn
  sam_input <- list(
    cn = cn,
    cw = ifelse(cn == 0, 0, (lw * ln + dw * dn) / cn),
    dw = dw,
    lw = lw,
    lf = ifelse(cn == 0, 0, ln / cn),
    sw = plus_w(stock$sw, stock$ln + stock$dn),
    mo = stock$mo[, ages, drop = FALSE],
    nm = stock$nm[, ages, drop = FALSE],
    pf = stock$pf[, ages, drop = FALSE],
    pm = stock$pm[, ages, drop = FALSE]
  )
  sam_input$cw[is.na(sam_input$cw)] <- 0
  sam_input$lf[is.na(sam_input$lf)] <- 0
}

sam_data <- setup.sam.data(
  surveys = surveys,
  residual.fleet = sam_input$cn,
  prop.mature = sam_input$mo,
  stock.mean.weight = sam_input$sw,
  catch.mean.weight = sam_input$cw,
  dis.mean.weight = sam_input$dw,
  land.mean.weight = sam_input$lw,
  prop.f = sam_input$pf,
  prop.m = sam_input$pm,
  natural.mortality = sam_input$nm,
  land.frac = sam_input$lf
)

tables <- list(
  catage = sam_input$cn, wcatch = sam_input$cw, wdiscards = sam_input$dw,
  wlandings = sam_input$lw, landfrac = sam_input$lf, wstock = sam_input$sw,
  maturity = sam_input$mo, natmort = sam_input$nm, propf = sam_input$pf,
  propm = sam_input$pm
)
for (s in names(surveys)) {
  name <- paste0("survey_", gsub("[^a-z0-9]+", "_", tolower(s)))
  tables[[name]] <- surveys[[s]]
}
for (name in names(tables)) {
  write.taf(xtab2taf(tables[[name]]), file = paste0(name, ".csv"),
            dir = "data")
}
saveRDS(sam_data, "data/data.stock.rds")
