# WKTSAT
Workshop on TAF Stock Assessment Templates

This repository contains placeholder, initially, for standard template scripts to perform well defined tasks required for a stock assessment using Stock Synthesis or SAM.

Examples include, running a model, running a retropsective, create the SAG output, creating an FLStock object, generating a presentation, generating a basic standard report.


## Starting Point

* Start from RDBES / Intercatch files
* Preformed survey indices
* ss ctrl files

## Developers:

* model_SS3.R - Laura
* data_SAM.R - Sofie
* output_SAG.R - Jean-Baptiste
* model_forecast - Klaas
* model_SAM.R - Klaas
* model_retrospective - Jean-Baptiste
* report_wg_presentation - Colin
* data_FLSAM.R - Petur

Additional tasks:

* investigate stockplotr:: convert_output, create_rda
* investigate dev containers

Consider scripts -> functions, candidates are:
* output_SAG.R

---
### Structure of the code

####src/SS3/data.R
Prepares the data to run the assessment, saved in the src/SS3/data folder as CSV files and RData file.

####src/SS3/model.R
RUN SS3 model fit, retrospective and jitter

####src/SS3/model_forecast.R
RUN the short-term forecast using SS3 (grid of F-multipliers relative to the Fsq to cover alternative scenarios in the catch option table) and generate default tables and figures for the advice sheet and the report.

## Running

```r
library(TAF)
install.packages(deps(installed = FALSE),
                 repos = c("https://fishfollower.r-universe.dev",
                           "https://ices-tools-prod.r-universe.dev",
                           "https://flr.r-universe.dev",
                           "https://cloud.r-project.org"))
taf.boot()
source.all()
```

This installs the R packages that the scripts use and that are missing, from
CRAN or the r-universes above. Run it in an assessment made from the
template, after removing the scripts it does not use.

## Software

- stockassessment 0.12.1 (2026-10-30), from
  <https://fishfollower.r-universe.dev>. Nielsen A, Berg CW (2014). Estimation
  of time-varying selectivity in stock assessments using state-space models.
  Fisheries Research 158: 96-101. <https://doi.org/10.1016/j.fishres.2014.01.014>
