
# TAF template for a SAM assessment

## note to developers

before modifying these scripts please install or update the packages
they use, as shown under Running below.

This is a template for a Stock Assessment Model (SAM) assessment using
the TAF (Transparent Assessment Framework). It provides a structured
approach to conducting stock assessments, ensuring transparency and
reproducibility.

## Flow

``` mermaid
flowchart TD
  A[("boot/initial/data<br/>Lowestoft files, model.cfg,<br/>reference points, TAC, stock info")]
  B[("boot/data")]
  C[("data<br/>input tables, SAM data")]
  D[("model<br/>SAM fit, retrospective,<br/>leave-one-out, residuals, forecast")]
  E[("output<br/>result and forecast tables,<br/>FLStock, SAG upload file")]
  F[("report<br/>plots, rounded tables")]
  P0("taf.boot()")
  P1("data.R")
  P2("model.R")
  P3("output.R")
  P4("report.R")
  A --> P0 --> B --> P1 --> C --> P2 --> D --> P3 --> E --> P4 --> F
  B -.-> P2
  B -.-> P3
  C -.-> P3
  C -.-> P4
  D -.-> P4
  classDef boot fill:#cfd6e0,stroke:#8a96a8,color:#1f2933
  classDef data fill:#bcd4f0,stroke:#6f95c4,color:#1f2933
  classDef model fill:#b6e0d6,stroke:#5fa596,color:#1f2933
  classDef output fill:#c8e6b5,stroke:#7cad62,color:#1f2933
  classDef report fill:#f3d9a6,stroke:#c9a35a,color:#1f2933
  class A,B boot
  class C data
  class D model
  class E output
  class F report
```

Cylinders are data, rounded boxes are the steps that read and write
them. Solid arrows: the main flow. Dashed arrows: a step also reads an
earlier folder. Data only flow forwards.

## Running

``` r
library(TAF)
install.packages(deps(installed = FALSE),
                 repos = c("https://fishfollower.r-universe.dev",
                           "https://ices-tools-prod.r-universe.dev",
                           "https://flr.r-universe.dev",
                           "https://cloud.r-project.org"))
taf.boot()
source.all()
```

This installs the R packages that the scripts use and that are missing,
from CRAN or the r-universes above. Run it in an assessment made from
the template, after removing the scripts it does not use.

## Software

- stockassessment 0.12.1 (2026-10-30), from
  <https://fishfollower.r-universe.dev>. Nielsen A, Berg CW (2014).
  Estimation of time-varying selectivity in stock assessments using
  state-space models. Fisheries Research 158: 96-101.
  <https://doi.org/10.1016/j.fishres.2014.01.014>

## Details

Each step runs its own scripts (rounded boxes), which read data
(cylinders) and write the next folder.

### data.R

``` mermaid
flowchart TD
  A[("boot/data/sam_data<br/>Lowestoft files")] --> S("data_SAM.R")
  S --> B[("data<br/>catage, wcatch, wdiscards, wlandings, landfrac,<br/>wstock, maturity, natmort, propf, propm,<br/>survey_* (csv); data.stock.rds")]
  classDef boot fill:#cfd6e0,stroke:#8a96a8,color:#1f2933
  classDef data fill:#bcd4f0,stroke:#6f95c4,color:#1f2933
  classDef model fill:#b6e0d6,stroke:#5fa596,color:#1f2933
  classDef output fill:#c8e6b5,stroke:#7cad62,color:#1f2933
  classDef report fill:#f3d9a6,stroke:#c9a35a,color:#1f2933
  class A boot
  class B data
```

### model.R

``` mermaid
flowchart TD
  A[("data/data.stock.rds")] --> S1("model_assessment_SAM.R")
  C[("boot/data/sam_config/model.cfg")] --> S1
  S1 --> M1[("model/assessment<br/>fit, retro_fit, leaveoneout,<br/>residuals, procres (jitter)")]
  M1 --> S2("model_forecast_SAM.R")
  R[("boot/data<br/>reference_points.csv, tac.csv")] --> S2
  S2 --> M2[("model/forecast<br/>forecast, forecast_F_range")]
  classDef boot fill:#cfd6e0,stroke:#8a96a8,color:#1f2933
  classDef data fill:#bcd4f0,stroke:#6f95c4,color:#1f2933
  classDef model fill:#b6e0d6,stroke:#5fa596,color:#1f2933
  classDef output fill:#c8e6b5,stroke:#7cad62,color:#1f2933
  classDef report fill:#f3d9a6,stroke:#c9a35a,color:#1f2933
  class A data
  class C,R boot
  class M1,M2 model
```

### output.R

``` mermaid
flowchart TD
  M1[("model/assessment")] --> S1("output_assessment_SAM.R")
  S1 --> O1[("output/assessment<br/>tab_summary, fatage, natage, partab,<br/>mohns_rho, modtab, qtab, qtab_sd")]
  M2[("model/forecast")] --> S2("output_forecast_SAM.R")
  S2 --> O2[("output/forecast<br/>catch option tables,<br/>ssb_blim_prob, Large_F_range_forecast")]
  M1 --> S3("output_FLStock_SAM.R")
  S3 --> O3[("output<br/>FLStock objects")]
  M1 --> S4("output_SAG_SAM.R")
  M2 --> S4
  D[("data<br/>catch tables")] --> S4
  B[("boot/data<br/>stock_info.csv, reference_points.csv")] --> S2
  B --> S3
  B --> S4
  S4 --> O4[("output/sag<br/>SAG upload file")]
  classDef boot fill:#cfd6e0,stroke:#8a96a8,color:#1f2933
  classDef data fill:#bcd4f0,stroke:#6f95c4,color:#1f2933
  classDef model fill:#b6e0d6,stroke:#5fa596,color:#1f2933
  classDef output fill:#c8e6b5,stroke:#7cad62,color:#1f2933
  classDef report fill:#f3d9a6,stroke:#c9a35a,color:#1f2933
  class M1,M2 model
  class O1,O2,O3,O4 output
  class D data
  class B boot
```

### report.R

``` mermaid
flowchart TD
  M1[("model/assessment")] --> S("report_assessment_SAM.R")
  O1[("output/assessment")] --> S
  D[("data")] --> S
  S --> R[("report/assessment<br/>input, summary, diagnostic and survey plots;<br/>rounded tables")]
  classDef boot fill:#cfd6e0,stroke:#8a96a8,color:#1f2933
  classDef data fill:#bcd4f0,stroke:#6f95c4,color:#1f2933
  classDef model fill:#b6e0d6,stroke:#5fa596,color:#1f2933
  classDef output fill:#c8e6b5,stroke:#7cad62,color:#1f2933
  classDef report fill:#f3d9a6,stroke:#c9a35a,color:#1f2933
  class M1 model
  class O1 output
  class D data
  class R report
```
