## Check the SAG and ASD upload files against the DATSU checks

## Before: output/sag/<stock>_sag_upload.csv, output/asd/<stock>_asd_upload.xml
## After: output/qc/datsu_checks.csv, output/qc/datsu_failures.csv

# The checks run locally with icesDatsuQC: only the check definitions are
# downloaded from DATSU, no data are sent. If DATSU cannot be reached, a file
# is recorded as not checked and the run carries on.

library(TAF)
library(icesDatsuQC)

mkdir("output/qc")

# DATSU dataset versions and record types. SAG is 126 (as in
# icesSAG::uploadStock). ASD has no DATSU dataset yet.
uploads <- list(
  SAG = list(file = dir("output/sag", "_sag_upload\\.csv$", full.names = TRUE),
             dataset = 126, records = c("AA", "AF")),
  ASD = list(file = dir("output/asd", "_asd_upload\\.xml$", full.names = TRUE),
             dataset = NA, records = NA)
)

checks <- data.frame()
failures <- data.frame(upload = character(0), record = character(0),
                       Linenumber = integer(0), check_Description = character(0),
                       errorType = character(0))
for (upload in names(uploads)) {
  u <- uploads[[upload]]
  for (record in u$records) {
    status <- if (length(u$file) == 0) {
      "not checked: no file"
    } else if (is.na(u$dataset)) {
      "not checked: no DATSU dataset"
    } else {
      # runQCChecks reads a file of one record type, so split the upload
      lines <- readLines(u$file)
      lines <- lines[sub(",.*", "", gsub('"', "", lines)) == record]
      tmp <- tempfile(fileext = ".csv")
      writeLines(lines, tmp)
      qc <- tryCatch(icesDatsu::getListQCChecks(u$dataset, record),
                     error = function(e) NULL)
      res <- if (!is.data.frame(qc)) {
        simpleError("DATSU did not return its checks")
      } else if (length(lines) == 0) {
        simpleError(paste("no", record, "records in the file"))
      } else {
        tryCatch(runQCChecks(tmp, u$dataset, record), error = function(e) e)
      }
      if (inherits(res, "error")) {
        paste("not checked:", conditionMessage(res))
      } else if (is.null(res)) {
        "passed"
      } else {
        failures <- rbind(failures, data.frame(upload = upload,
                                               record = record, res))
        paste(nrow(res), "failed")
      }
    }
    checks <- rbind(checks, data.frame(upload = upload,
                                       file = basename(u$file[1]),
                                       dataset = u$dataset, record = record,
                                       status = status))
  }
}

write.taf(checks, "datsu_checks.csv", dir = "output/qc")
write.taf(failures, "datsu_failures.csv", dir = "output/qc")
