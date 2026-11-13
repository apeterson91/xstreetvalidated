# One-time prep: read the large AHS SAS files once, filter to the analysis
# sample, compute the q-ratio, and cache a compact extract so re-knitting the
# post never re-reads the multi-GB SAS files.
#
# Run from the post directory:  Rscript R/build_ahs_extract.R

suppressPackageStartupMessages(library(purrr))

source(file.path("R", "housing_helpers.R"))

ahs <- build_ahs_extract("data")

out <- file.path("data", "ahs_extract.rds")
saveRDS(ahs, out)
message("Wrote ", out, " : ", nrow(ahs), " rows across years ",
        paste(sort(unique(ahs$year)), collapse = ", "))
