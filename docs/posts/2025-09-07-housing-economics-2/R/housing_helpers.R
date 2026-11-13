# Shared helpers for the Glaeser & Gyourko (2018) replication.
# Implements the "Tobin's q for housing" cost model and an era-aware AHS loader.
# Sourced by both R/build_ahs_extract.R and housing-economics-2.Rmd.

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

# ---- Cost model (from the figures quoted in the paper) -----------------------

# CPI-U, annual average (1982-84 = 100). Used to move the real cost curve, which
# the paper states in constant 2010 dollars, into each survey year's nominal $,
# so it is comparable with AHS VALUE / Zillow ZHVI (both nominal current $).
CPI_U <- c(
  `1980` = 82.4,  `1985` = 107.6, `1999` = 166.6, `2005` = 195.3,
  `2007` = 207.342, `2010` = 218.056, `2013` = 232.957, `2015` = 237.017,
  `2024` = 313.689
)

cpi <- function(year) {
  yr <- as.character(round(year))
  if (all(yr %in% names(CPI_U))) return(unname(CPI_U[yr]))
  # linear interpolation for years not in the table (e.g. Zillow annual panel)
  approx(as.numeric(names(CPI_U)), CPI_U, xout = year, rule = 2)$y
}

# Economy-quality physical construction cost, $/sqft, constant 2010$.
# Anchored on the three points the paper cites: ~$83 (1980), ~$66 (~2000),
# ~$85 (2015). Linear interpolation in between (paper stresses costs are flat).
real_cost_per_sqft <- function(year, quality = c("economy", "high")) {
  quality <- match.arg(quality)
  anchors_x <- c(1980, 2000, 2015)
  anchors_y <- c(83, 66, 85)
  econ <- approx(anchors_x, anchors_y, xout = year, rule = 2)$y
  # "high construction cost market" band: paper's $265k / $200k => ~ x1.33.
  if (quality == "high") econ * (265 / 200) else econ
}

# Minimum profitable production cost, nominal dollars in `year`.
# MPPC = (L + CC) * EP, with L = 0.25*CC and EP = 1.17  =>  MPPC = 1.4625 * CC.
mppc <- function(sqft, year, quality = "economy") {
  cc_real2010 <- sqft * real_cost_per_sqft(year, quality)
  cc_nominal  <- cc_real2010 * cpi(year) / cpi(2010)
  1.4625 * cc_nominal
}

# R.S. Means living-area grid used in the paper. A unit's reported square footage
# is snapped DOWN to the nearest grid point (<=700 -> 600), matching the paper's
# "less than or equal to 700 sq ft -> 600 sq ft economy cost" rule.
SQFT_GRID <- c(600, 800, 1000, 1200, 1400, 1600, 1800, 2000,
               2400, 2800, 3200, 3600, 4000)

snap_sqft <- function(sqft) {
  idx <- findInterval(sqft, SQFT_GRID)
  idx[idx < 1] <- 1                       # anything <=700 -> 600 grid point
  SQFT_GRID[idx]
}

# Four q-ratio bins from the paper.
Q_LEVELS <- c("<= 0.75", "0.75 - 1.25", "1.25 - 2", "> 2")
q_bin <- function(ratio) {
  cut(ratio, breaks = c(-Inf, 0.75, 1.25, 2, Inf),
      labels = Q_LEVELS, right = TRUE)
}

# ---- AHS loader (old design, 1985-2013) -------------------------------------

# File paths and the per-year weight variable (WEIGHT pre-2005, WGT90GEO after).
ahs_files <- function(data_path) {
  tibble::tribble(
    ~year, ~path, ~weight_var,
    1985L, file.path(data_path, "AHS_yearly/1985/ahs1985n.sas7bdat"),    "WEIGHT",
    1999L, file.path(data_path, "AHS_yearly/1999/ahs1999n.sas7bdat"),    "WEIGHT",
    2005L, file.path(data_path, "AHS_yearly/2005/ahs2005n.sas7bdat"),    "WGT90GEO",
    2007L, file.path(data_path, "AHS_sas_snapshot/ahs2007n.sas7bdat"),   "WGT90GEO",
    2013L, file.path(data_path, "AHS_yearly/2013/ahs2013n.sas7bdat"),    "WGT90GEO"
  )
}

# Read one old-design AHS national PUF, returning harmonized, filtered single-
# family-detached owner-occupied units with a computed q-ratio.
load_ahs <- function(path, year, weight_var) {
  raw <- haven::read_sas(
    path,
    col_select = all_of(c("VALUE", "UNITSF", "NUNIT2", "TENURE", "SMSA", weight_var))
  )
  raw |>
    rename(value = VALUE, sqft = UNITSF, structure = NUNIT2,
           tenure = TENURE, smsa = SMSA, weight = all_of(weight_var)) |>
    mutate(
      across(c(value, sqft, weight), as.numeric),
      structure = as.character(structure),
      tenure    = as.character(tenure),
      smsa      = str_trim(as.character(smsa)),
      year      = year
    ) |>
    filter(
      structure == "1",                 # single-family detached
      tenure == "1",                    # owner-occupied
      value > 0, sqft > 0,
      smsa != "", !str_starts(smsa, "999"), # drop suppressed / non-metro placeholders
      !is.na(weight)
    ) |>
    mutate(
      sqft_grid   = snap_sqft(sqft),
      mppc        = mppc(sqft_grid, year, "economy"),
      ratio       = value / mppc,
      qbin        = q_bin(ratio)
    ) |>
    select(year, smsa, value, sqft, sqft_grid, weight, mppc, ratio, qbin)
}

# Decode AHS 1980 SMSA codes -> metro names via the repo's NHGIS crosswalk.
# The crosswalk has duplicate rows per code, so keep one row per code.
load_smsa_crosswalk <- function(data_path) {
  readxl::read_xlsx(file.path(data_path, "nhgis_1980_block_shapefile_coverage.xlsx")) |>
    transmute(
      smsa     = str_pad(as.character(`SMSA Code`), 4, pad = "0"),
      smsa_name = `SMSA Name`
    ) |>
    distinct(smsa, .keep_all = TRUE)
}

# Build the combined extract across all available old-design years.
build_ahs_extract <- function(data_path) {
  files <- ahs_files(data_path)
  purrr::pmap_dfr(
    list(files$path, files$year, files$weight_var),
    function(p, y, w) {
      message("Loading AHS ", y, " ...")
      load_ahs(p, y, w)
    }
  )
}
