# sped_cpi_multiplier_fy20_fy23.R
# written 2026-08-27
#
# Computes the CPI multiplier used to convert FY20-FY22 dollars into FY23
# dollars, straight from the raw BLS CPI-U series (fy23 cpi calculation.xlsx)
# using the same school-year-average methodology as inflation_adjustor.R and
# cpi_inflation_calculation.R: for a given year Y, average the current year's
# HALF1 (Jan-Jun) CPI with the *prior* year's HALF2 (Jul-Dec) CPI - that
# approximates the CPI level over a school year that starts in the fall of
# Y-1 and ends in the spring of Y - then index every year to Y = 2023 so
# FY23 dollars are the common denominator, matching the "fy23" in the raw
# file's name and matching the multiplier values already hardcoded elsewhere
# in this pipeline (sped_funding_cpi.R, sped_topline_national_state_fy20_fy23.R).
# Those hardcoded values (1.165049 / 1.138837 / 1.062620 / 1.0 for
# FY20/FY21/FY22/FY23) were checked against this calculation and match
# exactly - this script just makes that calculation live and exportable
# instead of copy-pasted, which cpi_inflation_calculation.R (the FY20-only
# version) computes but never writes to a file.
#
# One methodology difference from the uploaded inflation_adjustor.R worth
# flagging: that script indexes to 2025 dollars (CPI_multiplier_2025dollars),
# but the source file only has CPI data through 2023 (no 2025 rows), so
# `avg_HALF1_HALF2lag[year==2025]` would return NA and every multiplier would
# come out NA. This script indexes to 2023 instead, matching the data that's
# actually in the file and matching the rest of this pipeline, which is
# already built around FY23 as the base year. If a newer CPI Inflation
# Adjustor file with 2024/2025 rows becomes available and the pipeline moves
# its base year forward, swap the filter/index year below.

# load ---------------------------------

options(scipen = 999)

library(tidyverse)
library(readxl)

# load the raw BLS CPI-U series -----
cpi_inflation_raw <- read_excel("sped-data/raw/fy23 cpi calculation.xlsx", skip = 11)

# Creating school-year CPI multiplier for adjusting to 2023 dollars ----

cpi_15to23 <- cpi_inflation_raw |>
  rename(year = Year) |>
  arrange(year) |>
  mutate(HALF2_lagged = lag(HALF2))

# Taking the average of both Half lags to get the annual CPI multiplier
cpi_15to23$avg_HALF1_HALF2lag <- rowMeans(cpi_15to23[c("HALF1", "HALF2_lagged")])

# Create the multiplier for adjusting to 2023 dollars
cpi_15to23$cpi_multiplier <-
  cpi_15to23$avg_HALF1_HALF2lag[cpi_15to23$year == 2023] / cpi_15to23$avg_HALF1_HALF2lag

# Keep the years this pipeline actually uses (FY20-FY23) and label them the
# way the rest of the pipeline labels years ("fy20", not "2020") -----

sped_cpi_multiplier_fy20_fy23 <- cpi_15to23 |>
  filter(year %in% c(2020, 2021, 2022, 2023)) |>
  mutate(fy = paste0("fy", str_sub(as.character(year), 3, 4))) |>
  select(fy, year, cpi_multiplier)

message(
  "CPI multipliers (to FY23 dollars):\n",
  paste(sped_cpi_multiplier_fy20_fy23$fy, round(sped_cpi_multiplier_fy20_fy23$cpi_multiplier, 6),
        sep = " = ", collapse = "\n")
)

# Export -----

write_csv(sped_cpi_multiplier_fy20_fy23, "sped-data/processed/sped_cpi_multiplier_fy20_fy23.csv")

# Tidy workplace -----

rm(cpi_inflation_raw, cpi_15to23)