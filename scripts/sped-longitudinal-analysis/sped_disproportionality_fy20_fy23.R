# sped_disproportionality_fy20_fy23.R
# written 2026-08-27
#
# Builds a district-year panel (FY20-FY23) with per-pupil special education
# funding and the disproportionality metric from the original FY20-only
# pipeline (1_sped_dist_expenditures.R / 2_sped_dist_revenue.R /
# 4_sped_rev_exp_enroll_join.R) - local share of special education spending
# vs. local share of everything else - ported over so it can be computed for
# every year, not just FY20.
#
# This reads directly from the already-cleaned per-year files this pipeline
# already produces (ccd_sped_expenditures_fyXX.csv, sped_revenue_long.csv,
# sped_enroll_fyXX_clean.csv) rather than going through
# sped_exp_rev_enroll_join.csv. That file requires a district to have valid
# FY20 *and* FY23 enrollment simultaneously (see its `filter(sped_enroll_fy20 > 0)`
# / `filter(sped_enroll_fy23 > 0)` lines), which is the right call for a
# FY20-vs-FY23 comparison but wrongly excludes, say, a district with good
# FY21/FY22 data but missing FY20 data from ever showing up in a FY21 or
# FY22 topline number. Building each year's panel from its own per-year
# sources instead means every year stands on its own - which is also why the
# number of usable districts/states differs from year to year below (FY20
# has less usable expenditure data than FY21-23; that's a real feature of
# the raw data, not a bug - see the "not included" notes in the per-year
# expenditure scripts).
#
# NOTE ON COLORADO: this script applies the same 16-state exclusion list
# already used in sped_exp_rev_enroll_join.R, which excludes Colorado
# (negligible dedicated state sped revenue in the source data / no reliable
# district-level enrollment - see sped_enroll_colorado_fy21_fy23.R). If
# Colorado's enrollment gap gets fully resolved, remove "Colorado" from
# state_exclude below and re-run.

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# the 16-state exclusion list already used in sped_exp_rev_enroll_join.R -
# states with no or negligible dedicated state special education revenue
state_exclude <- c(
  "Alaska", "District of Columbia", "Kentucky", "New Hampshire", "North Carolina",
  "Ohio", "Oklahoma", "Oregon", "Rhode Island", "Tennessee", "Texas", "Wyoming",
  "Louisiana", "New Mexico", "Colorado", "Hawaii"
)

# load the shared source files -----

# per-district revenue, all 4 years in one long file with a "year" column
sped_revenue_long <- read_csv("sped-data/processed/sped-revenue/sped_revenue_long.csv") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0"))

# build one year's disproportionality panel -----
#
# exp_path/enroll_path point at that year's already-cleaned expenditure and
# enrollment files; rev_year filters sped_revenue_long to that year.

build_year_panel <- function(fy_label, rev_year, exp_path, enroll_path, enroll_col) {
  
  exp_clean <- read_csv(exp_path) |>
    mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
    select(dist_id, state, district, total_enroll, tcurelsc, current_sped_exp)
  
  rev_clean <- sped_revenue_long |>
    filter(year == rev_year) |>
    select(dist_id, state_sped_rev, federal_sped_rev, state_fed_sped_rev,
           state_rev_v92_adjusted, federal_rev_v92_adjusted)
  
  enroll_clean <- read_csv(enroll_path) |>
    select(dist_id, sped_enroll = all_of(enroll_col))
  
  exp_clean |>
    left_join(rev_clean, by = "dist_id") |>
    left_join(enroll_clean, by = "dist_id") |>
    filter(!is.na(sped_enroll), sped_enroll > 0) |>
    filter(!(state %in% state_exclude)) |>
    mutate(
      # per-pupil special education funding -----
      sped_exp_pp = current_sped_exp / sped_enroll,
      state_sped_rev_pp = state_sped_rev / sped_enroll,
      federal_sped_rev_pp = federal_sped_rev / sped_enroll,
      state_fed_sped_rev_pp = state_fed_sped_rev / sped_enroll,
      sped_local_pp_obligation = sped_exp_pp - state_fed_sped_rev_pp,
      
      # disproportionality: local share of sped spending vs. local share of
      # everything else - ported from 4_sped_rev_exp_enroll_join.R
      state_sped_pct = state_sped_rev / current_sped_exp,
      federal_sped_pct = federal_sped_rev / current_sped_exp,
      local_sped_pct = 1 - state_sped_pct - federal_sped_pct,
      state_rev_no_sped = state_rev_v92_adjusted - state_sped_rev,
      federal_rev_no_sped = federal_rev_v92_adjusted - federal_sped_rev,
      current_exp_no_sped = tcurelsc - current_sped_exp,
      state_no_sped_pct = state_rev_no_sped / current_exp_no_sped,
      federal_no_sped_pct = federal_rev_no_sped / current_exp_no_sped,
      local_no_sped_pct = 1 - state_no_sped_pct - federal_no_sped_pct,
      disproportionate = local_sped_pct > local_no_sped_pct,
      
      fy = fy_label
    ) |>
    filter(is.finite(sped_local_pp_obligation))
}

# FY20 -----
sped_disprop_fy20 <- build_year_panel(
  fy_label = "fy20", rev_year = 2020,
  exp_path = "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy20.csv",
  enroll_path = "sped-data/processed/sped-enroll/sped_enroll_fy20_clean.csv",
  enroll_col = "sped_enroll_fy20"
)

# FY21 -----
sped_disprop_fy21 <- build_year_panel(
  fy_label = "fy21", rev_year = 2021,
  exp_path = "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy21.csv",
  enroll_path = "sped-data/processed/sped-enroll/sped_enroll_fy21_clean.csv",
  enroll_col = "sped_enroll_fy21"
)

# FY22 -----
sped_disprop_fy22 <- build_year_panel(
  fy_label = "fy22", rev_year = 2022,
  exp_path = "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy22.csv",
  enroll_path = "sped-data/processed/sped-enroll/sped_enroll_fy22_clean.csv",
  enroll_col = "sped_enroll_fy22"
)

# FY23 -----
sped_disprop_fy23 <- build_year_panel(
  fy_label = "fy23", rev_year = 2023,
  exp_path = "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy23.csv",
  enroll_path = "sped-data/processed/sped-enroll/sped_enroll_fy23_clean.csv",
  enroll_col = "sped_enroll_fy23"
)

# Rbind the data -----

sped_disproportionality_panel <- bind_rows(
  sped_disprop_fy20, sped_disprop_fy21, sped_disprop_fy22, sped_disprop_fy23
)

# Export the data -----

write_csv(sped_disproportionality_panel, "sped-data/processed/sped_disproportionality_panel_fy20_fy23.csv")

# Tidy workplace -----

rm(sped_disprop_fy20, sped_disprop_fy21, sped_disprop_fy22, sped_disprop_fy23,
   sped_revenue_long, build_year_panel)