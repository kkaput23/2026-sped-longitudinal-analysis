# sped_disproportionality_panel_cpi_fy20_fy23.R
# written 2026-08-27
#
# Multiplies the FY20-FY22 dollar figures in the district-year
# disproportionality panel (sped_disproportionality_panel_fy20_fy23.csv, built
# by sped_disproportionality_fy20_fy23.R) by the CPI multiplier computed in
# sped_cpi_multiplier_fy20_fy23.R, so every year's spending and revenue can be
# compared in FY23 dollars instead of nominal dollars. FY23 itself gets a
# multiplier of 1.0 (it's already the base year), so its "_adjusted" columns
# equal its nominal ones.
#
# This is the longitudinal, long-format equivalent of sped_funding_cpi.R,
# which does the same multiplication but on the FY20-only wide join
# (sped_exp_rev_enroll_join.csv) with hardcoded multiplier constants. Here the
# multiplier comes from a join against sped_cpi_multiplier_fy20_fy23.R's
# output instead of being retyped, and it applies to all 4 years at once via
# fy instead of one mutate() block per year.
#
# Only the raw dollar totals get multiplied directly (current_sped_exp,
# tcurelsc, state_sped_rev, federal_sped_rev, state_fed_sped_rev,
# state_rev_v92_adjusted, federal_rev_v92_adjusted). The per-pupil figures
# and the disproportionality percentages are then recomputed from those
# adjusted totals rather than multiplied themselves - per-pupil dollar
# amounts need the multiplier (they're still dollars), but the
# disproportionality shares/percentages don't, since scaling every dollar
# amount in a ratio by the same multiplier leaves the ratio unchanged. The
# disproportionate flag is carried over unchanged for the same reason - it's
# still worth double-checking that logic if this script is extended later.

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

sped_disproportionality_panel <- read_csv("sped-data/processed/sped_disproportionality_panel_fy20_fy23.csv")

sped_cpi_multiplier_fy20_fy23 <- read_csv("sped-data/processed/sped_cpi_multiplier_fy20_fy23.csv") |>
  select(fy, cpi_multiplier)

# Join the multiplier on and adjust the dollar totals -----

sped_disproportionality_panel_cpi <- sped_disproportionality_panel |>
  left_join(sped_cpi_multiplier_fy20_fy23, by = "fy") |>
  mutate(
    # adjusted (FY23 $) totals -----
    tcurelsc_adjusted = tcurelsc * cpi_multiplier,
    current_sped_exp_adjusted = current_sped_exp * cpi_multiplier,
    state_sped_rev_adjusted = state_sped_rev * cpi_multiplier,
    federal_sped_rev_adjusted = federal_sped_rev * cpi_multiplier,
    state_fed_sped_rev_adjusted = state_fed_sped_rev * cpi_multiplier,
    state_rev_v92_adjusted_real = state_rev_v92_adjusted * cpi_multiplier,
    federal_rev_v92_adjusted_real = federal_rev_v92_adjusted * cpi_multiplier,
    
    # adjusted (FY23 $) per-pupil figures, recomputed from the adjusted totals -----
    sped_exp_pp_adjusted = current_sped_exp_adjusted / sped_enroll,
    state_sped_rev_pp_adjusted = state_sped_rev_adjusted / sped_enroll,
    federal_sped_rev_pp_adjusted = federal_sped_rev_adjusted / sped_enroll,
    state_fed_sped_rev_pp_adjusted = state_fed_sped_rev_adjusted / sped_enroll,
    sped_local_pp_obligation_adjusted = sped_exp_pp_adjusted - state_fed_sped_rev_pp_adjusted
    
    # disproportionality shares (state_sped_pct, federal_sped_pct,
    # local_sped_pct, state_no_sped_pct, federal_no_sped_pct,
    # local_no_sped_pct) and the disproportionate flag are NOT recomputed -
    # they're ratios of same-year dollar figures, and every dollar figure in
    # a given year is scaled by the same multiplier, so the ratios (and the
    # comparison behind the disproportionate flag) come out identical either
    # way. The nominal versions already in the panel are the correct ones to
    # use here too.
  )

# Export -----

write_csv(sped_disproportionality_panel_cpi, "sped-data/processed/sped_disproportionality_panel_cpi_fy20_fy23.csv")

# Tidy workplace -----

rm(sped_disproportionality_panel, sped_cpi_multiplier_fy20_fy23)

