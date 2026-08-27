# sped_topline_national_state_fy20_fy23_cpi_summary.R
# written 2026-08-27
#
# Updated national and state topline summaries using the CPI-adjusted
# (FY23-dollar) district panel from sped_disproportionality_panel_cpi_fy20_fy23.R
# instead of nominal dollars - the inflation-adjusted counterpart to
# sped_topline_national_state_fy20_fy23.R, following the same
# no_cpi_summary / cpi_summary pattern as
# sped_funding_fy20_fy23_state_national_analysis.R, but across all four years
# (long format, grouped by fy / fy+state) instead of just FY20 vs. FY23.
#
# Every table below reports BOTH the nominal dollar figures and the FY23-$
# adjusted figures side by side, so it's easy to see how much of any
# year-over-year change is real versus just inflation. As with the earlier
# topline script, the state-level tables and the national trend table are
# restricted to the 21 states with usable data in all four years, so a state
# entering/leaving the sample can't be mistaken for a funding change; the
# national by-year table (every state with usable data that year) is not
# restricted, since that's the right table for "what did this year look like
# on its own."
#
# NOTE: this inherits the disproportionality panel's Colorado exclusion (see
# sped_disproportionality_fy20_fy23.R's header) - re-run the whole chain
# (sped_disproportionality_fy20_fy23.R -> sped_cpi_multiplier_fy20_fy23.R ->
# sped_disproportionality_panel_cpi_fy20_fy23.R -> this script) if/when that
# gets resolved.

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

sped_disproportionality_panel_cpi <- read_csv("sped-data/processed/sped_disproportionality_panel_cpi_fy20_fy23.csv")

# the 21 states with usable data in every year fy20-fy23 - same derivation as
# sped_topline_national_state_fy20_fy23.R, re-derived here so this script can
# run on its own without depending on that script having just run

consistent_states <- sped_disproportionality_panel_cpi |>
  distinct(fy, state) |>
  count(state) |>
  filter(n == 4) |>
  pull(state)

sped_disproportionality_panel_cpi_consistent21 <- sped_disproportionality_panel_cpi |>
  filter(state %in% consistent_states)

# National summary, by year: every state with usable data that year -----
# (nominal + FY23-$ adjusted, side by side)

sped_national_fy20_fy23_cpi_summary <- sped_disproportionality_panel_cpi |>
  group_by(fy) |>
  summarise(
    n_states = n_distinct(state),
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    # nominal
    current_sped_exp = sum(current_sped_exp, na.rm = T),
    state_sped_rev = sum(state_sped_rev, na.rm = T),
    federal_sped_rev = sum(federal_sped_rev, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev, na.rm = T),
    # FY23-$ adjusted
    current_sped_exp_adjusted = sum(current_sped_exp_adjusted, na.rm = T),
    state_sped_rev_adjusted = sum(state_sped_rev_adjusted, na.rm = T),
    federal_sped_rev_adjusted = sum(federal_sped_rev_adjusted, na.rm = T),
    state_fed_sped_rev_adjusted = sum(state_fed_sped_rev_adjusted, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T)
  ) |>
  mutate(
    # nominal per-pupil
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_sped_pp_funding = state_sped_rev / sped_enroll,
    federal_sped_pp_funding = federal_sped_rev / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    # FY23-$ adjusted per-pupil
    total_sped_pp_expenditure_adjusted = current_sped_exp_adjusted / sped_enroll,
    state_sped_pp_funding_adjusted = state_sped_rev_adjusted / sped_enroll,
    federal_sped_pp_funding_adjusted = federal_sped_rev_adjusted / sped_enroll,
    state_fed_sped_pp_funding_adjusted = state_fed_sped_rev_adjusted / sped_enroll,
    local_sped_pp_obligation_adjusted = total_sped_pp_expenditure_adjusted - state_fed_sped_pp_funding_adjusted,
    pct_disproportionate = n_disproportionate / n_districts
  )

# National trend, by year: restricted to the 21-state consistent panel -----
# (nominal + FY23-$ adjusted, side by side)

sped_national_fy20_fy23_cpi_consistent21_summary <- sped_disproportionality_panel_cpi_consistent21 |>
  group_by(fy) |>
  summarise(
    n_states = n_distinct(state),
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    current_sped_exp = sum(current_sped_exp, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev, na.rm = T),
    current_sped_exp_adjusted = sum(current_sped_exp_adjusted, na.rm = T),
    state_fed_sped_rev_adjusted = sum(state_fed_sped_rev_adjusted, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T)
  ) |>
  mutate(
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    total_sped_pp_expenditure_adjusted = current_sped_exp_adjusted / sped_enroll,
    state_fed_sped_pp_funding_adjusted = state_fed_sped_rev_adjusted / sped_enroll,
    local_sped_pp_obligation_adjusted = total_sped_pp_expenditure_adjusted - state_fed_sped_pp_funding_adjusted,
    pct_disproportionate = n_disproportionate / n_districts
  )

# State summary, by year: 21-state consistent panel, nominal + FY23-$ adjusted -----

sped_state_fy20_fy23_cpi_summary <- sped_disproportionality_panel_cpi_consistent21 |>
  group_by(fy, state) |>
  summarise(
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    current_sped_exp = sum(current_sped_exp, na.rm = T),
    state_sped_rev = sum(state_sped_rev, na.rm = T),
    federal_sped_rev = sum(federal_sped_rev, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev, na.rm = T),
    current_sped_exp_adjusted = sum(current_sped_exp_adjusted, na.rm = T),
    state_sped_rev_adjusted = sum(state_sped_rev_adjusted, na.rm = T),
    federal_sped_rev_adjusted = sum(federal_sped_rev_adjusted, na.rm = T),
    state_fed_sped_rev_adjusted = sum(state_fed_sped_rev_adjusted, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    total_sped_pp_expenditure_adjusted = current_sped_exp_adjusted / sped_enroll,
    state_fed_sped_pp_funding_adjusted = state_fed_sped_rev_adjusted / sped_enroll,
    local_sped_pp_obligation_adjusted = total_sped_pp_expenditure_adjusted - state_fed_sped_pp_funding_adjusted,
    pct_disproportionate = n_disproportionate / n_districts
  )

# State-level FY20 -> FY23 change, in real (FY23 $) terms -----
# mirrors old script 5's local_sped_pp_obligation_diff, but real instead of nominal

sped_state_fy20_fy23_cpi_change <- sped_state_fy20_fy23_cpi_summary |>
  filter(fy %in% c("fy20", "fy23")) |>
  select(fy, state, local_sped_pp_obligation_adjusted, pct_disproportionate) |>
  pivot_wider(
    names_from = fy,
    values_from = c(local_sped_pp_obligation_adjusted, pct_disproportionate)
  ) |>
  mutate(
    local_sped_pp_obligation_adjusted_diff = local_sped_pp_obligation_adjusted_fy23 - local_sped_pp_obligation_adjusted_fy20,
    local_sped_pp_obligation_adjusted_pct_change = local_sped_pp_obligation_adjusted_diff / local_sped_pp_obligation_adjusted_fy20,
    pct_disproportionate_diff = pct_disproportionate_fy23 - pct_disproportionate_fy20
  ) |>
  arrange(desc(local_sped_pp_obligation_adjusted_diff))

# Export the national data -----

write_csv(sped_national_fy20_fy23_cpi_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_cpi_summary.csv")

write_csv(sped_national_fy20_fy23_cpi_consistent21_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_cpi_consistent21_summary.csv")

# Export the state data -----

write_csv(sped_state_fy20_fy23_cpi_summary, "sped-data/processed/state-analysis/sped_state_fy20_fy23_cpi_summary.csv")

write_csv(sped_state_fy20_fy23_cpi_change, "sped-data/processed/state-analysis/sped_state_fy20_fy23_cpi_change.csv")

# Tidy workplace -----

rm(consistent_states, sped_disproportionality_panel_cpi_consistent21)
