# sped_topline_national_state_fy20_fy23.R
# written 2026-08-27
#
# Takes the district-year disproportionality panel built by
# sped_disproportionality_fy20_fy23.R (sped_disproportionality_panel_fy20_fy23.csv)
# and produces the topline national and state summaries, nominal and CPI-adjusted,
# for all four years - the longitudinal equivalent of
# sped_funding_fy20_fy23_state_national_analysis.R, which only covered FY20 vs. FY23
# in wide format. This script works off the long-format (one row per district per
# fy) panel instead, so it can group_by(fy) / group_by(fy, state) the way the rest
# of this pipeline groups by state, and adding a fifth year later is a matter of
# adding another build_year_panel() call upstream rather than adding a new wide
# column pair everywhere.
#
# Two national tables come out of this:
#   - sped_national_fy20_fy23_summary.csv: every state with usable data *that year*
#     (the right table for "what did FY22 look like on its own")
#   - sped_national_fy20_fy23_consistent21_summary.csv: restricted to the 21 states
#     with usable data in *all four* years (the right table for "how did things
#     change" - a state entering/leaving the sample shifts national totals on its
#     own, independent of any real funding change)
# And two state tables, both restricted to the same 21-state consistent panel so
# every state row has all four years to compare:
#   - sped_state_fy20_fy23_summary.csv: nominal dollars
#   - sped_state_fy20_fy23_cpi_summary.csv: FY23-dollar (real) terms, using the
#     same CPI multipliers as sped_funding_cpi.R
#
# NOTE: this inherits the disproportionality panel's Colorado exclusion (see the
# header of sped_disproportionality_fy20_fy23.R) - re-run both scripts together
# if/when that gets resolved.

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

sped_disproportionality_panel <- read_csv("sped-data/processed/sped_disproportionality_panel_fy20_fy23.csv")

# CPI inflation adjustments, same multipliers as sped_funding_cpi.R, indexed to FY23 -----

cpi_fy20 <- 1.165049
cpi_fy21 <- 1.138837
cpi_fy22 <- 1.062620
cpi_fy23 <- 1.000000

cpi_lookup <- tibble(
  fy = c("fy20", "fy21", "fy22", "fy23"),
  cpi_multiplier = c(cpi_fy20, cpi_fy21, cpi_fy22, cpi_fy23)
)

sped_disproportionality_panel_cpi <- sped_disproportionality_panel |>
  left_join(cpi_lookup, by = "fy") |>
  mutate(
    current_sped_exp_adjusted = current_sped_exp * cpi_multiplier,
    state_sped_rev_adjusted = state_sped_rev * cpi_multiplier,
    federal_sped_rev_adjusted = federal_sped_rev * cpi_multiplier,
    state_fed_sped_rev_adjusted = state_fed_sped_rev * cpi_multiplier
  )

# the 21 states with usable data in every year fy20-fy23 - the set to use for any
# year-over-year trend comparison, so a state entering/leaving the sample doesn't
# masquerade as a funding change. Re-derive this if a 5th year gets added.

states_by_fy <- sped_disproportionality_panel |>
  distinct(fy, state)

consistent_states <- states_by_fy |>
  count(state) |>
  filter(n == 4) |>
  pull(state)

message(
  "States with usable data in all 4 years (fy20-fy23): ", length(consistent_states), "\n",
  paste(sort(consistent_states), collapse = ", ")
)

sped_disproportionality_panel_consistent21 <- sped_disproportionality_panel_cpi |>
  filter(state %in% consistent_states)

# National summary, by year: every state with usable data that year -----

sped_national_fy20_fy23_summary <- sped_disproportionality_panel |>
  group_by(fy) |>
  summarise(
    n_states = n_distinct(state),
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    current_sped_exp = sum(current_sped_exp, na.rm = T),
    state_sped_rev = sum(state_sped_rev, na.rm = T),
    federal_sped_rev = sum(federal_sped_rev, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T)
  ) |>
  mutate(
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_sped_pp_funding = state_sped_rev / sped_enroll,
    federal_sped_pp_funding = federal_sped_rev / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    pct_disproportionate = n_disproportionate / n_districts
  )

# National trend, by year: restricted to the 21-state consistent panel -----

sped_national_fy20_fy23_consistent21_summary <- sped_disproportionality_panel_consistent21 |>
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
    # CPI-adjusted (FY23 dollars)
    current_sped_exp_adjusted = sum(current_sped_exp_adjusted, na.rm = T),
    state_sped_rev_adjusted = sum(state_sped_rev_adjusted, na.rm = T),
    federal_sped_rev_adjusted = sum(federal_sped_rev_adjusted, na.rm = T),
    state_fed_sped_rev_adjusted = sum(state_fed_sped_rev_adjusted, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T)
  ) |>
  mutate(
    # nominal per-pupil
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    # real (FY23 $) per-pupil
    total_sped_pp_expenditure_real = current_sped_exp_adjusted / sped_enroll,
    state_fed_sped_pp_funding_real = state_fed_sped_rev_adjusted / sped_enroll,
    local_sped_pp_obligation_real = total_sped_pp_expenditure_real - state_fed_sped_pp_funding_real,
    pct_disproportionate = n_disproportionate / n_districts
  )

# State summary, by year: nominal, 21-state consistent panel -----

sped_state_fy20_fy23_summary <- sped_disproportionality_panel_consistent21 |>
  group_by(fy, state) |>
  summarise(
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    current_sped_exp = sum(current_sped_exp, na.rm = T),
    state_sped_rev = sum(state_sped_rev, na.rm = T),
    federal_sped_rev = sum(federal_sped_rev, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_sped_pp_funding = state_sped_rev / sped_enroll,
    federal_sped_pp_funding = federal_sped_rev / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    pct_disproportionate = n_disproportionate / n_districts
  )

# State summary, by year: CPI-adjusted (FY23 $), 21-state consistent panel -----

sped_state_fy20_fy23_cpi_summary <- sped_disproportionality_panel_consistent21 |>
  group_by(fy, state) |>
  summarise(
    n_districts = n(),
    sped_enroll = sum(sped_enroll, na.rm = T),
    current_sped_exp = sum(current_sped_exp_adjusted, na.rm = T),
    state_sped_rev = sum(state_sped_rev_adjusted, na.rm = T),
    federal_sped_rev = sum(federal_sped_rev_adjusted, na.rm = T),
    state_fed_sped_rev = sum(state_fed_sped_rev_adjusted, na.rm = T),
    n_disproportionate = sum(disproportionate, na.rm = T),
    .groups = "drop"
  ) |>
  mutate(
    total_sped_pp_expenditure = current_sped_exp / sped_enroll,
    state_sped_pp_funding = state_sped_rev / sped_enroll,
    federal_sped_pp_funding = federal_sped_rev / sped_enroll,
    state_fed_sped_pp_funding = state_fed_sped_rev / sped_enroll,
    local_sped_pp_obligation = total_sped_pp_expenditure - state_fed_sped_pp_funding,
    pct_disproportionate = n_disproportionate / n_districts
  )

# Highest/lowest per-pupil spender by year, for the "topline bullets" narrative -----

sped_state_pp_extremes_by_fy <- sped_state_fy20_fy23_summary |>
  group_by(fy) |>
  summarise(
    highest_pp_state = state[which.max(total_sped_pp_expenditure)],
    highest_pp_value = max(total_sped_pp_expenditure),
    lowest_pp_state = state[which.min(total_sped_pp_expenditure)],
    lowest_pp_value = min(total_sped_pp_expenditure)
  )

# Export the national data -----

write_csv(sped_national_fy20_fy23_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_summary.csv")

write_csv(sped_national_fy20_fy23_consistent21_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_consistent21_summary.csv")

# Export the state data -----

write_csv(sped_state_fy20_fy23_summary, "sped-data/processed/state-analysis/sped_state_fy20_fy23_summary.csv")

write_csv(sped_state_fy20_fy23_cpi_summary, "sped-data/processed/state-analysis/sped_state_fy20_fy23_cpi_summary.csv")

write_csv(sped_state_pp_extremes_by_fy, "sped-data/processed/state-analysis/sped_state_pp_extremes_by_fy.csv")

# Tidy workplace -----

rm(cpi_fy20, cpi_fy21, cpi_fy22, cpi_fy23, cpi_lookup, states_by_fy, consistent_states,
   sped_disproportionality_panel_cpi, sped_disproportionality_panel_consistent21)