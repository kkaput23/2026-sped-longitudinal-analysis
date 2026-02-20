# sped_enroll_fy20_fy23_state_national_analysis
# last updated by Krista Kaput on 2026-02-20

# This script analyzes the FY20 and FY23 special education funding data at the 
# national and state levels 


# load ---------------------------------

library(tidyverse)

options(scipen = 999)

# load the joined data
sped_exp_rev_enroll_join_cpi <- read_csv("sped-data/processed/sped_exp_rev_enroll_join_cpi.csv")

# Create the national summary: These ARE NOT adjusted for inflation ----
sped_national_fy20_fy23_no_cpi_summary <- sped_exp_rev_enroll_join_cpi |>
  # group_by(state) |>
  summarise(state_sped_rev_fy20 = sum(state_sped_rev_fy20, na.rm = T),
            federal_sped_rev_fy20 = sum(federal_sped_rev_fy20, na.rm = T),
            state_fed_sped_rev_fy20 = sum(state_fed_sped_rev_fy20, na.rm = T),
            current_sped_exp_fy20 = sum(current_sped_exp_fy20, na.rm = T),
            sped_enroll_fy20 = sum(sped_enroll_fy20, na.rm = T),
            state_sped_rev_fy23 = sum(state_sped_rev_fy23, na.rm = T),
            federal_sped_rev_fy23 = sum(federal_sped_rev_fy23, na.rm = T),
            state_fed_sped_rev_fy23 = sum(state_fed_sped_rev_fy23, na.rm = T),
            current_sped_exp_fy23 = sum(current_sped_exp_fy23, na.rm = T),
            sped_enroll_fy23 = sum(sped_enroll_fy23, na.rm = T)) |>
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20 / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20 / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20 / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20 / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  # Create the sped local calculation 
  mutate(local_sped_pp_obligation_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20,
         local_sped_pp_obligation_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23,
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) 


# Create the national summary: These ARE adjusted for inflation -----

sped_national_fy20_fy23_cpi_summary <- sped_exp_rev_enroll_join_cpi |>
  # group_by(state) |>
  summarise(state_sped_rev_fy20 = sum(state_sped_rev_fy20_adjusted, na.rm = T),
            federal_sped_rev_fy20 = sum(federal_sped_rev_fy20_adjusted, na.rm = T),
            state_fed_sped_rev_fy20 = sum(state_fed_sped_rev_fy20_adjusted, na.rm = T),
            current_sped_exp_fy20 = sum(current_sped_exp_fy20_adjusted, na.rm = T),
            sped_enroll_fy20 = sum(sped_enroll_fy20, na.rm = T),
            state_sped_rev_fy23 = sum(state_sped_rev_fy23, na.rm = T),
            federal_sped_rev_fy23 = sum(federal_sped_rev_fy23, na.rm = T),
            state_fed_sped_rev_fy23 = sum(state_fed_sped_rev_fy23, na.rm = T),
            current_sped_exp_fy23 = sum(current_sped_exp_fy23, na.rm = T),
            sped_enroll_fy23 = sum(sped_enroll_fy23, na.rm = T)) |>
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20 / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20 / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20 / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20 / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  # Create the sped local calculation 
  mutate(local_sped_pp_obligation_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20,
         local_sped_pp_obligation_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23,
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) 



# Create a state summary: These ARE NOT adjusted for inflation ----

# THERE ARE ONLY 21 STATES WITH GOOD DATA 

sped_state_fy20_fy23_no_cpi_summary <- sped_exp_rev_enroll_join_cpi |>
  group_by(state) |>
  summarise(state_sped_rev_fy20 = sum(state_sped_rev_fy20, na.rm = T),
            federal_sped_rev_fy20 = sum(federal_sped_rev_fy20, na.rm = T),
            state_fed_sped_rev_fy20 = sum(state_fed_sped_rev_fy20, na.rm = T),
            current_sped_exp_fy20 = sum(current_sped_exp_fy20, na.rm = T),
            sped_enroll_fy20 = sum(sped_enroll_fy20, na.rm = T),
            state_sped_rev_fy23 = sum(state_sped_rev_fy23, na.rm = T),
            federal_sped_rev_fy23 = sum(federal_sped_rev_fy23, na.rm = T),
            state_fed_sped_rev_fy23 = sum(state_fed_sped_rev_fy23, na.rm = T),
            current_sped_exp_fy23 = sum(current_sped_exp_fy23, na.rm = T),
            sped_enroll_fy23 = sum(sped_enroll_fy23, na.rm = T)) |>
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20 / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20 / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20 / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20 / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  # Create the sped local calculation 
  mutate(local_sped_pp_obligation_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20,
         local_sped_pp_obligation_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23,
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) 


# Create a state summary: These ARE adjusted for inflation ----

# THERE ARE ONLY 21 STATES WITH GOOD DATA 

sped_state_fy20_fy23_cpi_summary <- sped_exp_rev_enroll_join_cpi |>
  group_by(state) |>
  summarise(state_sped_rev_fy20 = sum(state_sped_rev_fy20_adjusted, na.rm = T),
            federal_sped_rev_fy20 = sum(federal_sped_rev_fy20_adjusted, na.rm = T),
            state_fed_sped_rev_fy20 = sum(state_fed_sped_rev_fy20_adjusted, na.rm = T),
            current_sped_exp_fy20 = sum(current_sped_exp_fy20_adjusted, na.rm = T),
            sped_enroll_fy20 = sum(sped_enroll_fy20, na.rm = T),
            state_sped_rev_fy23 = sum(state_sped_rev_fy23, na.rm = T),
            federal_sped_rev_fy23 = sum(federal_sped_rev_fy23, na.rm = T),
            state_fed_sped_rev_fy23 = sum(state_fed_sped_rev_fy23, na.rm = T),
            current_sped_exp_fy23 = sum(current_sped_exp_fy23, na.rm = T),
            sped_enroll_fy23 = sum(sped_enroll_fy23, na.rm = T)) |>
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20 / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20 / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20 / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20 / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  # Create the sped local calculation 
  mutate(local_sped_pp_obligation_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20,
         local_sped_pp_obligation_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23,
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) 


# Export the national data -----
write_csv(sped_national_fy20_fy23_no_cpi_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_no_cpi_summary.csv")

write_csv(sped_national_fy20_fy23_cpi_summary, "sped-data/processed/national-analysis/sped_national_fy20_fy23_cpi_summary.csv")


# Export the state data ------

write_csv(sped_state_fy20_fy23_no_cpi_summary, "sped-data/processed/state-analysis/state_national_fy20_fy23_no_cpi_summary.csv")


write_csv(sped_state_fy20_fy23_cpi_summary, "sped-data/processed/state-analysis/state_national_fy20_fy23_cpi_summary.csv")






