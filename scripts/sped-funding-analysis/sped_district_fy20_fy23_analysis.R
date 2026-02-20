# sped_enroll_fy20_fy23_district_analysis
# last updated by Krista Kaput on 2026-02-20

# This script analyzes the FY20 and FY23 special education funding data at the 
# district level 


# load ---------------------------------

library(tidyverse)

options(scipen = 999)

# load the joined data
sped_exp_rev_enroll_join_cpi <- read_csv("sped-data/processed/sped_exp_rev_enroll_join_cpi.csv")

# Do the district analysis: This is NOT Adjusted for inflation -----

sped_district_fy20_fy23_no_cpi <- sped_exp_rev_enroll_join_cpi |>
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
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) |>
  select(dist_id, state, schlev, agchrt, district, current_sped_exp_fy20, state_sped_rev_fy20, 
         federal_sped_rev_fy20, state_fed_sped_rev_fy20, sped_enroll_fy20, current_sped_exp_fy23, 
         state_sped_rev_fy23, federal_sped_rev_fy23, state_fed_sped_rev_fy23, sped_enroll_fy23,
         state_sped_pp_funding_fy20, federal_sped_pp_funding_fy20, state_fed_sped_pp_funding_fy20,
         total_sped_pp_expenditure_fy20, state_sped_pp_funding_fy23, federal_sped_pp_funding_fy23, 
         state_fed_sped_pp_funding_fy23, total_sped_pp_expenditure_fy23, local_sped_pp_obligation_fy20,
         local_sped_pp_obligation_fy23, local_sped_pp_obligation_diff)

# Do the district analysis: This IS Adjusted for inflation -----

sped_district_fy20_fy23_cpi <- sped_exp_rev_enroll_join_cpi |>
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20_adjusted / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20_adjusted / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20_adjusted / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20_adjusted / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  # Create the sped local calculation 
  mutate(local_sped_pp_obligation_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20,
         local_sped_pp_obligation_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23,
         local_sped_pp_obligation_diff = local_sped_pp_obligation_fy23 - local_sped_pp_obligation_fy20) |>
  select(dist_id, state, schlev, agchrt, district, current_sped_exp_fy20_adjusted, state_sped_rev_fy20_adjusted, 
         federal_sped_rev_fy20_adjusted, state_fed_sped_rev_fy20_adjusted, sped_enroll_fy20, current_sped_exp_fy23, 
         state_sped_rev_fy23, federal_sped_rev_fy23, state_fed_sped_rev_fy23, sped_enroll_fy23,
         state_sped_pp_funding_fy20, federal_sped_pp_funding_fy20, state_fed_sped_pp_funding_fy20,
         total_sped_pp_expenditure_fy20, state_sped_pp_funding_fy23, federal_sped_pp_funding_fy23, 
         state_fed_sped_pp_funding_fy23, total_sped_pp_expenditure_fy23, local_sped_pp_obligation_fy20,
         local_sped_pp_obligation_fy23, local_sped_pp_obligation_diff)

# District summary: Districts that saw an INCREASE in their local pp obligation: NOT adjusted for inflation  ----

# This is 3199 of the districts out of 4008 
district_grow_local_obligation  <- sped_district_fy20_fy23_no_cpi |>
  filter(local_sped_pp_obligation_diff > 0) |>
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
  

# District summary: Districts that saw a DECLINE in their lcoal pp obligation: NOT adjusted for inflation  ----

# This is 801 of the districts out of 4008 
district_decline_local_obligation  <- sped_district_fy20_fy23_no_cpi |>
  filter(local_sped_pp_obligation_diff < 0) |>
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



# Export the district data ------

write_csv(sped_district_fy20_fy23_no_cpi, "sped-data/processed/district-analysis/sped_district_fy20_fy23_no_cpi.csv")


write_csv(sped_district_fy20_fy23_cpi, "sped-data/processed/district-analysis/sped_district_fy20_fy23_cpi.csv")


