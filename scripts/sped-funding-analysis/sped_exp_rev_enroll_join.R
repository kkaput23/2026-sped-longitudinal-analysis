# sped_exp_rev_enroll_join 
# last updated by Krista Kaput on 2026-02-18

# This script clean the fy23 special education enrollment data 


# load ---------------------------------

library(tidyverse)

options(scipen = 999)

# load the joined special education expenditure data 
sped_district_expenditure_join <- read_csv("sped-data/processed/sped-expenditure/sped_district_expenditure_join.csv")

# load the special education enrollment data 
sped_enroll_join <- read_csv("sped-data/processed/sped-enroll/sped_enroll_join.csv") |>
  select(-district, -state)

# load the special education expenditure data 
sped_rev_join <- read_csv("sped-data/processed/sped-revenue/sped_rev_join.csv") |>
  select(-district, -state)

# join the FY20 and FY23 data -----
sped_exp_rev_enroll_fy20_fy23_join <- sped_district_expenditure_join |>
  left_join(sped_rev_join, by = "dist_id") |>
  # There are several districts in FY22 whose IDEA count doesn't exist. I will create the FY20 and FY23 analysis 
  left_join(sped_enroll_join, by = "dist_id") |>
  select(dist_id, state, district, total_enroll_fy20, current_sped_exp_fy20, state_rev_v92_adjusted_fy20, 
         federal_rev_v92_adjusted_fy20, local_rev_v92_adjusted_fy20, state_local_federal_rev_v92_adjusted_fy20, 
         state_sped_rev_fy20, federal_sped_rev_fy20, state_fed_sped_rev_fy20, total_enroll_fy23, 
         current_sped_exp_fy23, state_rev_v92_adjusted_fy23, federal_rev_v92_adjusted_fy23, 
         local_rev_v92_adjusted_fy23, state_local_federal_rev_v92_adjusted_fy23, 
         state_sped_rev_fy23, federal_sped_rev_fy23, state_fed_sped_rev_fy23, sped_enroll_fy20, 
         sped_enroll_fy23) |>
  # This drops it to 6769 districts 
  filter(sped_enroll_fy23 > 0) |>
  # Calculate the special education state and federal revenues, and the local expenditures 
  mutate(state_sped_pp_funding_fy20 = state_sped_rev_fy20 / sped_enroll_fy20,
         federal_sped_pp_funding_fy20 = federal_sped_rev_fy20 / sped_enroll_fy20,
         state_fed_sped_pp_funding_fy20 = state_fed_sped_rev_fy20 / sped_enroll_fy20, 
         total_sped_pp_expenditure_fy20 =  current_sped_exp_fy20 / sped_enroll_fy20, 
         state_sped_pp_funding_fy23 = state_sped_rev_fy23 / sped_enroll_fy23,
         federal_sped_pp_funding_fy23 = federal_sped_rev_fy23 / sped_enroll_fy23,
         state_fed_sped_pp_funding_fy23 = state_fed_sped_rev_fy23 / sped_enroll_fy23, 
         total_sped_pp_expenditure_fy23 =  current_sped_exp_fy23 / sped_enroll_fy23) |>
  mutate(local_sped_pp_funding_fy20 = total_sped_pp_expenditure_fy20 - state_fed_sped_pp_funding_fy20, 
         local_sped_pp_funding_fy23 = total_sped_pp_expenditure_fy23 - state_fed_sped_pp_funding_fy23)

# join the data for FY20 to FY23----

sped_exp_rev_enroll_join <- sped_district_expenditure_join |>
  left_join(sped_rev_join, by = "dist_id") |>
  # There are several districts in FY22 whose IDEA count doesn't exist. I will create the FY20 and FY23 analysis 
  left_join(sped_enroll_join, by = "dist_id") |>
  # We are dropping the states that do not have any state special education revenue or have very low revenue
  filter(state != "Alaska") |>
  filter(state != "District of Columbia") |>
  filter(state != "Kentucky") |>
  filter(state != "New Hampshire") |>
  filter(state != "North Carolina") |>
  filter(state != "Ohio") |>
  filter(state != "Oklahoma") |>
  filter(state != "Oregon") |>
  filter(state != "Rhode Island") |>
  filter(state != "Tennessee") |>
  filter(state != "Texas") |>
  filter(state != "Wyoming") |>
  filter(state != "Louisiana") |>
  filter(state != "New Mexico") |>
  filter(state != "Colorado") |>
  filter(state != "Hawaii") |>
  # We are going to do an analysis of these districts so it's important to ensure they have the data
  # Drop districts who do not have special education enrollment for FY23 
  filter(sped_enroll_fy23 > 0) |>
  # Drop districts who do not have special education enrollment for FY20
  # THis drops the data to 4008 districts 
  filter(sped_enroll_fy20 > 0) 
  


# export the data -----
  
write_csv(sped_exp_rev_enroll_join, "sped-data/processed/sped_exp_rev_enroll_join.csv")
  
  
  
  
  
  
  