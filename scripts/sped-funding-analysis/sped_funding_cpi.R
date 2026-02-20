# sped_funding_cpi
# last updated by Krista Kaput on 2026-02-20

# This script adjusts all the FY20, FY21, and FY22 data for inflation 


# load ---------------------------------

library(tidyverse)

options(scipen = 999)

# load the joined data we are going to adjust 
sped_exp_rev_enroll_join <- read_csv("sped-data/processed/sped_exp_rev_enroll_join.csv")

# Create the CPI inflation adjustments -------

# 2022 CPI adjustment 
 cpi_fy22 <- 1.062620

# 2021 CPI adjustment 
cpi_fy21 <- 1.138837

# 2020 CPI adjustment 
cpi_fy20 <- 1.165049


# Do the CPI calculations ------

sped_exp_rev_enroll_join_cpi <- sped_exp_rev_enroll_join |>
  # 2020 CPI adjustment 
  mutate(state_sped_rev_fy20_adjusted = state_sped_rev_fy20 * cpi_fy20, 
         federal_sped_rev_fy20_adjusted =  federal_sped_rev_fy20 * cpi_fy20, 
         state_fed_sped_rev_fy20_adjusted =  state_fed_sped_rev_fy20 * cpi_fy20, 
         current_sped_exp_fy20_adjusted = current_sped_exp_fy20 * cpi_fy20,
         # 2021 CPI adjustment 
         state_sped_rev_fy21_adjusted = state_sped_rev_fy21 * cpi_fy21,
         federal_sped_rev_fy21_adjusted =  federal_sped_rev_fy21 * cpi_fy21, 
         state_fed_sped_rev_fy21_adjusted =  state_fed_sped_rev_fy21 * cpi_fy21, 
         current_sped_exp_fy21_adjusted = current_sped_exp_fy21 * cpi_fy21,
         # 2022 CPI adjustment 
         state_sped_rev_fy22_adjusted = state_sped_rev_fy22 * cpi_fy22, 
         federal_sped_rev_fy22_adjusted =  federal_sped_rev_fy22 * cpi_fy22, 
         state_fed_sped_rev_fy22_adjusted =  state_fed_sped_rev_fy22 * cpi_fy22, 
         current_sped_exp_fy22_adjusted = current_sped_exp_fy22 * cpi_fy22)
  
  
# export the data -----

write_csv(sped_exp_rev_enroll_join_cpi, "sped-data/processed/sped_exp_rev_enroll_join_cpi.csv")




