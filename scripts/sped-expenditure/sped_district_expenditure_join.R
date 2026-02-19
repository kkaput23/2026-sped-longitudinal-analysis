# sped_district_expenditures_join
# last updated by Krista Kaput on 2026-02-18

# This script joins the FY20 to FY23 sped district expenditure data 

# There are 6 states not included in the data frame: 
# Arizona, Delaware, Illinois, Michigan, Minnesota, New York 

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the FY20 clean district sped expenditure data 
sped_district_expenditure_fy20 <- read_csv("sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy20.csv")

# Load the FY21 clean district sped expenditure data 
sped_district_expenditure_fy21 <- read_csv("sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy21.csv")

# Load the FY22 clean district sped expenditure data 
sped_district_expenditure_fy22 <- read_csv("sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy22.csv")

# Load the FY23 clean district sped expenditure data 
sped_district_expenditure_fy23 <- read_csv("sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy23.csv")


# Clean the FY20 data so we can join it ----
sped_district_expenditure_fy20 <- sped_district_expenditure_fy20 |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(total_enroll_fy20 = total_enroll, 
         current_sped_exp_fy20 = current_sped_exp) |>
  select(dist_id, state, schlev, agchrt, district, total_enroll_fy20, current_sped_exp_fy20)
  
# Clean the FY21 data so we can join it ----
sped_district_expenditure_fy21 <- sped_district_expenditure_fy21 |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(total_enroll_fy21 = total_enroll, 
         current_sped_exp_fy21 = current_sped_exp) |>
  select(dist_id, state, total_enroll_fy21, current_sped_exp_fy21) 
  
# Clean the FY22 data so we can join it ----
sped_district_expenditure_fy22 <- sped_district_expenditure_fy22 |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(total_enroll_fy22 = total_enroll, 
         current_sped_exp_fy22 = current_sped_exp) |>
  select(dist_id, state, total_enroll_fy22, current_sped_exp_fy22) 

# Clean the FY23 data so we can join it ----
sped_district_expenditure_fy23 <- sped_district_expenditure_fy23 |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(total_enroll_fy23 = total_enroll, 
         current_sped_exp_fy23 = current_sped_exp) |>
  select(dist_id, state, total_enroll_fy23, current_sped_exp_fy23) 

# Join the fy20 and fy23 districts ----

# This is for the FY20 to FY23 analysis. We will drop the districts that do not have 
# data for both years 

# The districts went from 8885 to 7907 districts 
sped_district_fy20_fy23 <-  sped_district_expenditure_fy20 |>
  left_join(sped_district_expenditure_fy23, by = c("dist_id", "state")) |>
  select(dist_id, state, schlev, agchrt, district, total_enroll_fy20,
         current_sped_exp_fy20, total_enroll_fy23, current_sped_exp_fy23) |>
  mutate(current_sped_exp_growth = (current_sped_exp_fy23 - current_sped_exp_fy20) / current_sped_exp_fy20) |>
  drop_na()

# Join the data ----

# This is all the data joined together 
sped_district_expenditure_join <- sped_district_expenditure_fy20 |>
  left_join(sped_district_expenditure_fy21, by = c("dist_id", "state")) |>
  left_join(sped_district_expenditure_fy22, by = c("dist_id", "state"))  |>
  left_join(sped_district_expenditure_fy23, by = c("dist_id", "state")) |>
  # If we drop the districts with a missing year of data it goes from 8885 to 7846 districts 
  drop_na()

# Create a statewide summary ----
sped_state_expenditure_summary <- sped_district_expenditure_join |>
  group_by(state) |>
    # Summarise total enrollment 
  summarise(total_enroll_fy20 = sum(total_enroll_fy20, na.rm = T),
            total_enroll_fy21 = sum(total_enroll_fy21, na.rm = T),
            total_enroll_fy22 = sum(total_enroll_fy22, na.rm = T),
            total_enroll_fy23 = sum(total_enroll_fy23, na.rm = T),
            current_sped_exp_fy20 = sum(current_sped_exp_fy20, na.rm = T),
            current_sped_exp_fy21 = sum(current_sped_exp_fy21, na.rm = T),
            current_sped_exp_fy22 = sum(current_sped_exp_fy22, na.rm = T),
            current_sped_exp_fy23 = sum(current_sped_exp_fy23, na.rm = T)) |>
  mutate(current_sped_exp_fy20_fy23_growth = (current_sped_exp_fy23 - current_sped_exp_fy20) / current_sped_exp_fy20) 

# Export the data ---

write_csv(sped_district_expenditure_join, "sped-data/processed/sped-expenditure/sped_district_expenditure_join.csv")


  
  
  
  
  
