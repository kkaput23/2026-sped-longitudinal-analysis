# sped_enrollment_fy21
# last updated by Krista Kaput on 2026-02-18

# This script clean the fy21 special education enrollment data 


# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the special education fy21 enrollment 
sped_enroll_fy21_raw <- read_csv("sped-data/raw/bchildcountandedenvironmentlea2020-21.csv")

# Clean the fy21 data -----
sped_enroll_fy21_clean <- sped_enroll_fy21_raw |>
  rename_with(tolower) |>
  rename(state = "state name",
         district = "lea name", 
         dist_id = "nces lea id", 
         sped_enroll_fy21 = "school age all disabilities") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  select(state, district, dist_id, sped_enroll_fy21)

# Export the data ----
write_csv(sped_enroll_fy21_clean, "sped-data/processed/sped-enroll/sped_enroll_fy21_clean.csv")
