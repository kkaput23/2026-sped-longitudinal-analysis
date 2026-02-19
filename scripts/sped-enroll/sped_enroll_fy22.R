# sped_enrollment_fy22
# last updated by Krista Kaput on 2026-02-18

# This script clean the fy22 special education enrollment data 


# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the special education fy22 enrollment 
sped_enroll_fy22_raw <- read_csv("sped-data/raw/bchildcountandedenvironmentlea2021-22.csv", skip = 4)

# Clean the fy22 data -----
sped_enroll_fy22_clean <- sped_enroll_fy22_raw |>
  rename_with(tolower) |>
  rename(state = "state name",
         district = "lea name", 
         dist_id = "nces lea id", 
         sped_enroll_fy22 = "school age all disabilities") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  select(state, district, dist_id, sped_enroll_fy22)

# Export the data ----
write_csv(sped_enroll_fy22_clean, "sped-data/processed/sped-enroll/sped_enroll_fy22_clean.csv")
