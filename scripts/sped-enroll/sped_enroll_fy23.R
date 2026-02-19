# sped_enrollment_fy23
# last updated by Krista Kaput on 2026-02-18

# This script clean the fy23 special education enrollment data 


# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the special education fy23 enrollment 
sped_enroll_fy23_raw <- read_csv("sped-data/raw/bchildcountdisabilitycategorylea2022-23.csv", skip = 4)

# Clean the fy23 data -----
sped_enroll_fy23_clean <- sped_enroll_fy23_raw |>
  rename_with(tolower) |>
  rename(state = "state name",
         district = "lea name", 
         dist_id = "nces lea id", 
         sped_enroll_fy23 = "school age all disabilities") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  select(state, district, dist_id, sped_enroll_fy23)

# Export the data ----
write_csv(sped_enroll_fy23_clean, "sped-data/processed/sped-enroll/sped_enroll_fy23_clean.csv")