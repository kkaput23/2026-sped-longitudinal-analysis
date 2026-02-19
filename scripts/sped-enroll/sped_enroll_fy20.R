# sped_enrollment_fy20
# last updated by Krista Kaput on 2026-02-18

# This script clean the fy20 special education enrollment data 

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the special education FY20 enrollment 
sped_enroll_fy20_raw <- read_csv("sped-data/raw/ccd_lea_2_89_1920_l_1a_082120.csv")

# Clean the FY20 data -----
sped_enroll_fy20_clean <- sped_enroll_fy20_raw |>
  rename_with(tolower) |>
  rename(state = "statename",
         district = lea_name,
         dist_id = leaid, 
         sped_enroll_fy20 = idea_count) |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  select(state, district, dist_id, sped_enroll_fy20)

# Export the data ----
write_csv(sped_enroll_fy20_clean, "sped-data/processed/sped-enroll/sped_enroll_fy20_clean.csv")
