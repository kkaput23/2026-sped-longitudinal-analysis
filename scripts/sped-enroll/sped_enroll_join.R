# sped_enroll_join
# last updated by Krista Kaput on 2026-02-18

# Join the special education FY20 to FY23 enrollment 


# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# Load the FY20 special education enrollment 
sped_enroll_fy20_clean <- read_csv("sped-data/processed/sped-enroll/sped_enroll_fy20_clean.csv") 

# Load the FY21 special education enrollment 
sped_enroll_fy21_clean <- read_csv("sped-data/processed/sped-enroll/sped_enroll_fy21_clean.csv") |>
  select(-district, -state)

# Load the FY22 special education enrollment 
sped_enroll_fy22_clean <- read_csv("sped-data/processed/sped-enroll/sped_enroll_fy22_clean.csv") |>
  select(-district, -state)

# Load the FY23 special education enrollment 
sped_enroll_fy23_clean <- read_csv("sped-data/processed/sped-enroll/sped_enroll_fy23_clean.csv") |>
  select(-district, -state)

# Join the FY20 and FY23 data ----

sped_enroll_join <- sped_enroll_fy20_clean |>
  left_join(sped_enroll_fy21_clean, by = "dist_id") |>
  left_join(sped_enroll_fy22_clean, by = "dist_id" ) |>
  left_join(sped_enroll_fy23_clean, by = "dist_id") 

# Export the data ----

write_csv(sped_enroll_join, "sped-data/processed/sped-enroll/sped_enroll_join.csv")




