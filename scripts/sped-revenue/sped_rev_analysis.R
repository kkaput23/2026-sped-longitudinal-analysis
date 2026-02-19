# sped_rev_fy20
# last updated by Krista Kaput on 2026-02-18

# Join the special education FY20 to FY23 enrollment 


# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load the cleaned data
sped_revenue_long <- read_csv("sped-data/processed/sped-revenue/sped_revenue_long.csv")


# Filter and clean the 2020 data ----

sped_rev_fy20 <- sped_revenue_long |>
  filter(year == "2020") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(state_rev_v92_adjusted_fy20 = state_rev_v92_adjusted, 
         federal_rev_v92_adjusted_fy20 = federal_rev_v92_adjusted, 
         local_rev_v92_adjusted_fy20 = local_rev_v92_adjusted, 
         state_local_federal_rev_v92_adjusted_fy20 = state_local_federal_rev_v92_adjusted,
         state_sped_rev_fy20 = state_sped_rev,
         federal_sped_rev_fy20 = federal_sped_rev, 
         state_fed_sped_rev_fy20 = state_fed_sped_rev) |>
  select(dist_id, state, district, state_rev_v92_adjusted_fy20, federal_rev_v92_adjusted_fy20, 
         local_rev_v92_adjusted_fy20, state_local_federal_rev_v92_adjusted_fy20,
         state_sped_rev_fy20, federal_sped_rev_fy20, state_fed_sped_rev_fy20)

# Filter and clean the 2021 data ----

sped_rev_fy21 <- sped_revenue_long |>
  filter(year == "2021") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(state_rev_v92_adjusted_fy21 = state_rev_v92_adjusted, 
         federal_rev_v92_adjusted_fy21 = federal_rev_v92_adjusted, 
         local_rev_v92_adjusted_fy21 = local_rev_v92_adjusted, 
         state_local_federal_rev_v92_adjusted_fy21 = state_local_federal_rev_v92_adjusted,
         state_sped_rev_fy21 = state_sped_rev,
         federal_sped_rev_fy21 = federal_sped_rev, 
         state_fed_sped_rev_fy21 = state_fed_sped_rev) |>
  select(dist_id, state_rev_v92_adjusted_fy21, federal_rev_v92_adjusted_fy21, 
         local_rev_v92_adjusted_fy21, state_local_federal_rev_v92_adjusted_fy21, 
         state_sped_rev_fy21, federal_sped_rev_fy21, state_fed_sped_rev_fy21)

# Filter and clean the 2022 data ----

sped_rev_fy22 <- sped_revenue_long |>
  filter(year == "2022") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(state_rev_v92_adjusted_fy22 = state_rev_v92_adjusted, 
         federal_rev_v92_adjusted_fy22 = federal_rev_v92_adjusted, 
         local_rev_v92_adjusted_fy22 = local_rev_v92_adjusted, 
         state_local_federal_rev_v92_adjusted_fy22 = state_local_federal_rev_v92_adjusted, 
         state_sped_rev_fy22 = state_sped_rev,
         federal_sped_rev_fy22 = federal_sped_rev, 
         state_fed_sped_rev_fy22 = state_fed_sped_rev) |>
  select(dist_id, state_rev_v92_adjusted_fy22, federal_rev_v92_adjusted_fy22, 
         local_rev_v92_adjusted_fy22, state_local_federal_rev_v92_adjusted_fy22, 
         state_sped_rev_fy22, federal_sped_rev_fy22, state_fed_sped_rev_fy22)

# Filter and clean the 2023 data ----

sped_rev_fy23 <- sped_revenue_long |>
  filter(year == "2023") |>
  mutate(dist_id = str_pad(dist_id, width = 7, side = "left", pad = "0")) |>
  rename(state_rev_v92_adjusted_fy23 = state_rev_v92_adjusted, 
         federal_rev_v92_adjusted_fy23 = federal_rev_v92_adjusted, 
         local_rev_v92_adjusted_fy23 = local_rev_v92_adjusted, 
         state_local_federal_rev_v92_adjusted_fy23 = state_local_federal_rev_v92_adjusted, 
         state_sped_rev_fy23 = state_sped_rev,
         federal_sped_rev_fy23 = federal_sped_rev, 
         state_fed_sped_rev_fy23 = state_fed_sped_rev) |>
  select(dist_id, state_rev_v92_adjusted_fy23, federal_rev_v92_adjusted_fy23, 
         local_rev_v92_adjusted_fy23, state_local_federal_rev_v92_adjusted_fy23,
         state_sped_rev_fy23, federal_sped_rev_fy23, state_fed_sped_rev_fy23)

# Join the expenditure data -----

sped_rev_join <- sped_rev_fy20 |>
  left_join(sped_rev_fy21, by = "dist_id") |>
  left_join(sped_rev_fy22, by = "dist_id") |>
  left_join(sped_rev_fy23, by = "dist_id") 
  

# Export the data ---

write_csv(sped_rev_join, "sped-data/processed/sped-revenue/sped_rev_join.csv")



















