# sped_district_expenditures_fy22
# last updated by Krista Kaput on 2026-02-18
# updated 2026-08-27: fixed a data bug - in this year's raw file, SCHLEV is
# zero-padded ("05","06","07") instead of the single-digit format used in
# every other year's file ("5","6","7"). The old filter (schlev != "5", etc.)
# silently failed to match the padded codes, so 140 non-district entities
# (county offices of education, vocational/special-ed systems, and other
# administrative/ESA units) were incorrectly left in the FY22 analysis
# sample instead of being excluded. Added a normalization step so the
# filter works regardless of how the raw file pads this field.

# This script analyzes the FY22 special education expenditures

# There are 7 states not included in the data frame:
# Arizona, Delaware, Illinois, Michigan, Minnesota, New York, South Carolina

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load in the raw district data
# There are 19572 districts in the raw file
ccd_fy22_raw <- read_csv("sped-data/raw/sdf22_1a.csv")

# select the special education expenditure variables -------------

ccd_fy22_sped_expenditures <- ccd_fy22_raw |>
  rename_with(tolower) |>
  rename(district = name,
         dist_id = leaid,
         state = stname,
         current_sped_exp = se1,
         sped_instructional_exp = se2,
         sped_pupil_support_services_exp = se3,
         sped_instructional_support_exp = se4,
         sped_student_transportation_exp = se5,
         sped_teacher_salaries = z36,
         total_enroll = membersch) |>
  # Normalize schlev so a zero-padded code ("05") and an unpadded code ("5")
  # are treated identically by the filters below, regardless of how this
  # particular year's raw file happens to encode it.
  mutate(schlev = str_remove(schlev, "^0+")) |>
  select(dist_id, state, schlev, agchrt, district, total_enroll, tcurelsc, # This is the variable for current elementary and federal schools
         current_sped_exp, sped_instructional_exp,
         sped_pupil_support_services_exp, sped_instructional_support_exp,
         sped_student_transportation_exp, sped_teacher_salaries)


# filter the districts that we are not going to include --------

# There are 2361 districts with no sped expenditure data.
# It looks a lot of the districts to exclude are charters, alt. districts, and cooperatives
ccd_fy22_sped_no_exp <- ccd_fy22_sped_expenditures |>
  filter(current_sped_exp < 0)

# Clean up the expenditure data to determine our data set -----
ccd_sped_exp_clean <- ccd_fy22_sped_expenditures |>
  # When we filter these out, it drops from 19572 to 17393
  filter(schlev != "N") |> # Not applicable or the code could not be determined
  filter(schlev != "5") |> # Vocational or special education system
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools
  filter(schlev != "7") |> # Education service agency
  # filter out the districts that are charter schools or who are NA
  # We are keeping the schools with a 2 or 3, which means that some or none of their schools are charters
  # It drops to 13255
  filter(agchrt != "N") |> # Not applicable or the code could not be determined
  filter(agchrt != "1") |> # All associated schools are charter schools
  # filter the districts that have small enrollment. For the purposes of this
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 9888
  filter(total_enroll > 49) |>
  # Filters districts with special education expenditures
  filter(current_sped_exp > 0)

# check raw df vs final clean df -----------

raw_summary <- ccd_fy22_sped_expenditures |>
  group_by(state) |>
  summarise(n_dist_raw = n(),
            enroll_raw = sum(total_enroll, na.rm = T))

analysis_summary <- ccd_sped_exp_clean |>
  group_by(state) |>
  summarise(n_dist_clean = n(),
            enroll_clean = sum(total_enroll, na.rm = T))

# note: there are some states with a lot of drop-off in enrollment I assume
# they are charters but want to make sure there's not something else going on!
comp_df <- raw_summary |>
  left_join(analysis_summary) |>
  mutate(dist_pct = n_dist_clean / n_dist_raw,
         enroll_pct = enroll_clean / enroll_raw,
         enroll_diff = enroll_raw - enroll_clean)

# Create a state summary for the number of districts -----

state_total_district_count <- ccd_sped_exp_clean |>
  group_by(state) |>
  summarise(district_count = n())

# State group by ------

sped_state_exp <- ccd_sped_exp_clean |>
  group_by(state) |>
  summarise(state = first(state),
            current_sped_exp = sum(current_sped_exp, na.rm = T))


# Export the data -------

write_csv(ccd_sped_exp_clean, "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy22.csv")


write_csv(state_total_district_count, "sped-data/processed/sped-expenditure/state_total_district_count_fy22.csv")


# Tidy workplace -----

rm(ccd_fy22_raw, ccd_fy22_sped_expenditures, ccd_fy22_sped_no_exp, sped_state_exp,
   state_total_district_count)

rm(raw_summary, analysis_summary, comp_df)
