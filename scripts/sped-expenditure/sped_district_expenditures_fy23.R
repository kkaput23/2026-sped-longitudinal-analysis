# sped_district_expenditures_fy23
# last updated by Krista Kaput on 2026-02-18

# This script analyzes the fy23 special education expenditures 

# There are 6 states not included in the data frame: 
# Arizona, Delaware, Illinois, Michigan, Minnesota, New York 

# load ---------------------------------

options(scipen = 999)

library(tidyverse)

# load in the raw district data
# There are 19570 districts in the raw file 
ccd_fy23_raw <- read_csv("sped-data/raw/sdf23_1a.csv")

# select the special education expenditure variables -------------

ccd_fy23_sped_expenditures <- ccd_fy23_raw |>
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
  select(dist_id, state, schlev, agchrt, district, total_enroll, tcurelsc, # This is the variable for current elementary and federal schools 
         current_sped_exp, sped_instructional_exp, 
         sped_pupil_support_services_exp, sped_instructional_support_exp, 
         sped_student_transportation_exp, sped_teacher_salaries)


# filter the districts that we are not going to include --------

# There are 1438 districts with no sped expenditure data. 
# It looks a lot of the districts to exclude are charters, alt. districts, and cooperatives
ccd_fy23_sped_no_exp <- ccd_fy23_sped_expenditures |>
  filter(current_sped_exp < 0)

# Clean up the expenditure data to determine our data set -----
ccd_sped_exp_clean <- ccd_fy23_sped_expenditures |>
  # When we filter these out, it drops from 19570 to 17412
  filter(schlev != "N") |> # Not applicable or the code could not be determined 
  filter(schlev != "5") |> # Vocational or special education system  
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools  
  filter(schlev != "7") |> # Education service agency 
  # filter out the districts that are charter schools or who are NA 
  # We are keeping the schools with a 2 or 3, which means that some or none of their schools are charters
  # It drops to 13241
  filter(agchrt != "N") |> # Not applicable or the code could not be determined 
  filter(agchrt != "1") |> # All associated schools are charter schools  
  # filter the districts that have small enrollment. For the purposes of this 
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 9908
  filter(total_enroll > 49) |>
  # Filters districts with special education expenditures 
  filter(current_sped_exp > 0) 

# check raw df vs final clean df -----------

raw_summary <- ccd_fy23_sped_expenditures |> 
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

write_csv(ccd_sped_exp_clean, "sped-data/processed/sped-expenditure/ccd_sped_expenditures_fy23.csv")


write_csv(state_total_district_count, "sped-data/processed/sped-expenditure/state_total_district_count_fy23.csv")


# Tidy workplace -----

rm(ccd_fy23_raw, ccd_fy23_sped_expenditures, ccd_fy23_sped_no_exp, sped_state_exp, 
   state_total_district_count)

rm(raw_summary, analysis_summary, comp_df)  

