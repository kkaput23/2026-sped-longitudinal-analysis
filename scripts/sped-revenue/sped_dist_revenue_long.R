# sped_dist_revenue_long.R
# last updated by Krista Kaput on 2026-02-06


# load ---------------------------------

options(scipen = 999)

library(tidyverse)
library(readxl)

# load the 2020 data 
sped_revenue_fy20_raw <- read_csv("sped-data/raw/sdf20_1a 2.csv")

# load the 2021 data 
sped_revenue_fy21_raw  <- read_csv("sped-data/raw/sdf21_1a.csv")

# load the 2022 data
sped_revenue_fy22_raw <- read_csv("sped-data/raw/sdf22_1a.csv")

# load the 2023 data
sped_revenue_fy23_raw <- read_csv("sped-data/raw/sdf23_1a.csv")


# Clean the 2020 data ------

sped_revenue_fy20_clean <- sped_revenue_fy20_raw |>
  rename_with(tolower) |>
  rename(district = name, 
         dist_id = leaid, 
         state = stname,
         state_sped_rev = c05,
         federal_sped_rev = c15,
         total_enroll = membersch) |>
  mutate(state_fed_sped_rev = state_sped_rev + federal_sped_rev) |> 
  select(dist_id, state, district, schlev, agchrt, total_enroll, totalrev, tfedrev, 
         tstrev, tlocrev, state_sped_rev, 
         federal_sped_rev, state_fed_sped_rev,
         c11, #STATE REVENUE - CAPITAL OUTLAY AND DEBT SERVICES PROGRAMS
         u11, # local revenue sale of property
         v92) |> # payments to charter schools
  # I am making the adjustments per Edbuildr and the leveling the landscape paper
  mutate(state_rev_adjusted =  tstrev - c11, # subtract capital outlay
         local_rev_adjusted = tlocrev - u11, # subtract property sales 
         state_local_rev_adjusted = state_rev_adjusted + local_rev_adjusted,
         state_local_federal_rev_adjusted = state_local_rev_adjusted + tfedrev) |>
  # adjust the revenue for payments to charter schools (v92)
  # Determine the state percentage of the adjusted revenue 
  mutate(rev_state_pct = state_rev_adjusted /state_local_federal_rev_adjusted,
         # determine local % and federal % of adj rev
         rev_local_pct = local_rev_adjusted / state_local_federal_rev_adjusted,
         rev_federal_pct = tfedrev / state_local_federal_rev_adjusted,
         # calc state, federal, and local portions of v92 payments to charter schools
         v92_state = v92 * rev_state_pct,
         v92_local = v92 * rev_local_pct,
         v92_federal = v92 * rev_federal_pct,
         # calc v92 adj state, local, and federal adjusted revenue
         state_rev_v92_adjusted = state_rev_adjusted - v92_state,
         local_rev_v92_adjusted = local_rev_adjusted - v92_local,
         federal_rev_v92_adjusted = tfedrev - v92_federal,
         state_local_federal_rev_v92_adjusted = state_rev_v92_adjusted + local_rev_v92_adjusted + federal_rev_v92_adjusted) |>
  mutate(year = 2020) |>
  # When we filter these out, it drops from 19672 to 17286
  filter(schlev != "N") |> # Not applicable or the code could not be determined 
  filter(schlev != "5") |> # Vocational or special education system  
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools  
  filter(schlev != "7") |> # Education service agency 
  # filter out the districts that are charter schools or who are NA 
  # It drops from 17286 to 13271
  filter(agchrt != "N") |> # Not applicable or the code could not be determined 
  filter(agchrt != "1") |> # All associated schools are charter schools  
  # filter the districts that have small enrollment. For the purposes of this 
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 129839
  filter(total_enroll > 49) |>
  # This drops districts that do not have any revenue 
  # This drops districts to 12737
  filter(totalrev > 0) 

# Clean the 2021 data ------
sped_revenue_fy21_clean <- sped_revenue_fy21_raw |>
  rename_with(tolower) |>
  rename(district = name, 
         dist_id = leaid, 
         state = stname,
         state_sped_rev = c05,
         federal_sped_rev = c15,
         total_enroll = membersch) |>
  mutate(state_fed_sped_rev = state_sped_rev + federal_sped_rev) |> 
  select(dist_id, state, district, schlev, agchrt, total_enroll, totalrev, tfedrev, 
         tstrev, tlocrev, state_sped_rev, 
         federal_sped_rev, state_fed_sped_rev,
         c11, #STATE REVENUE - CAPITAL OUTLAY AND DEBT SERVICES PROGRAMS
         u11, # local revenue sale of property
         v92) |> # payments to charter schools
  # I am making the adjustments per Edbuildr and the leveling the landscape paper
  mutate(state_rev_adjusted =  tstrev - c11, # subtract capital outlay
         local_rev_adjusted = tlocrev - u11, # subtract property sales 
         state_local_rev_adjusted = state_rev_adjusted + local_rev_adjusted,
         state_local_federal_rev_adjusted = state_local_rev_adjusted + tfedrev) |>
  # adjust the revenue for payments to charter schools (v92)
  # Determine the state percentage of the adjusted revenue 
  mutate(rev_state_pct = state_rev_adjusted /state_local_federal_rev_adjusted,
         # determine local % and federal % of adj rev
         rev_local_pct = local_rev_adjusted / state_local_federal_rev_adjusted,
         rev_federal_pct = tfedrev / state_local_federal_rev_adjusted,
         # calc state, federal, and local portions of v92 payments to charter schools
         v92_state = v92 * rev_state_pct,
         v92_local = v92 * rev_local_pct,
         v92_federal = v92 * rev_federal_pct,
         # calc v92 adj state, local, and federal adjusted revenue
         state_rev_v92_adjusted = state_rev_adjusted - v92_state,
         local_rev_v92_adjusted = local_rev_adjusted - v92_local,
         federal_rev_v92_adjusted = tfedrev - v92_federal,
         state_local_federal_rev_v92_adjusted = state_rev_v92_adjusted + local_rev_v92_adjusted + federal_rev_v92_adjusted) |>
  mutate(year = 2021) |>
  # When we filter these out, it drops from 19554 to 17325
  filter(schlev != "N") |> # Not applicable or the code could not be determined 
  filter(schlev != "5") |> # Vocational or special education system  
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools  
  filter(schlev != "7") |> # Education service agency 
  # filter out the districts that are charter schools or who are NA 
  # It drops from 17325 to 13247
  filter(agchrt != "N") |> # Not applicable or the code could not be determined 
  filter(agchrt != "1") |> # All associated schools are charter schools  
  # filter the districts that have small enrollment. For the purposes of this 
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 12747
  filter(total_enroll > 49) |>
  # This drops districts that do not have any revenue 
  # This drops districts to 12648
  filter(totalrev > 0) 

# Clean the 2022 data ------

sped_revenue_fy22_clean <- sped_revenue_fy22_raw |>
  rename_with(tolower) |>
  rename(district = name, 
         dist_id = leaid, 
         state = stname,
         state_sped_rev = c05,
         federal_sped_rev = c15,
         total_enroll = membersch) |>
  mutate(state_fed_sped_rev = state_sped_rev + federal_sped_rev) |> 
  select(dist_id, state, district, schlev, agchrt, total_enroll, totalrev, tfedrev, 
         tstrev, tlocrev, state_sped_rev, 
         federal_sped_rev, state_fed_sped_rev,
         c11, #STATE REVENUE - CAPITAL OUTLAY AND DEBT SERVICES PROGRAMS
         u11, # local revenue sale of property
         v92) |> # payments to charter schools
  # I am making the adjustments per Edbuildr and the leveling the landscape paper
  mutate(state_rev_adjusted =  tstrev - c11, # subtract capital outlay
         local_rev_adjusted = tlocrev - u11, # subtract property sales 
         state_local_rev_adjusted = state_rev_adjusted + local_rev_adjusted,
         state_local_federal_rev_adjusted = state_local_rev_adjusted + tfedrev) |>
  # adjust the revenue for payments to charter schools (v92)
  # Determine the state percentage of the adjusted revenue 
  mutate(rev_state_pct = state_rev_adjusted /state_local_federal_rev_adjusted,
         # determine local % and federal % of adj rev
         rev_local_pct = local_rev_adjusted / state_local_federal_rev_adjusted,
         rev_federal_pct = tfedrev / state_local_federal_rev_adjusted,
         # calc state, federal, and local portions of v92 payments to charter schools
         v92_state = v92 * rev_state_pct,
         v92_local = v92 * rev_local_pct,
         v92_federal = v92 * rev_federal_pct,
         # calc v92 adj state, local, and federal adjusted revenue
         state_rev_v92_adjusted = state_rev_adjusted - v92_state,
         local_rev_v92_adjusted = local_rev_adjusted - v92_local,
         federal_rev_v92_adjusted = tfedrev - v92_federal,
         state_local_federal_rev_v92_adjusted = state_rev_v92_adjusted + local_rev_v92_adjusted + federal_rev_v92_adjusted) |>
  mutate(year = 2022) |>
  # When we filter these out, it drops from 19572 to 17393
  filter(schlev != "N") |> # Not applicable or the code could not be determined 
  filter(schlev != "5") |> # Vocational or special education system  
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools  
  filter(schlev != "7") |> # Education service agency 
  # filter out the districts that are charter schools or who are NA 
  # It drops from 17393 to 13255
  filter(agchrt != "N") |> # Not applicable or the code could not be determined 
  filter(agchrt != "1") |> # All associated schools are charter schools  
  # filter the districts that have small enrollment. For the purposes of this 
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 12804
  filter(total_enroll > 49) |>
  # This drops districts that do not have any revenue 
  # This drops districts to 12710
  filter(totalrev > 0) 


# Clean the 2023 data ------
sped_revenue_fy23_clean <- sped_revenue_fy23_raw |>
  rename_with(tolower) |>
  rename(district = name, 
         dist_id = leaid, 
         state = stname,
         state_sped_rev = c05,
         federal_sped_rev = c15,
         total_enroll = membersch) |>
  mutate(state_fed_sped_rev = state_sped_rev + federal_sped_rev) |> 
  select(dist_id, state, district, schlev, agchrt, total_enroll, totalrev, tfedrev, 
         tstrev, tlocrev, state_sped_rev, 
         federal_sped_rev, state_fed_sped_rev,
         c11, #STATE REVENUE - CAPITAL OUTLAY AND DEBT SERVICES PROGRAMS
         u11, # local revenue sale of property
         v92) |> # payments to charter schools
  # I am making the adjustments per Edbuildr and the leveling the landscape paper
  mutate(state_rev_adjusted =  tstrev - c11, # subtract capital outlay
         local_rev_adjusted = tlocrev - u11, # subtract property sales 
         state_local_rev_adjusted = state_rev_adjusted + local_rev_adjusted,
         state_local_federal_rev_adjusted = state_local_rev_adjusted + tfedrev) |>
  # adjust the revenue for payments to charter schools (v92)
  # Determine the state percentage of the adjusted revenue 
  mutate(rev_state_pct = state_rev_adjusted /state_local_federal_rev_adjusted,
         # determine local % and federal % of adj rev
         rev_local_pct = local_rev_adjusted / state_local_federal_rev_adjusted,
         rev_federal_pct = tfedrev / state_local_federal_rev_adjusted,
         # calc state, federal, and local portions of v92 payments to charter schools
         v92_state = v92 * rev_state_pct,
         v92_local = v92 * rev_local_pct,
         v92_federal = v92 * rev_federal_pct,
         # calc v92 adj state, local, and federal adjusted revenue
         state_rev_v92_adjusted = state_rev_adjusted - v92_state,
         local_rev_v92_adjusted = local_rev_adjusted - v92_local,
         federal_rev_v92_adjusted = tfedrev - v92_federal,
         state_local_federal_rev_v92_adjusted = state_rev_v92_adjusted + local_rev_v92_adjusted + federal_rev_v92_adjusted) |>
  mutate(year = 2023) |>
  # When we filter these out, it drops from 19570 to 17412
  filter(schlev != "N") |> # Not applicable or the code could not be determined 
  filter(schlev != "5") |> # Vocational or special education system  
  filter(schlev != "6") |> # Nonoperating school system that exists for administrative purposes only and does not operate its own schools  
  filter(schlev != "7") |> # Education service agency 
  # filter out the districts that are charter schools or who are NA 
  # It drops from 17412 to 13241
  filter(agchrt != "N") |> # Not applicable or the code could not be determined 
  filter(agchrt != "1") |> # All associated schools are charter schools  
  # filter the districts that have small enrollment. For the purposes of this 
  # paper we are going to only look at districts with total enrollment of at least 50
  # This filters the districts to 12801
  filter(total_enroll > 49) |>
  # This drops districts that do not have any revenue 
  # This drops districts to 12707
  filter(totalrev > 0) 


# Rbind the data -----

sped_revenue_long <- rbind(sped_revenue_fy20_clean, 
                           sped_revenue_fy21_clean, 
                           sped_revenue_fy22_clean, 
                           sped_revenue_fy23_clean)

# Export the data -----

write_csv(sped_revenue_long, "sped-data/processed/sped-revenue/sped_revenue_long.csv")

# Tidy work place ------

rm(sped_revenue_fy20_raw, sped_revenue_fy21_raw, sped_revenue_fy22_raw,
   sped_revenue_fy23_raw)

rm(sped_revenue_fy20_clean, 
      sped_revenue_fy21_clean, 
      sped_revenue_fy22_clean, 
      sped_revenue_fy23_clean)









