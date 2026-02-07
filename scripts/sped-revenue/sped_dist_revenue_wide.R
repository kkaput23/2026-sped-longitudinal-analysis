# sped_dist_revenue_wide.R
# last updated by Krista Kaput on 2026-02-06


# load ---------------------------------

options(scipen = 999)

library(tidyverse)
library(readxl)

# load in the long special education revenue data 
sped_revenue_long <- read_csv("sped-data/processed/sped-revenue/sped_revenue_long.csv")

# 
