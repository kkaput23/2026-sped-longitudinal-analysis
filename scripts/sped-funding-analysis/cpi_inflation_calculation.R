# cpi_inflation_calculation
# last updated by Krista Kaput on 2026-02-20

# This is the CPI caluclation for the FY20 data

# load ---------------------------------

options(scipen = 999)

library(tidyverse)
library(readxl)


# load the data 
cpi_inflation_raw <- read_excel("sped-data/raw/fy23 cpi calculation.xlsx", 
                                   skip = 11)



# Creating school-year CPI multiplier for adjusting to 2023 dollars ----

cpi_15to23 <- cpi_inflation_raw  |>
  rename(year = Year) |>
  arrange(year) |>
  mutate(HALF2_lagged = lag(HALF2)) 

# Taking the average of both Half lags into to get the annual CPI multiplier
cpi_15to23$avg_HALF1_HALF2lag <- rowMeans(cpi_15to23[c("HALF1", "HALF2_lagged")])

# Create the multiplier for adjusting 
cpi_15to23$CPI_multiplier_2023dollars <- 
  cpi_15to23$avg_HALF1_HALF2lag[cpi_15to23$year==2023]/cpi_15to23$avg_HALF1_HALF2lag

# Create the multiplier to use for adjusting for inflation 
cpi_15to23_use <- cpi_15to23 |>
  select(year, CPI_multiplier_2023dollars)





