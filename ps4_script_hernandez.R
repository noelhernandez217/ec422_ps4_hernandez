library(tidyverse)
library(fpp3)
library(BVAR)

# Part 2 – A Business Forecasting Horse Race: ARIMA vs. ETS

# 2.1a
liquor_raw <- read_csv(
  "data/MRTSSM4453USN.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    MRTSSM4453USN = col_double()
  )
)

liquor <- liquor_raw |>
  transmute(Month = yearmonth(observation_date), sales = MRTSSM4453USN) |>
  as_tsibble(index = Month) |>
  filter(Month <= yearmonth("2019 Dec"))

has_gaps(liquor)

# 2.1b - Plotting 

liquor |> autoplot(sales) + 
  labs(title = "Liquour Sales")

liquor |> gg_season(log(sales)) + 
  labs(title = "Seasonal log(sales) of Liquour")

# In a few sentences: describe the trend and the seasonal pattern. 
# Which months are high and which are low? 
# Do the seasonal swings grow with the level of the series, and what does that 
# suggest about using logs?
  
