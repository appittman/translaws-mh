#getting BRFSS data
library(tidyverse)
library(haven)
library(here)
# the BRFSS data on the CDC website is now censored for sexual orientation and gender identity...so i went elsewhere for this.
# ICPSR ftw

# making a list of the variables i want
my_cols <- c("_STATE", #state
             "_SEX", #sex
             "SOFEMALE", "SOMALE", #sexual orientation
             "TRNSGNDR", #trans identity
             "_RACE", "_RACE1", #race
             "EDUCA", #education
             "GENHLTH",#self-rated health
             "MENTHLTH",#poor mental health days
             "INCOME3", #income
             "IMONTH", "IDAY", "IYEAR", #interview date
             "_AGE80" #age, topcoded at 80
             )

brfss <- here("data_raw", "brfss", dir(here("data_raw", "brfss"))) |> 
  set_names(c(2019:2023)) |> 
  map(~read_xpt(.x, col_select = any_of(my_cols)))


brfss <- brfss |> 
  map(janitor::clean_names) 

fips <- tibble(
  state_abb = tidycensus::fips_codes$state,
  fips = as.numeric(tidycensus::fips_codes$state_code)) |> 
  unique()

brfss <- brfss |> 
  tibble() |> 
  mutate(year = names(brfss)) |> 
  unnest(brfss) |> 
  left_join(fips, by = c("state" = "fips")) |> 
 select(state_code = state,
         state = state_abb,
        everything())


brfss |> 
  drop_na(trnsgndr) |> 
  filter(trnsgndr %in% c(1:3, 7)) |> 
  group_by(state, year) |> 
  summarize(n = n()) |> 
  pivot_wider(id_cols = state,
              names_from = year,
              values_from = n,
              names_prefix = "n_") |>
  print(n = Inf)
  
     