library(tidyverse)
library(janitor)
library(here)
library(did)

deaths <- read_delim(here("suicides.txt")) |> 
  clean_names() |> 
  select(state = residence_state, month, month_code, deaths) |> 
  mutate(date = ymd(paste0(month_code, "/01")),
         year = str_sub(month, start = -4)) |> 
  select(state, date, year, deaths)


population <- readxl::read_xlsx(here("NST-EST2024-POP.xlsx"), skip = 3,
                  n_max = 56) |> 
  select(-...2) |> 
  rename(state = ...1) |> 
  filter(str_detect(state, "\\.")) |> 
  mutate(state = str_sub(state, start = 2)) |> 
  pivot_longer(!state,
               names_to = "year",
               values_to = "population")


suicides <- left_join(deaths, population, by = c("state", "year")) |> 
  mutate(suiciderate = (deaths / population)*10000) |>  #rate per 10k
  rename(state_name = state) |> 
  drop_na()

suicides <- tibble(state = state.abb, 
                   state_name = state.name, 
                   region = state.region) |> 
  right_join(suicides) 


suicides <- suicides |> 
  drop_na()
