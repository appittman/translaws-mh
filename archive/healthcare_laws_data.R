library(here)
library(tidyverse)

d <- read_csv(here("data_raw", "healthcare_laws_data.csv"))


healthcare_laws_data <- d |> 
  mutate(date = mdy(date)) |> 
  filter(healthcare == "yes") |> 
  arrange(date) |> 
  select(state, bill, date, url)


save(healthcare_laws_data, file = here("data", 
                                       "healthcare_laws_data.Rdata"))
