################ Setup

library(here)
library(tidyverse)
################ PDFs downloaded from MAP on 4/5/26

############ PDFs parsed by Claude, had it export into csvs

bathroomlaws <- read_csv(here("data_raw", "laws", "bathroomlaws.csv"))

medicallaws <- read_csv(here("data_raw", "laws", "youthmedicalcarelaws.csv"))

sportslaws <- read_csv(here("data_raw", "laws", "sportslaws.csv"))


db <- bathroomlaws |> 
  select(state = State,
         law = `Title of Law`,
         date = `Date Passed`) |> 
  mutate(type = "bathroom",
         date = mdy(date))

ds <- sportslaws |> 
  select(state = State,
         law = `Title of Law`,
         date = `Date Passed`) |> 
  mutate(type = "sports",
         date = mdy(date))

dm <- medicallaws |> 
  select(state = State,
         law = `Title of Law`,
         date = `Date Passed`) |> 
  mutate(type = "medical",
         date = mdy(date))


all_laws <- bind_rows(db, dm, ds) |>
  arrange(date)

all_laws <- all_laws |> 
  left_join(tibble(state = state.name, st = state.abb)) |> 
  filter(state != "Puerto Rico") |> 
  select(state = st,
         law, date, type)

save(all_laws, file = here("data_clean", "all_laws.Rdata"))

all_laws |> 
  group_by(state) |> 
  slice_min(date) |> 
  arrange(desc(date))
