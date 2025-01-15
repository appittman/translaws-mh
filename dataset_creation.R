####################### merging hps data with laws data
library(tidyverse)
library(here)

load(here("data", "hps_data.Rdata")) # survey data
load(here("data", "healthcare_laws_data.Rdata")) # laws data
load(here("data", "panelsetup.Rdata")) # information about survey waves

##### creating an 'empty' state*week panel (30 weeks, 50 states, 1500 total)
p <- expand_grid(state = state.abb, week = 34:63)
#using waves 34-63 of HPS data

##### joining with the corresponding data collection dates
p <- left_join(p, panelsetup)

##### pull the FIRST trans healthcare law passed in each state
l <- healthcare_laws_data |> 
  group_by(state) |> 
  slice_min(date) |> 
  rename(bill_date = date, bill_url = url)


##### join with 'empty' panel, then create a variable for if each state is not-yet-treated, treated, or never-treated

##### if the bill passed *during* the data collection period for a wave, I consider that wave as 'treated'
d <- left_join(p, l) |> 
  mutate(state_treatment_status = 
           case_when(bill_date > enddate ~ "not yet treated",
                     bill_date < startdate ~ "treated",
                     is.na(bill_date) ~ "never treated",
                     .default = "contaminated wave")) |> 
  select(state, week, startdate, enddate, bill_date, state_treatment_status, everything())

###### trimming it up for merging

d <- d |> 
  select(-c(bill, bill_url)) |> 
  arrange(state)

###### creating a variable that holds the week a state was treated
d <- d |> 
  drop_na(state_treatment_status) |> 
  mutate(treated_week = if_else(state_treatment_status == "treated" &
                                  lag(state_treatment_status) %in% c("not yet treated", "contaminated wave"), 
                                week, NA_integer_)) |> 
  group_by(state) |> 
  fill(treated_week, .direction = "downup")


############# merging with the hps data, creating a variable that's a summary of the PHQ-4 questions, trimming up the dataset so it only has what I need

dfull <- left_join(hps_data, d) |> 
  select(id, state, week, state_treatment_status, treated_week, everything()) |> 
  mutate(phq_4 = anxious + worry + interest + down) |> 
  select(-c(anxious, worry, interest, down)) |> 
  relocate(phq_4, .before = "state") |> 
  select(-c(ends_with("imp"))) |> 
  drop_na(phq_4) |> 
  drop_na(queer)

#1953207 #total observations
#1706298 #after dropping NA on phq-4
#1688295 #after dropping NA on queer


### saving
save(dfull, file = here("data", "dfull.Rdata"))
