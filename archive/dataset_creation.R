####################### merging hps data with laws data
library(tidyverse)
library(here)

load(here("data", "hps_data.Rdata")) # survey data
load(here("data", "healthcare_laws_data.Rdata")) # laws data
load(here("data", "surveydates.Rdata")) # information about survey waves


##### creating an 'empty' state*week panel (24 weeks, 50 states, 1200 total)
p <- expand_grid(state = state.abb, week = 40:63)
#using waves 40-63 of HPS data

##### joining with the corresponding data collection dates
p <- left_join(p, surveydates)

##### pull the FIRST trans healthcare law passed in each state
l <- healthcare_laws_data |> 
  group_by(state) |> 
  slice_min(date) |> 
  rename(bill_date = date, bill_url = url)


##### join with 'empty' panel, then create a variable for if each state is not-yet-treated, treated, or never-treated

##### using the midpoint of the wave as the cutoff

p <- p |> 
  group_by(week) |> 
  mutate(wave_mid = startdate + 
           round((enddate - startdate)/2)) |> 
  ungroup()




d <- left_join(p, l) |> 
  mutate(state_treatment_status = 
           case_when(bill_date >= wave_mid ~ "not yet treated",
                     bill_date < wave_mid ~ "treated",
                     is.na(bill_date) ~ "never treated"))

###### trimming it up for merging

d <- d |> 
  select(-c(wave_mid, bill, bill_url)) |> 
  arrange(state)

###### creating a variable that holds the wave a state was treated
d <- d |> 
  drop_na(state_treatment_status) |> 
  mutate(treated_wave = if_else(state_treatment_status == "treated" &
                                  lag(state_treatment_status) == "not yet treated", 
                                week, NA_integer_)) |> 
  group_by(state) |> 
  fill(treated_wave, .direction = "downup") |> 
  mutate(treated_wave = if_else(state == "AR", #making this so that I can easily filter arkansas out (they were treated before wave 1)
                                -1,
                                treated_wave)) |> 
  ungroup()

############# merging with the hps data, creating a variable that's a summary of the PHQ-4 questions, trimming up the dataset so it only has what I need

dfull <- left_join(hps_data, d) |> 
  select(id, state, week, state_treatment_status, treated_wave, everything()) |> 
  mutate(phq_4 = anxious + worry + interest + down) |> 
  select(-c(anxious, worry, interest, down)) |> 
  relocate(phq_4, .before = "state") |> 
  select(-c(ends_with("imp"))) |> 
  drop_na(queer) |> 
  drop_na(phq_4)


#1570299 total respondents
#of those, 1544930 able to identify as likely queer or not queer
#of those, 1355240 answered all of the PHQ-4 questions


### saving
save(dfull, file = here("data", "dfull.Rdata"))
