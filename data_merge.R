# in this file, I take the cleaned data and get it ready for analysis

######################## Setup

library(tidyverse)
library(here)


load(here("data_clean", "hps_data.Rdata"))
load(here("data_clean", "laws.Rdata"))

# HPS data starts on July 21, 2021
# looking at the impact of the FIRST PASSED ANTI-TRANS LAW IN A STATE
# any state that passed an anti-trans law before that will be excluded from analysis

exclude_states <- laws |> 
  filter(date < mdy("July 21 2021")) |> 
  pull(state) |> 
  unique()

exclude_states
# Alabama, Arkansas, Florida, Idaho, Mississippi, Montana, North Carolina, South Dakota, Tennessee, West Virginia

laws <- laws |> 
  filter(!(state %in% exclude_states)) |> 
  group_by(state) |> 
  slice_min(date, with_ties = TRUE) |> 
  select(state, date, type) |> 
  ungroup() |> 
  nest(.by = c(state, date)) |> 
  rename(lawdate = date,
         lawdata = data)


# setting up the panel info in a smaller dataset to make it easier
panelsetup <- hps_data |> 
  select(state, time, startdate, enddate) |> 
  unique()

panelsetup <- panelsetup |> 
  arrange(state, time) |> 
  left_join(laws) |> 
  group_by(state) |> 
  filter(lawdate < enddate) |> 
  slice_min(enddate) |> 
  select(state, 
         treatment_time = time)

# if a law passed during data collection, I treat that wave as "treated"

# Kansas is the reason why I have 19 laws but only 18 treatment times - Kansas passed their first anti-trans law in 2025

hps_data <- hps_data |> 
  left_join(panelsetup) |> 
  relocate(wave, .after = everything()) |> 
  relocate(c(time, treatment_time), .after = state_code) |> 
  mutate(treatment_time = case_when(
    !is.na(treatment_time) ~ treatment_time,
    is.na(treatment_time) & state %in% exclude_states ~ NA_integer_,
    is.na(treatment_time) & !(state %in% exclude_states) ~ 0
    # untreated cases get a 0 by `did` package default
  )) |> 
  drop_na(treatment_time)


# recoding outcome and control variables in hps data

hps_data <- hps_data |> 
  drop_na(anxious, worry, interest, down) |>  # 1.822m complete cases
  mutate(age = year(enddate) - birth_year,
         female = case_match(genid_birth, # sex dummy for selection
                             1 ~ 0,
                             2 ~ 1),
         coldeg = case_match(educ, # educ dummy to use in did
                             5:7 ~ 1,
                             1:4 ~ 0,
                             .default = NA_integer_),
         nhb = case_match(race_rc, # race dummies to use in did
                            "2" ~ 1,
                            .default = 0),
         nho = case_match(race_rc,
                          "3" ~ 1,
                          .default = 0),
         hisp = case_match(race_rc,
                           "4" ~ 1,
                           .default = 0),
         phq4 = anxious + worry + interest + down - 4) |> 
  select(state, time, treatment_time, id, age, female, queer, trans_gnc, coldeg, nhb, nho, hisp, phq4, pweight, startdate, enddate)

# set up mini datasets for each analysis
# analysis 1: queer people vs. nonqueer people, same states
# analysis 2: queer people vs. queer people, different states
# analysis 3: women vs. men, same states
# analysis 4: women vs. women, different states


dq1 <- hps_data |> 
  filter(treatment_time > 0) |> 
  drop_na(queer) |> 
  mutate(treatment_time = if_else(queer == 1, treatment_time, 0))

dq2 <- hps_data |> 
  filter(queer == 1)

dw1 <- hps_data |> 
  filter(treatment_time > 0) |> 
  mutate(treatment_time = if_else(female == 1, treatment_time, 0))

dw2 <- hps_data |> 
  filter(female == 1)


# save these so they can be loaded separately to save space in R
save(dq1, dq2, dw1, dw2, file = here("data_clean", "analytic_datasets.Rdata"))


# also want to make a final analytic dataset that compares trans folks to lgbq folks
dt <- hps_data |> 
  filter(queer == 1) |> 
  drop_na(trans_gnc) |> 
  mutate(treatment_time = if_else(trans_gnc == 1, treatment_time, 0))

save(dt, file = here("data_clean", "analytic_datasets_trans.Rdata"))


library(did)
