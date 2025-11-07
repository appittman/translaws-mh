# in this file, I take the cleaned data and get it ready for analysis

######################## Setup

library(tidyverse)
library(here)

load(here("data_clean", "first_laws.Rdata"))

# setting up the panel info in a smaller dataset to make it easier
load(here("data_clean", "hps_data.Rdata"))

panelsetup <- hps_data |> 
  group_by(state, time, startdate, enddate) |> 
  summarize(n = n()) |> 
  select(-n) |> 
  ungroup()

# HPS data starts on July 21, 2021
# looking at the first of each type, as well as the first overall, for a total of 4 analyses
# for each analysis, any state that passed a law before July 21, 2021 or after October 30, 2023 will be excluded from analysis

# bathroom bills
bathroom_sample <- laws_data |> 
  select(state, state_name, bathroom_bill, bathroom_date) |> 
  mutate(exclude = case_when(
    bathroom_date > mdy("October 30 2023") ~ 1,
    bathroom_date < mdy("July 21 2021") ~ 1,
    is.na(bathroom_date) ~ 1,
    .default = 0
  )) |> 
 filter(exclude == 0) |> 
  select(-exclude) |> 
  right_join(panelsetup) |> 
  drop_na()
  
bathroom_treatment <- bathroom_sample |> 
  filter(bathroom_date < enddate) |> 
  group_by(state) |> 
  slice_min(time) |> 
  ungroup() |> 
  select(state, treatment_time = time)

bathroom_panel <- bathroom_sample |> 
  left_join(bathroom_treatment)

# sports bills
sports_sample <- laws_data |> 
  select(state, state_name, sports_bill, sports_date) |> 
  mutate(exclude = case_when(
    sports_date > mdy("October 30 2023") ~ 1,
    sports_date < mdy("July 21 2021") ~ 1,
    is.na(sports_date) ~ 1,
    .default = 0
  )) |> 
  filter(exclude == 0) |> 
  select(-exclude) |> 
  right_join(panelsetup) |> 
  drop_na()

sports_treatment <- sports_sample |> 
  filter(sports_date < enddate) |> 
  group_by(state) |> 
  slice_min(time) |> 
  ungroup() |> 
  select(state, treatment_time = time)

sports_panel <- sports_sample |> 
  left_join(sports_treatment)

# medical bills
medical_sample <- laws_data |> 
  select(state, state_name, medical_bill, medical_date) |> 
  mutate(exclude = case_when(
    medical_date > mdy("October 30 2023") ~ 1,
    medical_date < mdy("July 21 2021") ~ 1,
    is.na(medical_date) ~ 1,
    .default = 0
  )) |> 
  filter(exclude == 0) |> 
  select(-exclude) |> 
  right_join(panelsetup) |> 
  drop_na()


medical_treatment <- medical_sample |> 
  filter(medical_date < enddate) |> 
  group_by(state) |> 
  slice_min(time) |> 
  ungroup() |> 
  select(state, treatment_time = time)

medical_panel <- medical_sample |> 
  left_join(medical_treatment)

# first bills of any kind
first_sample <- laws_data |> 
  select(state, state_name, first_bill, first_date) |> 
  mutate(exclude = case_when(
    first_date > mdy("October 30 2023") ~ 1,
    first_date < mdy("July 21 2021") ~ 1,
    is.na(first_date) ~ 1,
    .default = 0
  )) |> 
  filter(exclude == 0) |> 
  select(-exclude) |> 
  right_join(panelsetup) |> 
  drop_na()

first_treatment <- first_sample |> 
  filter(first_date < enddate) |> 
  group_by(state) |> 
  slice_min(time) |> 
  ungroup() |> 
  select(state, treatment_time = time)

first_panel <- first_sample |> 
  left_join(first_treatment)


# recoding outcome / control variables in hps data

hps_data <- hps_data |> 
  drop_na(anxious, worry, interest, down) |>
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
  select(state, time, id, age, female, queer, trans_gnc, coldeg, nhb, nho, hisp, phq4, pweight, startdate, enddate)


# merging

hps_data_bathroom <- left_join(hps_data, bathroom_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_sports <- left_join(hps_data, sports_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_medical <- left_join(hps_data, medical_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_first <- left_join(hps_data, first_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

# if a law passed during data collection, I treat that wave as "treated"


dt_bathroom <- hps_data_bathroom |> 
  filter(queer == 1) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(trans_gnc == 1 ~ treatment_time,
                                    .default = 0))
dt_bathroom |> 
  filter(trans_gnc == 1)

# 25311 observations, of which 3808 were trans/gnc

dt_sports <- hps_data_sports |> 
  filter(queer == 1) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(trans_gnc == 1 ~ treatment_time,
                                    .default = 0))

dt_sports |> 
  filter(trans_gnc == 1)

# 47856 observations of which 7391 were trans/gnc

dt_medical <- hps_data_medical |> 
  filter(queer == 1) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(trans_gnc == 1 ~ treatment_time,
                                    .default = 0))

dt_medical |> 
  filter(trans_gnc == 1)

# 54391 observations of which 8393 were trans/gnc

dt_first <- hps_data_first |> 
  filter(queer == 1) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(trans_gnc == 1 ~ treatment_time,
                                    .default = 0))

dt_first |> 
  filter(trans_gnc == 1)

# 51607 observations of which 7949 were trans/gnc


##################### QUEER vs NONQUEER

dq_bathroom <- hps_data_bathroom |> 
  filter(queer %in% c(0,1)) |> 
  relocate(treatment_time, .after = "time") |> 
mutate(treatment_time = case_when(queer == 1 ~ treatment_time,
                                  .default = 0))

dq_medical <- hps_data_medical |> 
  filter(queer %in% c(0,1)) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(queer == 1 ~ treatment_time,
                                    .default = 0))

dq_sports <- hps_data_sports |> 
  filter(queer %in% c(0,1)) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(queer == 1 ~ treatment_time,
                                    .default = 0))

dq_first <- hps_data_first |> 
  filter(queer %in% c(0,1)) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(queer == 1 ~ treatment_time,
                                    .default = 0))


save(hps_data_bathroom, hps_data_first, hps_data_medical, hps_data_sports,
     file = here("data_clean", "datasets_notreatmentgroup.Rdata"))

save(dt_first, dt_medical, dt_sports, dt_bathroom,
     file = here("data_clean", "datasets_transgnc.Rdata"))

save(dq_first, dq_bathroom, dq_sports, dq_medical,
     file = here("data_clean", "datasets_queer.Rdata"))
