
# panel setup

first_laws <- all_laws |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> # grab first one to pass in each state
  right_join(tibble(state = state.abb), by = "state") |> # get the rest of the states
  mutate(exclude = case_when(date <= mdy("July 21 2021") ~ 1,
                             date > mdy("July 21 2021") ~ 0,
                             is.na(date) ~ 0,
                             .default = NA_integer_)) # create a flag to drop states which passed their first anti-trans law prior to data collection


firstlawpanel <- panelsetup |> 
  left_join(first_laws, by = "state") |> 
  group_by(state) |> 
  mutate(post_time = case_when(is.na(date) ~ 0,
                               date <= startdate + 7 ~ 1,
                               date > startdate + 7 ~ 0),
         treated_state = if_else(is.na(date), 0, 1),
         treatment_time = if_else(treated_state == 1, 
                                  as.numeric(sum(post_time == 0) + 1), 
                                  Inf),
         event_time = time - treatment_time,
         period = case_when(time %in% c(1:9) ~ 1,
                            time %in% c(10:21) ~ 2,
                            time %in% c(22:30) ~ 3),
         treatment_period = case_when(treatment_time %in% c(1:9) ~ 1,
                                      treatment_time %in% c(10:21) ~ 2,
                                      treatment_time %in% c(22:30) ~ 3,
                                      .default = Inf),
         post_period = case_when(period >= treatment_period ~ 1,
                                 period < treatment_period ~ 0,
                                 .default = NA_integer_))


################## MERGE
### doing some sample size code here for a methods section eventually

hps_data |> nrow() #1953207

hps_data |> filter(queer == 1) |> nrow() #209928

hps_data |> filter(queer == 1) |> 
  drop_na(anxious, worry, interest, down) |> nrow() #183292

fulldata <- hps_data |> 
  filter(queer == 1) |> 
  drop_na(anxious, worry, interest, down, trans_gnc) |> 
  mutate(age = year(enddate) - birth_year,
         female = case_match(genid_birth, # sex dummy
                             1 ~ 0,
                             2 ~ 1),
         coldeg = case_match(educ, # educ dummy
                             5:7 ~ 1,
                             1:4 ~ 0,
                             .default = NA_integer_),
         nhw = case_match(race_rc, # race dummy
                          "1" ~ 1,
                          .default = 0),
         phq4 = anxious + worry + interest + down - 4) |> # outcome var
  left_join(firstlawpanel, by = c("state", "time", "startdate", "enddate")) |> 
  filter(exclude == 0)

# exclude texas
# period 1: waves 1-9
# Jan 21, 2021 to Feb 7, 2022
# period 2: waves 10-21
# Mar 2, 2022 to Feb 13, 2023
# period 3: waves 22-30
# Mar 1, 2023 to Oct 30, 2023

firstlawpanel |> 
  ggplot(aes(y = reorder(state, desc(treatment_time)), xmax = time, fill = factor(post_period), width = 1, height = 1)) +
  geom_rect() +
  geom_vline(aes(xintercept = 10)) +
  geom_vline(aes(xintercept = 22))

fulldata |> 
  filter(trans_gnc == 1) |> 
  group_by(period, treated_state, treatment_period) |> 
  summarize(mean_phq = weighted.mean(phq4, w = pweight, na.rm = T),
            sd_phq = sd(phq4, na.rm = T))  

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

hps_data_rc <- hps_data |> 
  drop_na(anxious, worry, interest, down) |>
  mutate(age = year(enddate) - birth_year,
         female = case_match(genid_birth, # sex dummy for selection
                             1 ~ 0,
                             2 ~ 1),
         coldeg = case_match(educ, # educ dummy to use in did
                             5:7 ~ 1,
                             1:4 ~ 0,
                             .default = NA_integer_),
         nhw = case_match(race_rc, # race dummy to use in did
                          "1" ~ 1,
                          .default = 0),
         phq4 = anxious + worry + interest + down - 4) |> 
  select(state, time, id, age, female, queer, trans_gnc, coldeg, race_rc, nhw, phq4, pweight, startdate, enddate) |>
  filter(time < 31)


# merging

hps_data_bathroom <- left_join(hps_data_rc, bathroom_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_sports <- left_join(hps_data_rc, sports_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_medical <- left_join(hps_data_rc, medical_panel) |> 
  filter(!is.na(treatment_time)) |> 
  filter(time < 31)

hps_data_first <- left_join(hps_data_rc, first_panel) |> 
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
  filter(trans_gnc == 1) |> 
  nrow()

dt_first <- hps_data_first |> 
  filter(queer == 1) |> 
  relocate(treatment_time, .after = "time") |> 
  mutate(treatment_time = case_when(trans_gnc == 1 ~ treatment_time,
                                    .default = 0))



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


#### sample size journey
hps_data |> nrow()

hps_data |> drop_na(c(anxious, worry, interest, down)) |> nrow()

hps_data |> drop_na(c(anxious, worry, interest, down)) |>  filter(queer == 1) |> nrow()

hps_data |> drop_na(c(anxious, worry, interest, down)) |>  filter(queer == 1) |> filter(trans_gnc == 1) |> nrow()
hps_data |> drop_na(c(anxious, worry, interest, down)) |>  filter(queer == 1) |> drop_na(c(birth_year, educ)) |> nrow()

samplesizes <- function(df){
  full <- df |> nrow()
  treated <- df |> filter(trans_gnc == 1) |> nrow()
  states <- df |> group_by(state) |> summarize(n = n()) |> nrow()
  return(c(full = full, treated = treated, states = states))
}

map_df(list(dt_bathroom, dt_first, dt_medical, dt_sports), samplesizes) |> 
  mutate(analysis = c("bathroom", "first", "medical", "sports")) |> 
  knitr::kable()


dt_bathroom |> 
  group_by(treatment_time, time) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  group_by(treatment_time == 0) |> 
  filter(n > 4) |> 
  summarize(mean_n = mean(n))

dt_medical |> 
  group_by(treatment_time, time) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  group_by(treatment_time == 0) |> 
  filter(n > 4) |> 
  summarize(mean_n = mean(n))

dt_sports |> 
  group_by(treatment_time, time) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  group_by(treatment_time == 0) |> 
  filter(n > 4) |> 
  summarize(mean_n = mean(n))

dt_first |> 
  group_by(treatment_time, time) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  group_by(treatment_time == 0) |> 
  filter(n > 4) |> 
  summarize(mean_n = mean(n))


dt_bathroom |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  pull(state)

dt_medical |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  pull(state) |> 
  paste(collapse = ", ")

dt_sports |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  pull(state) |> 
  paste(collapse = ", ")

dt_first |> 
  group_by(state) |> 
  summarize(n = n()) |> 
  pull(state) |> 
  paste(collapse = ", ")

