library(tidyverse)
library(here)

load(here("data_clean", "all_laws.Rdata"))
load(here("data_clean", "hps_data.Rdata"))

## panel setup
panelsetup <- hps_data |> 
  count(state, time, startdate, enddate) |> 
  select(state, time, startdate, enddate)

## first overall
first_laws <- all_laws |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> 
  rename(lawdate = date) |> 
  ungroup()

#splitting up by type
first_laws_bytype <- all_laws |> 
  group_by(state) |> 
  slice_min(date, with_ties = T) |> 
  ungroup() |> 
  nest(.by = "type")

types <- first_laws_bytype |> 
  pull(type)

laws <- first_laws_bytype |> 
  pull(data) |> 
  set_names(types) |> 
  map(~rename(.x, lawdate = date)) |> 
  (\(x) c(x, list("first" = first_laws)))()

#there's only 2 states whose first anti-trans law was a bathroom bill; not enough to make a meaningful analysis. 

laws$bathroom <- NULL

laws$medical$state
laws$sports$state

laws

# NH is a special case: on the same day, they passed a medical law and a sports law. will need to perform subsequent analyses without NH in either group.


## joining with panel setup
## if a law passed within seven days of a survey wave starting, that wave is "treated"

build_panel <- function(laws, panelsetup) {
  
  panelsetup |> 
    left_join(laws, by = "state") |> 
    select(state, time, startdate, enddate, lawdate) |> 
    mutate(state_type = case_when(is.na(lawdate) ~ "nevertreated",
                                  lawdate >= mdy("July 21 2021") &
                                    lawdate <= mdy("October 30 2023") ~
                                    "treated",
                                  lawdate > mdy("October 30 2023") ~
                                    "notyettreated",
                                  lawdate < mdy("July 21 2021") ~
                                    "alreadytreated"),
           post_period = case_when(is.na(lawdate) ~ NA_real_,
                                   lawdate < startdate + 7 ~ 1,
                                   lawdate >= startdate + 7 ~ 0)) |> 
    group_by(state) |> 
    mutate(treatment_period = case_when(state_type == "treated" ~ 
                                          sum(post_period == 0) + 1,
                                        .default = NA_real_)) |> 
    ungroup()
}


panels <- map(laws, ~build_panel(.x, panelsetup = panelsetup))

## narrowing down hps data for quicker/cleaner merging
### comparison group pairing 1: trans people in treated states vs. cis queer people in treated states

hps_queer <- hps_data |> 
  filter(queer == 1,
         !is.na(trans_gnc))

### comparison group pairing 2: trans people in treated states vs. trans people in not-yet-treated states

hps_trans <- hps_data |> 
  filter(trans_gnc == 1)

## merging
merger <- function(hps_data, panel, keep_types) {
  hps_data |>
    left_join(panel, by = c("state", "time", "startdate", "enddate")) |>
    filter(state_type %in% keep_types)
}

df_queer <- map(panels, ~merger(hps_queer,
                                panel = .x,
                                keep_types = "treated"))

df_trans <- map(panels, ~merger(hps_trans,
                                panel = .x,
                                keep_types = c("treated", "notyettreated")))


# doing a little cleaning/recoding
df_queer <- map(df_queer, ~drop_na(.x,
                                   anxious, worry, interest, down) |> 
                  mutate(phq4 = anxious + worry + interest + down - 4, #HPS codes these from 1-4; we need them added as though they are 0-3
                         group = case_when(trans_gnc == 1 ~ treatment_period,
                                           trans_gnc == 0 ~ 0), #`did` package convention: comparison group's group is 0
                         coldeg = case_match(educ, 
                                             c(1:4) ~ 0,
                                             c(5:7) ~ 1,
                                             .default = NA_integer_),
                         nhw = case_match(as.numeric(race_rc),
                                          1 ~ 1,
                                          c(2:4) ~ 0,
                                          .default = NA_integer_),
                         age = year(startdate) - birth_year))

#using the same process on the trans datasets:
df_trans <- map(df_trans, ~drop_na(.x,
                                   anxious, worry, interest, down) |> 
                  mutate(phq4 = anxious + worry + interest + down - 4,
                         group = case_when(state_type == "treated" ~ treatment_period,
                                           state_type == "notyettreated" ~ 0),
                         coldeg = case_match(educ, 
                                             c(1:4) ~ 0,
                                             c(5:7) ~ 1,
                                             .default = NA_integer_),
                         nhw = case_match(as.numeric(race_rc),
                                          1 ~ 1,
                                          c(2:4) ~ 0,
                                          .default = NA_integer_),
                         age = year(startdate) - birth_year))


## saving data
save(df_queer, df_trans,
     file = here("data_clean", "datasets.Rdata"))
