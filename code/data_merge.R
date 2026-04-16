library(tidyverse)
library(here)

load(here("data_clean", "all_laws.Rdata"))
load(here("data_clean", "hps_data.Rdata"))

## panel setup
panelsetup <- hps_data |> 
  count(state, time, startdate, enddate) |> 
  select(-n)

## first of each type, and first overall
first_laws <- all_laws |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> 
  rename(lawdate = date)

sports_laws <- all_laws |> 
  filter(type == "sports") |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> 
  rename(lawdate = date)

bathroom_laws <- all_laws |> 
  filter(type == "bathroom") |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> 
  rename(lawdate = date)

medical_laws <- all_laws |> 
  filter(type == "medical") |> 
  group_by(state) |> 
  slice_min(date, with_ties = F) |> 
  rename(lawdate = date)

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

laws_list <- list(first = first_laws,
                  bathroom = bathroom_laws,
                  sports = sports_laws,
                  medical = medical_laws)

panels <- map(laws_list, ~build_panel(.x, panelsetup = panelsetup))

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

## saving data
save(df_queer, df_trans,
     file = here("data_clean", "datasets.Rdata"))
