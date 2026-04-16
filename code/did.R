## setup
library(tidyverse)
library(here)
library(did)

load(here("data_clean", "datasets.Rdata"))


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


did_trans <- map(df_trans, ~att_gt(yname = "phq4",
       tname = "time",
       gname = "group",
       data = .x,
       panel = FALSE,
       control_group = "notyettreated",
       weightsname = "pweight",
       base_period = "universal"
       ))

did_queer <- map(df_queer, ~att_gt(yname = "phq4",
                                   tname = "time",
                                   gname = "group",
                                   data = .x,
                                   panel = FALSE,
                                   weightsname = "pweight",
                                   base_period = "universal"
                                   ))

######################### ROBUSTNESS CHECKS: Getting Rid of Small Groups

drop_small_groups <- function(df) {
  
  small_groups <- df |> 
    group_by(group, time) |> 
    summarize(n = n()) |> 
    filter(n < 10) |> 
    summarize(n = n()) |> 
    pull(group)
  
  df |> 
    filter(!(group %in% small_groups))
}


df_queer_nsg <- df_queer |> 
  map(drop_small_groups)

df_trans_nsg <- df_trans |> 
  map(drop_small_groups)
                            
did_queer_nsg <- df_queer_nsg |> map(~att_gt(yname = "phq4",
                                   tname = "time",
                                   gname = "group",
                                   data = .x,
                                   panel = FALSE,
                                   weightsname = "pweight",
                                   base_period = "universal",
                                   print_details = T))

did_trans_nsg <- df_trans_nsg |> map(~att_gt(yname = "phq4",
                                             tname = "time",
                                             gname = "group",
                                             data = .x,
                                             panel = FALSE,
                                             control_group = "notyettreated",
                                             weightsname = "pweight",
                                             base_period = "universal",
                                             print_details = T))

################## ROBUSTNESS CHECKS: Adding Pre-Treatment Covariates

# Utah gives some trouble here
# It's in a group of its own for the medical analyses
# And there aren't enough nonwhite folks there to handle the nhw covariate.
# So, I take out Utah for the Medical analyses

df_queer_nsg$medical <- df_queer_nsg$medical |> filter(state != "UT")

df_trans_nsg$medical <- df_trans_nsg$medical |> filter(state != "UT")

did_queer_cov <- df_queer_nsg |> map(~att_gt(yname = "phq4",
                                             tname = "time",
                                             gname = "group",
                                             data = .x,
                                             xformla = ~age+coldeg+nhw,
                                             panel = FALSE,
                                             weightsname = "pweight",
                                             base_period = "universal",
                                             print_details = T))



did_trans_cov <- df_trans_nsg |> map(~att_gt(yname = "phq4",
                                             tname = "time",
                                             gname = "group",
                                             data = .x,
                                             xformla = ~age+coldeg+nhw,
                                             panel = FALSE,
                                             control_group = "notyettreated",
                                             weightsname = "pweight",
                                             base_period = "universal",
                                             print_details = T))
  

### saving DID results
save(did_queer,
     did_queer_nsg,
     did_queer_cov,
     did_trans,
     did_trans_nsg,
     did_trans_cov,
     file = here("did_results", "results_0413.Rdata"))
