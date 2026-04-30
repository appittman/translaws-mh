## setup
library(tidyverse)
library(here)
library(did)

load(here("data_clean", "datasets.Rdata"))

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

did_queer_nsg_cov <- df_queer_nsg |> map(~att_gt(yname = "phq4",
                                             tname = "time",
                                             gname = "group",
                                             data = .x,
                                             xformla = ~age+coldeg+nhw,
                                             panel = FALSE,
                                             weightsname = "pweight",
                                             base_period = "universal",
                                             print_details = T))



did_trans_nsg_cov <- df_trans_nsg |> map(~att_gt(yname = "phq4",
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
     did_queer_nsg_cov,
     did_trans,
     did_trans_nsg,
     did_trans_nsg_cov,
     file = here("did_results", "results_0429.Rdata"))
