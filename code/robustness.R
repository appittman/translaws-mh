library(tidyverse)
library(here)
library(did)

load(here("data_clean", "datasets.Rdata"))

############## ROBUSTNESS: checking that just using never-treated folks for the trans comparison group shows similar results (it does)

did_trans_nt_first <- att_gt(yname = "phq4",
                             tname = "time",
                             gname = "group",
                             data = df_trans$first,
                             panel = FALSE,                            weightsname = "pweight",
                             base_period = "universal",
                             print_details = T)


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

df_trans_nsg <- map(df_trans, drop_small_groups)

did_trans_nt_nsg_first <- att_gt(yname = "phq4",
                                 tname = "time",
                                 gname = "group",
                                 data = df_trans_nsg$first,
                                 panel = FALSE,                            weightsname = "pweight",
                                 base_period = "universal",
                                 print_details = T)


did_trans_nt_nsg_cov_first <- att_gt(yname = "phq4",
                                     tname = "time",
                                     gname = "group",
                                     data = df_trans_nsg$first,
                                     panel = FALSE,                            weightsname = "pweight",
                                     xformla = ~age+coldeg+nhw,
                                     base_period = "universal",
                                     print_details = T)

trans_nt_te_unadj <- aggte(did_trans_nt_first,
                           type = "dynamic",
                           min_e = -6,
                           max_e = 6)

trans_nt_te_nsg <- aggte(did_trans_nt_nsg_first,
                         type = "dynamic",
                         min_e = -6,
                         max_e = 6)

trans_nt_te_nsg_cov <- aggte(did_trans_nt_nsg_cov_first,
                             type = "dynamic",
                             min_e = -6,
                             max_e = 6)


ggdid(trans_nt_te_unadj)
ggdid(trans_nt_te_nsg)
ggdid(trans_nt_te_nsg_cov)
