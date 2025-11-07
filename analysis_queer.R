# setup
library(tidyverse)
library(here)
library(did)

load(here("data_clean", "datasets_queer.Rdata"))

if (dir.exists(here("did_results"))) {
  message("did results will be saved in ", here("did_results"))
} else {
  dir.create(here("did_results"))
  message("did results will be saved in ", here("did_results"))
}


####### Bathroom Bills

didq_b <- att_gt(data = dq_bathroom,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didq_b_te <- aggte(didq_b,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didq_b_te

########## Sports Bills

didq_s <- att_gt(data = dq_sports,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didq_s_te <- aggte(didq_s,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didq_s_te

########## Medical Bills

didq_m <- att_gt(data = dq_medical,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didq_m_te <- aggte(didq_m,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didq_m_te

########### first bill of any kind
didq_f <- att_gt(data = dq_first,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didq_f_te <- aggte(didq_f,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didq_f_te

save(didq_b, didq_s, didq_m, 
     didq_f, 
     file = here("did_results", "did_queer.Rdata"))
save(didq_b_te, didq_s_te, didq_m_te, 
     didq_f_te, 
     file = here("did_results", "te_queer.Rdata"))
