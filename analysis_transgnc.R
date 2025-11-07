# setup
library(tidyverse)
library(here)
library(did)

load(here("data_clean", "datasets_transgnc.Rdata"))

if (dir.exists(here("did_results"))) {
  message("did results will be saved in ", here("did_results"))
} else {
  dir.create(here("did_results"))
  message("did results will be saved in ", here("did_results"))
}



####### Bathroom Bills

didb <- att_gt(data = dt_bathroom,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didb_te <- aggte(didb,
      type = "dynamic",
      min_e = -10,
      max_e = 10)


########## Sports Bills

dids <- att_gt(data = dt_sports,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

dids_te <- aggte(dids,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


dids_te

########## Medical Bills

didm <- att_gt(data = dt_medical,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didm_te <- aggte(didm,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didm_te

# first bill of any kind
didf <- att_gt(data = dt_first,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight",
               print_details = TRUE,
               pl = TRUE,
               cores = 4)

didf_te <- aggte(didf,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)


didf_te



didf_controlled <- att_gt(data = dt_first,
                          yname = "phq4",
                          tname = "time",
                          gname = "treatment_time",
                          panel = FALSE,
                          weightsname = "pweight",
                          xformla = ~age+coldeg,
                          print_details = TRUE,
                          pl = TRUE,
                          cores = 4)

didf_te_controlled <- aggte(didf_controlled,
                            type = "dynamic",
                            min_e = -10,
                            max_e = 10)

didf_te_controlled

save(didb, dids, didm, didf, didf_controlled, file = here("did_results", "did_transgnc.Rdata"))
save(didb_te, dids_te, didm_te, didf_te, didf_te_controlled, file = here("did_results", "te_transgnc.Rdata"))
