# setup
library(tidyverse)
library(here)
library(did)


load(here("data_clean", "analytic_datasets.Rdata"))
load(here("data_clean", "analytic_datasets_trans.Rdata"))

if (dir.exists(here("did_results"))) {
  message("did results will be saved in ", here("did_results"))
} else {
  dir.create(here("did_results"))
  message("did results will be saved in ", here("did_results"))
}


####### Analysis 1
# without controls

tictoc::tic()
did1a <- att_gt(yname = "phq4",
       tname = "time",
       gname = "treatment_time",
       data = dq1,
       weightsname = "pweight",
       clustervars = "state",
       panel = FALSE,
       pl = TRUE,
       cores = 4,
       print_details = TRUE)
tictoc::toc()
# takes about 28 minutes

save(did1a, file = here("did_results", "did1a.Rdata"))

did1a_te <- aggte(did1a,
      type = "group",
      min_e = -10,
      max_e = 10)

####### Analysis 2
# without controls

######## Analysis 3
# without controls
tictoc::tic()
did3a <- att_gt(yname = "phq4",
                tname = "time",
                gname = "treatment_time",
                data = dw1,
                weightsname = "pweight",
                clustervars = "state",
                panel = FALSE,
                print_details = TRUE)
tictoc::toc()

tictoc::tic()
did3a_te_g <- aggte(did3a,
                    type = "group",
                    balance_e = 2)

did3a_te_d <- aggte(did3a,
                    type = "dynamic",
                    min_e = -5,
                    max_e = 5)
tictoc::toc()

save(did3a, file = here("did_results", "did3a.Rdata"))

####### Analysis 4
# without controls
did4a <- att_gt(yname = "phq4",
                tname = "time",
                gname = "treatment_time",
                data = dw2,
                weightsname = "pweight",
                clustervars = "state",
                panel = FALSE,
                print_details = TRUE)


save(did4a, file = here("did_results", "did4a.Rdata"))


did4a_te <- aggte(did4a,
      type = "dynamic",
      min_e = -6,
      max_e = 6)

# hm. big fat nothing on all accounts
# not great, but...we'll see

didt <- att_gt(data = dt,
               yname = "phq4",
               tname = "time",
               gname = "treatment_time",
               panel = FALSE,
               weightsname = "pweight")


ggdid(didt, ncol = 3)

save(didt, file = here("did_results", "didt.Rdata"))


didt_te <- aggte(didt,
                 type = "dynamic",
                 min_e = -10,
                 max_e = 10)

didt_te


ggdid(didt_te)
# ok i guess!
# going with that for now...
  
save(didt_te, file = here("did_results", "didt_te.Rdata"))
  
# weird things happen at the phase 3/4 switchover...i'll figure that out in the future
# until then...

dt_2 <- dt |> 
  filter(time < 31)

didt_2 <- att_gt(data = dt_2,
                 yname = "phq4",
                 tname = "time",
                 gname = "treatment_time",
                 panel = FALSE,
                 weightsname = "pweight")


didt_te_2 <- aggte(didt_2,
                   type = "dynamic",
                   min_e = -10,
                   max_e = 10)

save(didt_2, file = here("did_results", "didt_2.Rdata"))

didt_te_2

