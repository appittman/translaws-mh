####################### merging hps data with laws data
library(tidyverse)
library(here)


load(here("data", "hps_data.Rdata"))
load(here("data", "passed_laws_data.Rdata"))
load(here("data", "panelsetup.Rdata"))

# panelsetup object has each state*week, along with whether or not they are considered 'treated'
# hps data has individual observations, each nested within state*weeks

full_dataset <- left_join(hps_data, panelsetup, by = c("state" = "state",
                                       "week" = "week"))

save(full_dataset, file = here("data", "full_dataset.Rdata"),
     compress = "bzip2")

##################### probably don't want to have to load in and use the full dataset for everything
## should create smaller datasets for specific analyses

## DID setup implies 2 pairs of control-treated groups:
# 1 - treated is LGBTQ people in a treated state, control is straight people in a treated state
# 2 - treated is LGBTQ people in a treated state, control is LGBTQ people in a non-treated state

## In addition, I'll want to run some models with just trans/gnc folks, and some with all queer folks


## control group 1: LGBTQ vs. straight people, same state
# always-treated and never-treated are excluded here
df_treated_states <- full_dataset |> 
  filter(number_treated > 0 & number_treated < 30)

## control group 2: LGBTQ vs. LGBTQ people, different states
# always-treated states are still excluded

df_only_queer <- full_dataset |> 
  filter(number_treated < 30 & queer == 1)


save(df_treated_states, df_only_queer, file = here("data", "smaller_datasets.Rdata"))
