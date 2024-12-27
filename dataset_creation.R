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

## full_dataset

d <- full_dataset |> 
 drop_na(anxious, worry, interest, down)

(1953207-1706298)/1953207

#12.6% of the sample is missing on one of the PHQ questionnaires
#1706288 people answered all four of the PHQ questions

#1688285 people are identifiably queer or non-queer

(1706288 - 1688285)/1706288
