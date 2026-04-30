#installing synthdid package (only necessary once)
#devtools::install_github("synth-inference/synthdid")
library(synthdid)
library(gsynth)
library(ggplot2)
library(tidyverse)
library(here)
synthdid::california_prop99

load(here("data_clean", "hps_data.Rdata"))
load(here("data_clean", "all_laws.Rdata"))

