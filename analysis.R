########### setup
library(here)
library(tidyverse)
library(srvyr)
library(did)


load(here("data", "dfull.Rdata"))
load(here("data", "wgtlist.Rdata"))

dwgt <- left_join(dfull, wgtlist, by = c("id", "week"))


### make a fips code dataframe for id numbers later
fips <- tidycensus::fips_codes |> 
  tibble() |> 
  select(state, state_code) |> 
  unique() |> 
  mutate(state_code = as.integer(state_code))

## set seed for bootstrapping stuff
set.seed(623)


################################# ANALYSIS 1: comparing queer people in treated states to non-queer people in those same states

d1 <- dwgt |> 
  filter(state_treatment_status != "never treated")

d1 <- as_survey_rep(d1,
              weights = pweight,
              repweights = pweight1:pweight80)


########## calculate weighted means, then set up the dataframe for use with the `did` package

d1_means <- d1 |> 
  group_by(state, week, treated_week, queer) |> 
  summarize(mean_phq = survey_mean(phq_4),
            n = n()) |> 
  left_join(fips) |> 
  mutate(id = as.integer(paste0(state_code, queer))) |> 
  mutate(treated_week = if_else(queer == 1, treated_week, 0))


####### run did

out1 <- att_gt(data = d1_means,
               yname = "mean_phq",
               tname = "week",
               gname = "treated_week",
               idname = "id",
               clustervars = "state",
               panel = TRUE,
               allow_unbalanced_panel = TRUE)


aggte(out1, type = "dynamic",
      min_e = -10,
      max_e = 9)



############################ ANALYSIS 2: compare queer people in treated states to queer people in non-treated states

d2 <- dwgt |> 
  filter(queer == 1)

d2 <- as_survey_rep(d2,
                    weights = pweight,
                    repweights = pweight1:pweight80)


########## calculate weighted means, then set up the dataframe for use with the `did` package
  
d2_means <-  d2 |> 
  group_by(state, week, treated_week) |> 
  summarize(mean_phq = survey_mean(phq_4),
            n = n()) |> 
  left_join(fips) |> 
  filter(state != "AR") |>  #arkansas is always-treated so need to take them out
  mutate(treated_week = if_else(is.na(treated_week), 0, treated_week))


#### run did

out2 <- att_gt(data = d2_means,
               yname = "mean_phq",
               tname = "week",
               gname = "treated_week",
               idname = "state_code",
               clustervars = "state",
               panel = TRUE,
               allow_unbalanced_panel = TRUE)

aggte(out2, type = "dynamic",
          min_e = -10,
          max_e = 9,
      bstrap = TRUE,
      cband = TRUE,
      clustervars = "state")


#### trying out this second analysis with synthetic control

d2g <- d2_means |> 
  mutate(treatment = case_when(treated_week == 0 ~ 0,
                treated_week > 0 & week >= treated_week ~ 1,
                treated_week > 0 & week < treated_week ~ 0)) |> 
  filter(!(treated_week != 0 & (week - treated_week > 9 | week - treated_week < -10)))

out3 <- gsynth::gsynth(data = d2g,
                     Y = "mean_phq",
                     D = "treatment",
                     se = TRUE,
                     r = 0,
                     CV = FALSE,
                     inference = "parametric",
                     index = c("state", "week"))

plot(out3, type = "gap")

######################## SENSITIVITY ANALYSES

### include percentage queer, black, Republican, college graduate
### take out trans folks

################### DESCRIPTIVES

d1_means |> 
  mutate(treated_week = if_else(treated_week == 0, NA_integer_,
                                treated_week)) |> 
  group_by(state) |> 
  fill(treated_week, .direction = "downup") |> 
  filter(state != "AR") |> 
  filter(week - treated_week < 10 & week - treated_week > -11) |> 
  ggplot(aes(x = week, y = mean_phq)) +
  geom_line(aes(color = as.factor(queer), group = queer)) +
  facet_wrap("state") +
  geom_vline(aes(xintercept = treated_week - .5),
             linetype = "dashed") +
  xlim(33,64) +
  theme(legend.position = "bottom") +
  labs(x = "HPS Wave",
       y = "Mean PHQ-4 Score",
       color = "LGBTQ+")


d2_means |> 
  filter(treated_week == 0 | state == "AL") |> 
  ggplot(aes(x = week, group = state)) +
  geom_line(aes(y = mean_phq), alpha = .3) +
  geom_line(data = d2_means[d2_means$state == "AL",],
            aes(x = week, y = mean_phq), color = "tomato",
            linewidth = 1) +
  geom_vline(data = d2_means[d2_means$state == "AL",],
             aes(xintercept = treated_week),
             linetype = "dashed")


  