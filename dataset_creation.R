####################### merging hps data with laws data
library(tidyverse)
library(here)

load(here("data", "hps_data.Rdata")) # survey data
load(here("data", "healthcare_laws_data.Rdata")) # laws data
load(here("data", "panelsetup.Rdata")) # information about survey waves

##### creating an 'empty' state*week panel (30 weeks, 50 states, 1500 total)
p <- tibble(state = rep(state.abb, length.out = 1500), 
       week = rep(34:63, each = 50)) #using waves 34-63 of HPS data

##### joining with the corresponding data collection dates
p <- left_join(p, panelsetup)

##### pull the FIRST trans healthcare law passed in each state
l <- healthcare_laws_data |> 
  group_by(state) |> 
  slice_min(date) |> 
  rename(bill_date = date, bill_url = url)


##### join with 'empty' panel, then create a variable for if each state is not-yet-treated, treated, or never-treated

##### if the bill passed *during* the data collection period for a wave, I consider that wave as 'treated'
d <- left_join(p, l) |> 
  mutate(state_treatment_status = 
           case_when(bill_date > enddate ~ "not yet treated",
                     bill_date <= enddate ~ "treated",
                     is.na(bill_date) ~ "never treated")) |> 
  select(state, week, startdate, enddate, bill_date, state_treatment_status, everything())


######### a bit of visualization (saved a couple in 'plots' folder)

d_labels <- d |> 
  group_by(state) |> 
  slice(1) |> 
  filter(state != "AR")

ggplot(data = d) +
  geom_tile(aes(x = startdate, y = reorder(state, desc(bill_date)), fill = state_treatment_status), color = "black") +
  scale_fill_manual(values = c("lightgreen", "pink", "tomato")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.length.x = unit(.1, "in"),
        axis.minor.ticks.length.x = unit(.05, "in"),
        axis.minor.ticks.x.bottom = element_line(color = "black"),
        axis.ticks.x = element_line(color = "black")) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b\n%Y",
               minor_breaks = "month",
               expand = c(0,0)) +
  guides(x = guide_axis(minor.ticks = TRUE)) +
  labs(x = "Date",
       y = "State",
       fill = "Treatment Status",
        title = "Treatment Schedule Relative to HPS Waves, by State")
  # + geom_point(data = d_labels,
  #            aes(x = bill_date, y = state)) +
  # ggrepel::geom_text_repel(data = d_labels,
  #            aes(x = bill_date, y = state, label = bill))


###### trimming it up for merging

d <- d |> 
  select(state, week, state_treatment_status) |> 
  arrange(state)


d <- d |> 
  mutate(state_treatment_week = case_match(state_treatment_status,
                                           "never treated" ~ 0,
                                           "not yet treated" ~ NA_integer_,
                                           "treated" ~ 1)) |> 
  group_by(state) |> 
  mutate(state_treatment_week = sum(state_treatment_week, na.rm = TRUE),
         state_treatment_week = case_when(state_treatment_week != 0 ~ 63 - state_treatment_week + 1,
                         .default = 0)) |> 
  print(n = Inf)

############# merging with the hps data

dfull <- left_join(hps_data, d) |> 
  select(id, state, week, state_treatment_status, everything())


# making a summary variable for the phq-4 questions (clinical threshold is 6...may end up using that)
dfull <- dfull |> 
  mutate(phq_4 = anxious + worry + interest + down) |> 
  relocate(phq_4, .before = "state") |> 
  relocate(state_treatment_week, .before = "queer") |> 
  relocate(id, .after = everything())

### saving
save(dfull, file = here("data", "dfull.Rdata"))
