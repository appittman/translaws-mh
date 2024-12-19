#### code for reading in and visualizing the anti-trans laws data


library(tidyverse)
library(readxl)
library(here)

laws_data <- read_xlsx(here("data_raw", "antitranslaws.xlsx")) |> 
  mutate(passed_date = as.Date(passed_date))

laws_data_merge <- laws_data |> 
  select(state, passed_date) |> 
  unique()


laws_data_merge 

load(here("data", "pulsetimeline.Rdata"))


panel <- tibble(state.name) |> 
  slice(rep(1:n(), each = 30)) |> 
  mutate(week = rep(34:63, length.out = 1500)) |> 
  left_join(pulsetimeline) |> 
  left_join(laws_data_merge, by = c("state.name" = "state")) |> 
  mutate(treated_state_year = case_when(is.na(passed_date) ~ 0,
                                        passed_date < enddate ~ 1,
                                        .default = 0)) |> 
  group_by(state.name) |> 
  mutate(number_treated = sum(treated_state_year))



ggplot(data = panel) +
  geom_tile(aes(x = week, y = reorder(state.name, number_treated), 
                     fill = as.factor(treated_state_year)), color = "black") +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  labs(x = "Wave",
       y = "State",
       fill = "Treatment Status") +
  theme_minimal() +
  theme(legend.position = "bottom")


save(panel, file = here("data", "panelsetup.Rdata"))
