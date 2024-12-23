## setup
library(tidyverse)
library(readxl)
library(here)

## read in excel data
## note: this data was obtained from the following websites:


# https://www.aclu.org/documents/past-legislation-affecting-lgbt-rights-across-country-2019
# https://www.aclu.org/documents/past-legislation-affecting-lgbt-rights-across-country-2020
# https://www.aclu.org/documents/legislation-affecting-lgbtq-rights-across-country-2021
# https://www.aclu.org/documents/legislation-affecting-lgbtq-rights-across-country-2022
# https://www.aclu.org/legislative-attacks-on-lgbtq-rights-2023

# in 2018-2022, the ACLU separated the bills into sections, and I only included those in the anti-trans section
# in 2023, they had to be tagged with one of the following: public accomodations bans, school facilities bans, school sports bans, healthcare restrictions, drag bans, barriers to accessing accurate ID

laws_data <- read_xlsx(here("data_raw", "antitranslaws.xlsx"))

states <- tibble(state.name, state.abb)

laws_data <- left_join(laws_data, states, by = c("state_name" = "state.name")) |>
  print(n = Inf)

laws_data <- laws_data |>
  mutate(state = case_when(is.na(state_name) ~ state,
                           !is.na(state_name) ~ state.abb)) |>
  select(-c(state_name, state.abb))

# total: 579 laws
# need to get data about them from LegiScan

laws_data <- laws_data |> 
  mutate(bill = sub("/.*", "", bill),
         bill = str_remove_all(bill, "[[:space:]]"),
         legiscan_url = paste0("https://legiscan.com/",
                               state,
                               "/bill/",
                               bill,
                               "/",
                               year)) |> 
  select(-date)

# from here, looked up each url manually and entered either "dead" or "passed" in the status
# for bills that were passed, I included the date they were passed

# did a third of these on 12/22/24, here's that data

complete <- read_csv(here("data_raw", "incomplete_laws_data.csv"))

laws_data <- left_join(laws_data, complete)


write_csv(laws_data, file = here("data_raw", "partial_laws_data.csv"))

#################################### need to do the rest