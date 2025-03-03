####### getting panel setup
library(rvest)
library(tidyverse)
library(janitor)

data23 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/tables.2023.html#list-tab-404305343")


data23 <- data23 |> 
  html_elements("div") |>
  html_elements(xpath =
"/html/body/div[3]/div/div/div[8]/div/div[2]/div[2]/div[2]") |> 
  html_children() |>
  html_children() |> 
  html_children() |> 
  html_children() |> 
  html_text() |> 
  read_delim(delim = "\r\n") |> 
  rename(title = 1) |> 
  mutate(title = str_squish(title)) |> 
  filter(str_detect(title, "Week")) |> 
  mutate(title = str_split(title, ":")) |> 
  unnest_wider(title, names_sep = "_") |> 
  mutate(title_1 = parse_number(title_1)) |> 
  mutate(title_2 = str_split(title_2, "[[:punct:]]")) |> 
  unnest_wider(title_2, names_sep = "_") |> 
  rename(week = 1,
         startdate = 2,
         enddate = 3) |> 
  mutate(startdate = mdy(paste0(str_squish(startdate), " 2023")),
         enddate = mdy(paste0(str_squish(enddate), " 2023"))) |> 
  mutate(startdate = case_when(week == 52 ~ startdate - years(1),
                               .default = startdate),
         enddate = case_when(week == 52 ~ enddate - years(1),
                             .default = enddate))

data22 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/tables.2022.html#list-tab-404305343")

data22 <- data22 |> 
  html_elements("div") |>
  html_elements(xpath = "/html/body/div[3]/div/div/div[8]/div/div[2]/div[2]/div[2]") |> 
  html_children() |>
  html_children() |> 
  html_children() |> 
  html_children() |> 
  html_text() |> 
  read_delim(delim = "\r\n") |> 
  rename(title = 1) |> 
  mutate(title = str_squish(title)) |> 
  filter(str_detect(title, "Week")) |> 
  mutate(title = str_split(title, ":")) |> 
  unnest_wider(title, names_sep = "_") |> 
  mutate(title_1 = parse_number(title_1)) |> 
  mutate(title_2 = str_split(title_2, "[[:punct:]]")) |> 
  unnest_wider(title_2, names_sep = "_") |> 
  rename(week = 1,
         startdate = 2,
         enddate = 3) |> 
  mutate(startdate = mdy(paste0(str_squish(startdate), " 2022")),
         enddate = mdy(paste0(str_squish(enddate), " 2022"))) |> 
  mutate(startdate = case_when(week == 41 ~ startdate - years(1),
                               .default = startdate))


data21 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/tables.2021.html#list-tab-404305343")

data21 <- data21 |> 
  html_elements("div") |>
  html_elements(xpath = "/html/body/div[3]/div/div/div[8]/div/div[2]/div[2]/div[2]") |> 
  html_children() |>
  html_children() |> 
  html_children() |> 
  html_children() |> 
  html_text() |> 
  read_delim(delim = "\r\n") |> 
  rename(title = 1) |> 
  mutate(title = str_squish(title)) |> 
  filter(str_detect(title, "Week")) |> 
  mutate(title = str_split(title, ":")) |> 
  unnest_wider(title, names_sep = "_") |> 
  mutate(title_1 = parse_number(title_1)) |> 
  mutate(title_2 = str_split(title_2, "[[:punct:]]")) |> 
  unnest_wider(title_2, names_sep = "_") |> 
  rename(week = 1,
         startdate = 2,
         enddate = 3) |> 
  mutate(startdate = mdy(paste0(str_squish(startdate), " 2021")),
         enddate = mdy(paste0(str_squish(enddate), " 2021"))) |> 
  filter(week > 33)


surveydates <- bind_rows(data21, data22, data23) |> 
  arrange(week)

save(surveydates, file = here("data", "surveydates.Rdata"))


########## realized I need to combine into larger waves
wavedates <- surveydates |> 
  arrange(week) |> 
  filter(week > 39) |> 
  mutate(wave = rep(1:8, each = 3)) |> 
  group_by(wave) |> 
  mutate(wave_start = min(startdate),
         wave_end = max(enddate)) |> 
  select(-c(startdate, enddate))

save(wavedates, file = here("data", "wavedates.Rdata"))
