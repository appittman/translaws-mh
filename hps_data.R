# this is an R script for scraping and compiling data from the CDC's Household Pulse Survey (public use microdata)
# if you're reading this and you work for the CDC i'm sorry if this is a rude scraping algorithm...I'm a novice lol

############################ Step 0: Setup
library(tidyverse)
library(here)
library(rvest)
library(curl)
library(janitor)

if (file.exists(here("zipfiles"))) {
  print("you've already got a zipfiles directory here...awkward")
} else {
  dir.create(here("zipfiles"))
}

if (file.exists(here("csvfiles"))) {
  print("you've already got a csvfiles directory here...DOUBLE awkward")
} else {
  dir.create(here("csvfiles"))
}

#url: https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.html

############################ Step 1: get a list of urls
# need weeks 34-63, which were in 2021-2023
# got these files by navigating to the page above, clicking the tab that corresponds with the year I want, then saving a copy of the html to my computer in the project folder under the subfolder "data_raw"

html2023 <- read_html(here("hpspublicuse_2023.html"))

urls23 <- html2023 |> 
  html_elements("a") |> 
  html_attr("href") |> 
  tibble() |> 
  rename(url = 1) |> 
  mutate(zipfile = str_detect(url, "CSV\\.zip")) |> 
  filter(zipfile) |> 
  pull(url)

html2022 <- read_html(here("hpspublicuse_2022.html"))

urls22 <- html2022 |> 
  html_elements("a") |> 
  html_attr("href") |> 
  tibble() |> 
  rename(url = 1) |> 
  mutate(zipfile = str_detect(url, "CSV\\.zip")) |> 
  filter(zipfile) |> 
  pull(url)

html2021 <- read_html(here("hpspublicuse_2021.html"))

urls21 <- html2021 |> 
  html_elements("a") |> 
  html_attr("href") |> 
  tibble() |> 
  rename(url = 1) |> 
  mutate(zipfile = str_detect(url, "CSV\\.zip")) |> 
  filter(zipfile) |> 
  filter(!str_detect(url, "wk2|wk30|wk31|wk32|wk33")) |> 
  pull(url)

urls_list <- c(urls21, urls22, urls23)

# gut check here: should have 30 urls, one for each zip file

######################### Step 2: download the zip files

# highly suggest you start with one and test it so you don't die, uncomment the following two lines
#testurl <- urls_list[1]
#curl_download(testurl, destfile = here("zipfiles", "test1.zip"))

basenames <- tibble(urls_list) |> 
  mutate(base = basename(urls_list)) |> 
  pull(base)

multi_download(urls_list, destfiles = here("zipfiles", basenames))

# a few of the zipfiles didn't download because the urls were wonky
urls_fixed <- tibble(urls_list) |> 
  filter(!str_detect(urls_list, "^h")) |> 
  mutate(urls_fixed = paste0("https:", urls_list),
         base = basename(urls_list))

multi_download(urls_fixed$urls_fixed, destfiles = here("zipfiles", urls_fixed$base))


################## Step 3: Unzip the zip files, grabbing only the csvs

csv_names <- map(.x = here("zipfiles", dir(here("zipfiles"))), 
    .f = ~unzip(.x, list = TRUE)) |> 
  tibble() |> 
  unnest(cols = 1) |> 
  filter((str_detect(Name, ".csv"))) |> 
  filter(!(str_detect(Name, "repwgt"))) |> 
  pull(Name)


# try with ONE!!! pls
# unzip(here("zipfiles", dir(here("zipfiles")))[1], files = csv_names[1],
     # exdir = here("csvfiles"))


map2(.x = here("zipfiles", dir(here("zipfiles"))), .y = csv_names,
     .f = ~unzip(.x, files = .y, exdir = here("csvfiles")))



######################## Step 4: read csvs and combine

#first, know the variable names you want to use

# id: scram
# week: week
# survey weight: pweight, hweight
# state: est_st
# age: tbirth_year
# race: rrace, rhispanic
# gender id: genid_describe
# sexual orientation: sexual_orientation
# gender id at birth: egenid_birth, agenid_birth
# outcomes: anxious, worry, interest, down
# income: income
# education: eeduc, aeduc

datalist <- map(.x = here("csvfiles", dir(here("csvfiles"))), 
    .f = ~read_csv(.x,
         col_select = c("SCRAM",
                        "WEEK",
                        "EST_ST",
                        contains("WEIGHT"),
                        contains("BIRTH_YEAR"),
                        contains("RACE"),
                        contains("HISPANIC"),
                        contains("GENID"),
                        "SEXUAL_ORIENTATION",
                        contains("INCOME"),
                        "EEDUC",
                        "AEDUC",
                        "ANXIOUS",
                        "WORRY",
                        "INTEREST",
                        "DOWN")))

data_raw <- datalist |> 
  tibble() |> 
  unnest(col = datalist)

# save this big guy to data_raw so you don't have to go through any of this again
# bzip2 takes longer but is smaller at the end, choose wisely

#save(data_raw, file = here("data_raw", "data_raw_bin.Rdata"), compress = "gzip")
save(data_raw, file = here("data_raw", "data_raw_bin.Rdata"), compress = "bzip2")

############################## Step 5: Data Cleaning
### HPS codebooks are in each zip file, I grabbed the one from the beginning of Phase 4 and then only kept the variables I needed
# most of the recoding is making sure missing values are consistent
# variables that start in "a" are imputation flags
# all missing codes in these variables are -99

load(here("data_raw", "data_raw_bin.Rdata"))

data_raw <- data_raw |> 
  clean_names()


data_rc <- data_raw |> 
  rename(id = scram,
         state_code = est_st,
         birth_year = tbirth_year,
         birth_year_imp = abirth_year,
         race = rrace,
         race_imp = arace,
         hispanic = rhispanic,
         hispanic_imp = ahispanic,
         genid_birth = egenid_birth,
         genid_birth_imp = agenid_birth,
         educ = eeduc,
         educ_imp = aeduc) |> 
  select(id, week, state_code, birth_year, race, hispanic, genid_birth,
         genid_describe, sexual_orientation, income, educ, anxious, worry,
         interest, down, everything()) |> 
  mutate(across(c(birth_year:down), ~case_match(.x,
                                        -99 ~ NA_integer_,
                                        -88 ~ NA_integer_,
                                        .default = .x)),
         race_rc = as.factor(case_when(hispanic == 1 & race == 1 ~ 1, #NHW
                                       hispanic == 1 & race == 2 ~ 2, #NHB
                                       hispanic == 1 & race == 3 ~ 3, #NHO
                                       hispanic == 1 & race == 4 ~ 3, #NHO
                                       hispanic == 2 ~ 4))) |> #HISP
  relocate(race_rc, .after = birth_year) |> 
  select(-c(race, hispanic)) |> 
  mutate(trans_gnc = case_when(genid_describe == 3 ~ 1,
                               genid_describe == 4 ~ 1,
                               genid_birth == 1 & genid_describe == 2 ~ 1,
                               genid_birth == 2 & genid_describe == 1 ~ 1,
                               is.na(genid_describe) ~ NA_integer_,
                               .default = 0),
         cis_lgbq = case_when(trans_gnc == 0 & 
                                sexual_orientation %in% c(1,3,4,5) ~ 1,
                              is.na(trans_gnc) ~ NA_integer_,
                              is.na(sexual_orientation) ~ NA_integer_,
                              .default = 0),
         queer = case_when(trans_gnc == 1 ~ 1,
                           cis_lgbq == 1 ~ 1,
                           is.na(trans_gnc) & 
                             sexual_orientation %in% c(1,3,4,5) ~ 1,
                           is.na(sexual_orientation) &
                             trans_gnc == 0 ~ NA_integer_,
                           is.na(sexual_orientation) &
                             is.na(trans_gnc) ~ NA_integer_,
                           .default = 0))

### doing some checking of recodes, think i have it
data_rc |> 
  group_by(sexual_orientation, genid_birth, genid_describe, trans_gnc, cis_lgbq) |> 
  summarize(n = n()) |> 
  print(n = Inf)

### adding in state abbreviations instead of just fips codes
hps_data <- tibble(tidycensus::fips_codes) |> 
  select(state, state_code) |> 
  unique() |> 
  right_join(data_rc)


hps_data <- hps_data |> select(id, state, week, queer, cis_lgbq, trans_gnc, 
                   anxious, worry, interest, down, everything())

#### congrats! you now have all of the Household Pulse Survey data

save(hps_data,
     file = here("data", "hps_data.Rdata"), 
     compress = "bzip2")


