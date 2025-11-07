# this is an R script for downloading and compiling data from the CDC's Household Pulse Survey (public use microdata)

############################ Step 0: Setup
library(tidyverse)
library(here)
library(rvest)
library(curl)
library(janitor)

if (dir.exists(here("zipfiles"))) {
  message(".zip files will be saved to ", here("zipfiles"))
} else {
  dir.create(here("zipfiles"))
  message(".zip files will be saved to ", here("zipfiles"))
}

if (dir.exists(here("csvfiles"))) {
  message(".csv files will be saved to ", here("csvfiles"))
} else {
  dir.create(here("csvfiles"))
  message(".csv files will be saved to ", here("csvfiles"))
}

if (dir.exists(here("codebooks"))) {
  message("codebook .xlsx files will be saved to ", here("codebooks"))
} else {
  dir.create(here("codebooks"))
  message("codeboook .xlsx files will be saved to ", here("codebooks"))
}

if (dir.exists(here("data_raw", "hps"))) {
  message("raw HPS data will be saved to ", here("data_raw", "hps"))
} else {
  dir.create(here("data_raw", "hps"))
  message("raw HPS data will be saved to ", here("data_raw", "hps"))
}

############################ Step 1: get a list of urls
# need weeks 34 and up

html24 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.2024.html")

html23 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.2023.html")

html22 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.2022.html")

html21 <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/data/datasets.2021.html")


urls <- map(list(html21, html22, html23, html24),
    ~ {
     urls <-  .x |> 
        html_elements("a") |> 
       html_attr("href") |> 
       str_subset("CSV\\.zip") |> 
       tibble(url = _) |> 
       mutate(url = if_else(str_detect(url, "^https:"), url,
                            paste0("https:", url)))
     return(urls)
    }) |> 
  bind_rows() |> 
  filter(str_detect(url, "wk|cycle")) |>
  mutate(wave = as.integer(str_extract(url, "(?<=hhp/[0-9]{4}/(wk|cycle))([a-zA-Z0-9]+)"))) |> 
  filter(!(wave %in% c(8, 9, 21:33))) # cycles 8 and 9 don't ask about gender identity; cycles 21-33 don't ask about sexual orientation OR gender identity


# gut check here: should have 37 urls, one for each zip file

######################### Step 2: download the zip files

multi_download(urls = urls$url, destfiles = here("zipfiles", basename(urls$url)))


################## Step 3: Unzip the zip files, grabbing only the csvs
csv <- here("zipfiles", dir(here("zipfiles"))) |> 
  set_names() |> 
  map(~unzip(.x, list = TRUE)) |> 
  tibble(data = _) |> 
  mutate(filepath = names(data)) |> 
  unnest(data) |> 
  select(name = Name, filepath) |> 
  filter(str_detect(name, ".csv"),
         !str_detect(name, "repwgt"))


map2(.x = csv$filepath, .y = csv$name,
     .f = ~unzip(.x, files = .y, exdir = here("csvfiles")))



######################## Step 4: read csvs and combine

# codebooks:
unzip(here("zipfiles", "HPS_Phase4-1Cycle04_PUF_CSV.zip"),
      files = "HPS_data.dictionary_Phase 4.1 Cycle04_CSV.xlsx",
      exdir = here("codebooks"))
unzip(here("zipfiles", "HPS_Week63_PUF_CSV.zip"), 
      files = "pulse2023_data.dictionary_CSV_63.xlsx",
      exdir = here("codebooks"))

#first, know the variable names you want to use

# id: scram
# wave: week, cycle
# survey weight: pweight
# state: est_st
# age: tbirth_year
# race: rrace, rhispanic
# gender id: genid_describe
# sexual orientation: sexual_orientation
# gender id at birth: egenid_birth
# outcomes: anxious, worry, interest, down
# education: eeduc

datalist <- map(.x = here("csvfiles", dir(here("csvfiles"))), 
    .f = ~read_csv(.x,
         col_select = any_of(c("SCRAM",
                        "WEEK",
                        "CYCLE",
                        "EST_ST",
                        "PWEIGHT",
                        "TBIRTH_YEAR",
                        "RRACE",
                        "RHISPANIC",
                        "EGENID_BIRTH",
                        "GENID_DESCRIBE",
                        "SEXUAL_ORIENTATION",
                        "EEDUC",
                        "ANXIOUS",
                        "WORRY",
                        "INTEREST",
                        "DOWN"))))

# harmonize time variable

datalist <- datalist |> 
  map(~{
    if("WEEK" %in% colnames(.x)) {
      df <- .x |> rename(wave = WEEK)
    }
    if ("CYCLE" %in% colnames(.x)) {
      df <- .x |> rename(wave = CYCLE)
    }
    return(df)
  })

# combining into one dataframe

data_raw <- bind_rows(datalist) |> 
  clean_names()

# save here
save(data_raw, file = here("data_raw", "hps", "hps_data_raw.Rdata"), compress = "bzip2")


############################## Step 4.5: Dates

dates21 <- html21 |> 
  html_elements(".aem-Grid--default--10") |> 
  html_children() |> 
  html_text2() |> 
  tibble(text = _) |> 
  mutate(text = str_remove_all(text, "\r|\n"), #remove whitespace markers
         text = str_squish(text)) |> 
  filter(str_detect(text, "^H")) |> 
  mutate(wave = str_extract(text, "(?<=Week )[0-9]+")) |> 
  fill(wave, .direction = "up") |> 
  filter(str_detect(text, "Household")) |> 
  mutate(text = str_remove(text, "^.*\\: ")) |> 
  separate(text, into = c("startdate", "enddate"), sep = "\\p{Pd}") |> 
  mutate(across(c(startdate, enddate), ~paste(.x, "2021")),
         across(c(startdate, enddate), ~mdy(.x))) |> 
  filter(wave > 33)

dates22 <- html22 |> 
  html_elements(".aem-Grid--default--10") |> 
  html_children() |> 
  html_text2() |> 
  tibble(text = _) |> 
  mutate(text = str_remove_all(text, "\r|\n"), #remove whitespace markers
         text = str_squish(text)) |> 
  filter(str_detect(text, "^H")) |> 
  mutate(wave = str_extract(text, "(?<=Week )[0-9]+")) |> 
  fill(wave, .direction = "up") |> 
  filter(str_detect(text, "Household")) |> 
  mutate(text = str_remove(text, "^.*\\: ")) |> 
  separate(text, into = c("startdate", "enddate"), sep = "\\p{Pd}") |> 
  mutate(across(everything(), ~str_squish(.x))) |> 
  mutate(across(c(startdate,enddate), ~if_else(str_detect(.x, "[0-9]{4}$"),
                                               .x,
                                               paste(.x, "2022"))),
         across(c(startdate,enddate), ~mdy(.x)))

dates23 <- html23 |> 
  html_elements(".aem-Grid--default--10") |> 
  html_children() |> 
  html_text2() |> 
  tibble(text = _) |> 
  mutate(text = str_remove_all(text, "\r|\n"), #remove whitespace markers
         text = str_squish(text)) |> 
  filter(str_detect(text, "^H")) |> 
  mutate(wave = str_extract(text, "(?<=Week )[0-9]+")) |> 
  fill(wave, .direction = "up") |> 
  filter(str_detect(text, "Household")) |> 
  mutate(text = str_remove(text, "^.*\\: ")) |>  
  separate(text, into = c("startdate", "enddate"), sep = "\\p{Pd}") |> 
  mutate(across(everything(), ~str_squish(.x))) |> 
  mutate(across(c(startdate,enddate), ~if_else(str_detect(.x, "December"),
                                               paste(.x, "2022"),
                                               paste(.x, "2023"))),
         across(c(startdate,enddate), ~mdy(.x)))

dates24 <- html24 |> 
  html_elements(".aem-Grid--default--10") |> 
  html_children() |> 
  html_text2() |> 
  tibble(text = _) |> 
  mutate(text = str_remove_all(text, "\r|\n"), #remove whitespace markers
         text = str_squish(text)) |> 
  filter(str_detect(text, "^(Phase|Household)")) |> 
  tail(-1) |> 
  mutate(wave = str_extract(text, "(?<=Cycle )[0-9]+")) |> 
  fill(wave, .direction = "down") |> 
  filter(str_detect(text, "Household")) |> 
  mutate(text = str_remove(text, "^.*\\: ")) |> 
  separate(text, into = c("startdate", "enddate"), sep = "\\p{Pd}") |> 
  mutate(across(everything(), ~str_squish(.x))) |> 
  mutate(startdate = paste(startdate, "2024")) |> 
  mutate(across(c(startdate,enddate), ~mdy(.x)))

dates <- bind_rows(dates21, dates22, dates23, dates24) |> 
  arrange(startdate) |> 
  mutate(wave = as.integer(wave))

############################## Step 5: Data Cleaning
# most of the recoding is making sure missing values are consistent

load(here("data_raw", "hps", "hps_data_raw.Rdata"))


data_rc <- data_raw |> 
  select(id = scram, 
         wave,
         state_code = est_st, 
         birth_year = tbirth_year, 
         race = rrace, 
         hispanic = rhispanic, 
         genid_birth = egenid_birth,
         genid_describe,
         sexual_orientation,
         educ = eeduc,
         anxious, worry, interest, down,
         pweight) |> 
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
         lgbq = case_when(sexual_orientation %in% c(1,3,4,5) ~ 1,
                              is.na(sexual_orientation) ~ NA_integer_,
                              .default = 0),
         queer = case_when(trans_gnc == 1 ~ 1,
                           trans_gnc == 0 & lgbq == 1 ~ 1,
                           trans_gnc == 0 & lgbq == 0 ~ 0,
                           trans_gnc == 0 & is.na(lgbq) ~ NA_integer_,
                           is.na(trans_gnc) & lgbq == 1 ~ 1,
                           is.na(trans_gnc) & lgbq == 0 ~ NA_integer_,
                           is.na(trans_gnc) & is.na(lgbq) ~ NA_integer_,
                           .default = 0))

### adding in state abbreviations instead of just fips codes
hps_data <- tibble(tidycensus::fips_codes) |> 
  select(state, state_code) |> 
  unique() |> 
  right_join(data_rc) |> 
#adding dates, creating "time" variable (true order of waves)
  left_join(dates) |> 
  mutate(time = case_match(wave,
                           34:63 ~ wave - 33,
                           1:9 ~ wave + 30))

#### congrats! you now have all of the Household Pulse Survey data

save(hps_data, file = here("data_clean", "hps_data.Rdata"))
