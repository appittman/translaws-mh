# this is an R script for scraping and compiling data from the CDC's Household Pulse Survey (public use microdata)
# if you're reading this and you work for the CDC i'm sorry if this is a rude scraping algorithm...I'm a novice lol

############################ Step 0: Setup
library(tidyverse)
library(here)
library(rvest)
library(curl)

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
# got these files by navigating on the page above, clicking the tab that corresponds with the year I want, then saving a copy of the html to my computer

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


read_csv()
