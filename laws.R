################ Setup

library(here)
library(tidyverse)
library(pdftools)

if (dir.exists(here("data_raw", "laws"))) {
  message("Laws PDFs will be saved in ", here("data_raw", "laws"))
} else {
  dir.create(here("data_raw", "laws"))
  message("Laws PDFs will be saved in ", here("data_raw", "laws"))
}



#################### Step 1: get the PDFs from MAP

pdfs <- tibble(url = c("https://www.lgbtmap.org/img/maps/citations-bathroom-facilities-bans.pdf", 
                       "https://www.lgbtmap.org/img/maps/citations-sports-participation-bans.pdf", 
                       "https://www.lgbtmap.org/img/maps/citations-youth-medical-care-bans.pdf")) |> 
  mutate(filename = basename(url))


map2(.x = pdfs$url, .y = pdfs$filename,
     ~curl::curl_download(url = .x,
                          destfile = here("data_raw", 
                                          "laws", .y),
                          quiet = FALSE))



################## Step 2: parse the pdfs into useful dataframes
# all of these are set up the same, with the chronology pages being the ones that we really want

pdf_parser <- function(file) {
  
  page1 <- NULL
  page2 <- NULL
  text <- pdf_text(here("data_raw", "laws", file))
  
  for (i in 2:length(text)) {
    if (str_detect(text[i], "Order of Law")) {
      page1 <- i
      break
    }
  }
  
  for (i in 2:length(text)) {
    if (str_detect(text[i], "Order of Governor Vetoes")) {
      page2 <- i
      break
    }
  }
  
  return(text[page1:page2])
  
}


text <- dir(here("data_raw", "laws")) |> 
  set_names() |> 
  map(pdf_parser)


laws_data <- map(text, ~read_delim(.x, delim = "\n") |> 
                   rename(data = 1) |> 
                   mutate(data = str_squish(data)) |> 
                   filter(!str_detect(data, "^20[[:digit:]]")) |> 
                   mutate(drop = if_else(str_detect(data, "Order of Governor Vetoes"), 1, 0),
                          drop = cumsum(drop)) |> 
                   filter(drop == 0) |> 
                   mutate(drop = if_else(str_detect(data, "^1."), 0, NA_integer_)) |> 
                   fill(drop, .direction = "down") |> 
                   drop_na() |> 
                   select(-drop) |> 
                   separate(data, into = c("state_name", "bill", "date"), 
                            sep = " \\p{Pd} ") |> 
                   mutate(state_name = str_remove_all(state_name, "^([[:digit:]]|[[:punct:]]|[[:space:]])+")) |> 
                   separate(date, into = c("date", "notes"), sep = "\\(") |> 
                   mutate(across(everything(), ~str_squish(.x)),
                          date = mdy(date)))


laws <- tibble(laws_data) |> 
  mutate(pdf_name = names(laws_data)) |> 
  unnest(laws_data) |> 
  mutate(type = case_when(
    str_detect(pdf_name, "bathroom") ~ "bathroom",
    str_detect(pdf_name, "sports") ~ "sports",
    str_detect(pdf_name, "medical") ~ "medical"
  )) |> 
  arrange(date) |> 
  drop_na(bill)

laws <- tibble(state_name = state.name, state = state.abb) |> 
  right_join(laws)



first_bathroom <- laws |> 
  filter(type == "bathroom") |> 
  group_by(state) |> 
  slice_min(date, with_ties = FALSE) |> 
  select(state,
         state_name,
         bathroom_bill = bill,
         bathroom_date = date)

first_sports <- laws |> 
  filter(type == "sports") |> 
  group_by(state) |> 
  slice_min(date, with_ties = FALSE) |> 
  select(state,
         state_name,
         sports_bill = bill,
         sports_date = date)

first_medical <- laws |> 
  filter(type == "medical") |> 
  group_by(state) |> 
  slice_min(date, with_ties = FALSE) |> 
  select(state,
         state_name,
         medical_bill = bill,
         medical_date = date)


first_overall <- laws |> 
  group_by(state) |> 
  slice_min(date, with_ties = FALSE) |> 
  select(state,
         state_name,
         first_bill = bill,
         first_date = date)


laws_data <- tibble(state = state.abb, state_name = state.name) |> 
  left_join(first_overall) |> 
  left_join(first_bathroom) |> 
  left_join(first_sports) |> 
  left_join(first_medical)

save(laws_data, file = here("data_clean", "first_laws.Rdata"))
save(laws, file = here("data_clean", "all_laws.Rdata"))

load(here("data_clean", "all_laws.Rdata"))
