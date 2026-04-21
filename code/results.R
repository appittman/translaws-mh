library(did)
library(tidyverse)
library(here)

load(here("did_results", "results_0413.Rdata"))


################ Calculate treatment effects

get_te <- function(did_result) {
  aggte(did_result,
        type = "dynamic",
        min_e = -10,
        max_e = 10)
}

te_queer <- did_queer |> map(get_te)
te_queer_nsg <- did_queer_nsg |> map(get_te)
te_queer_cov <- did_queer_cov |> map(get_te)

te_trans <- did_trans |> map(get_te)
te_trans_nsg <- did_trans_nsg |> map(get_te)
te_trans_cov <- did_trans_cov |> map(get_te)

############ reorganizing for displaying results

first_models <- list(trans = te_trans$first,
                     trans_cov = te_trans_cov$first,
                     trans_nsg = te_trans_nsg$first,
                     queer = te_queer$first,
                     queer_cov = te_queer_cov$first,
                     queer_nsg = te_queer_nsg$first)

bathroom_models <- list(trans = te_trans$bathroom,
                     trans_cov = te_trans_cov$bathroom,
                     trans_nsg = te_trans_nsg$bathroom,
                     queer = te_queer$bathroom,
                     queer_cov = te_queer_cov$bathroom,
                     queer_nsg = te_queer_nsg$bathroom)

medical_models <- list(trans = te_trans$medical,
                     trans_cov = te_trans_cov$medical,
                     trans_nsg = te_trans_nsg$medical,
                     queer = te_queer$medical,
                     queer_cov = te_queer_cov$medical,
                     queer_nsg = te_queer_nsg$medical)

sports_models <- list(trans = te_trans$sports,
                      trans_cov = te_trans_cov$sports,
                      trans_nsg = te_trans_nsg$sports,
                      queer = te_queer$sports,
                      queer_cov = te_queer_cov$sports,
                      queer_nsg = te_queer_nsg$sports)

############## Get coefficients from aggte objects

get_coeffs <- function(model_list) {

dynamic <- tibble(
  t = model_list |> 
    map(pluck("egt")),
  att = model_list |> 
    map(pluck("att.egt")),
  se = model_list |> 
    map(pluck("se.egt"))) |> 
  mutate(modelspec = names(t)) |> 
  unnest_longer(t:se)

overall <- tibble(
  overall_att = model_list |> 
    map(pluck("overall.att")),
  overall_se = model_list |> 
    map(pluck("overall.se")),
  modelspec = names(model_list)) |> 
  unnest_longer(overall_att:overall_se)

left_join(dynamic, overall, by = "modelspec")

}

coeffs <- map(list(medical = medical_models,
         bathroom = bathroom_models,
         sports = sports_models,
         first = first_models),
    get_coeffs)


################ Relative size of agg. post ATT vs. avg. pre ATTs

coeffs$sports |> 
  filter(t < -1) |> 
  group_by(modelspec) |> 
  summarize(overall_att = mean(overall_att),
            avg_pre = mean(abs(att))) |> 
  mutate(ratio = overall_att / avg_pre)

coeffs$first |> 
  filter(t < -1) |> 
  group_by(modelspec) |> 
  summarize(overall_att = mean(overall_att),
            avg_pre = mean(abs(att))) |> 
  mutate(ratio = overall_att / avg_pre)


save(coeffs, file = here("did_results", "coeffs.Rdata"))
