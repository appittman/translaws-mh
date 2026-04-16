library(did)
library(tidyverse)
library(here)

load(here("did_results", "results_0413.Rdata"))

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

######## reorganizing for displaying results

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

get_coeffs <- function(model_list) {
  atts <- model_list |> 
    map(pluck("overall.att"))
  ses <- model_list |> 
    map(pluck("overall.se"))
  
  tibble(model = names(model_list),
         overall_att = unlist(atts),
         overall_se = unlist(ses))
}


coeffs <- map(list(medical = medical_models,
         bathroom = bathroom_models,
         sports = sports_models,
         first = first_models),
    get_coeffs)
  
df_coeffs <- tibble(coeffs) |> 
  unnest_wider(coeffs) |> 
  mutate(law_type = names(coeffs)) |> 
  unnest_longer(c(model:overall_se)) |> 
  select(-ends_with("id"))


lawnames <- c("first" = "First Law Overall",
              "medical" = "First Medical Law",
              "bathroom" = "First Bathroom Law",
              "sports" = "First Sports Law")

ggplot(data = df_coeffs,
       aes(x = overall_att,
           y = case_match(model,
                      "trans_nsg" ~ "Trans Comparison Group,\nNo Small Groups",
                      "trans_cov" ~ "Trans Comparison Group,\nWith Covariates",
                      "trans" ~ "Trans Comparison Group",
                      "queer_nsg" ~ "Queer Comparison Group,\nNo Small Groups",
                      "queer_cov" ~ "Queer Comparison Group,\nWith Covariates",
                      "queer" ~ "Queer Comparison Group"))) +
  geom_point() +
  geom_segment(aes(x = overall_att + 1.96*overall_se,
                   xend = overall_att - 1.96*overall_se)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(x = "Overall ATT", y = "Model Specification",
       title = "Overall ATTs and 95% CIs for Six Model Specifications of the Effect of \nPassage of a State's First Anti-Trans Law (Overall and by Type)\non the PHQ-4 Score of Trans/GNC Residents of that State") +
  theme_minimal() +
  theme(panel.background = element_rect(color = "black")) +
  facet_wrap(law_type ~ ., labeller = as_labeller(lawnames), nrow = 1)
