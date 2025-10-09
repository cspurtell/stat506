##### Problem 1. #####

library(haven)
library(dplyr)

### a. ###
aux <- read_xpt("data/AUX_I.xpt")
demo <- read_xpt("data/DEMO_I.xpt")

intersect(names(aux), names(demo))

merged <- inner_join(aux, demo, by = "SEQN")
dim(merged)
# [1] 4582  119

### b. ###
merged <- merged %>%
  mutate(RIAGENDR = case_when(
    RIAGENDR == 1 ~ "Male",
    RIAGENDR == 2 ~ "Female",
    TRUE ~ NA_character_
  )) %>%
  mutate(RIAGENDR = factor(RIAGENDR))

merged <- merged %>%
  mutate(DMDCITZN = case_when(
    DMDCITZN == 1 ~ "Citizen by birth or naturalization",
    DMDCITZN == 2 ~ "Not a citizen",
    DMDCITZN == 7 ~ "Refused",
    DMDCITZN == 9 ~ "Don't Know",
    TRUE ~ NA_character_
  )) %>%
  mutate(DMDCITZN = factor(DMDCITZN))

merged <- merged %>%
  mutate(DMDHHSZA = case_when(
    DMDHHSZA == 0 ~ "0",
    DMDHHSZA == 1 ~ "1",
    DMDHHSZA == 2 ~ "2",
    DMDHHSZA == 3 ~ "3 or more",
    TRUE ~ NA_character_
  )) %>%
  mutate(DMDHHSZA = factor(DMDHHSZA))

merged <- merged %>% 
  mutate(INDHHIN2 = case_when(
    INDHHIN2 == 1  ~ "$0–$4,999",
    INDHHIN2 == 2  ~ "$5,000–$9,999",
    INDHHIN2 == 3  ~ "$10,000–$14,999",
    INDHHIN2 == 4  ~ "$15,000–$19,999",
    INDHHIN2 == 5  ~ "$20,000–$24,999",
    INDHHIN2 == 6  ~ "$25,000–$34,999",
    INDHHIN2 == 7  ~ "$35,000–$44,999",
    INDHHIN2 == 8  ~ "$45,000–$54,999",
    INDHHIN2 == 9  ~ "$55,000–$64,999",
    INDHHIN2 == 10 ~ "$65,000–$74,999",
    INDHHIN2 == 14 ~ "$75,000–$99,999",
    INDHHIN2 == 15 ~ "≥$100,000",
    INDHHIN2 == 12 ~ "$20,000 and over",
    INDHHIN2 == 13 ~ "Under $20,000",
    INDHHIN2 == 77 ~ "Refused",
    INDHHIN2 == 99 ~ "Don't know",
    TRUE ~ NA_character_
  )) %>% 
  mutate(
    INDHHIN2 = factor(
      INDHHIN2,
      levels = c(
        "$0–$4,999", "$5,000–$9,999", "$10,000–$14,999", "$15,000–$19,999",
        "$20,000–$24,999", "$25,000–$34,999", "$35,000–$44,999",
        "$45,000–$54,999", "$55,000–$64,999", "$65,000–$74,999",
        "$75,000–$99,999", "≥$100,000",
        "$20,000 and over", "Under $20,000",
        "Refused", "Don't know"
      )
    )
  )

### c. ###
library(broom)
library(pscl)
library(knitr)

model_1R <- glm(AUXTWIDR ~ RIAGENDR, data = merged, family = poisson)
model_2R <- glm(AUXTWIDR ~ RIAGENDR + DMDCITZN + DMDHHSZA + INDHHIN2, 
                data = merged, family = poisson)

model_1L <- glm(AUXTWIDL ~ RIAGENDR, data = merged, family = poisson)
model_2L <- glm(AUXTWIDL ~ RIAGENDR + DMDCITZN + DMDHHSZA + INDHHIN2, 
                data = merged, family = poisson)

summarize_poisson <- function(model) {
  irr <- exp(coef(model))  # incidence rate ratios
  ci <- exp(confint(model))
  
  tidy_df <- broom::tidy(model) %>%
    mutate(
      IRR = exp(estimate),
      CI_low = exp(estimate - 1.96 * std.error),
      CI_high = exp(estimate + 1.96 * std.error)
    ) %>%
    select(term, IRR, CI_low, CI_high, p.value)
  
  stats <- data.frame(
    N = nobs(model),
    Pseudo_R2 = round(pscl::pR2(model)["McFadden"], 3),
    AIC = AIC(model)
  )
  
  list(coefs = tidy_df, stats = stats)
}

m1R <- summarize_poisson(model_1R)
m2R <- summarize_poisson(model_2R)
m1L <- summarize_poisson(model_1L)
m2L <- summarize_poisson(model_2L)

coefs_table <- bind_rows(
  m1R$coefs %>% mutate(Model = "1R: Right ear (Gender only)"),
  m2R$coefs %>% mutate(Model = "2R: Right ear (Full model)"),
  m1L$coefs %>% mutate(Model = "1L: Left ear (Gender only)"),
  m2L$coefs %>% mutate(Model = "2L: Left ear (Full model)")
) %>%
  select(Model, term, IRR, CI_low, CI_high, p.value) %>%
  mutate(across(c(IRR, CI_low, CI_high, p.value), ~round(., 3)))

stats_table <- bind_rows(
  m1R$stats %>% mutate(Model = "1R: Right ear (Gender only)"),
  m2R$stats %>% mutate(Model = "2R: Right ear (Full model)"),
  m1L$stats %>% mutate(Model = "1L: Left ear (Gender only)"),
  m2L$stats %>% mutate(Model = "2L: Left ear (Full model)")
) %>%
  select(Model, N, Pseudo_R2, AIC)

kable(coefs_table, caption = "Incidence Rate Ratios for Poisson Models")

kable(stats_table, caption = "Model Sample Sizes, Pseudo-R², and AIC Values")

### d. ###