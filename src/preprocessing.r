#### The Increasing Cost of Happiness ####
# RW Morris, N Kettlewell, N Glozier
# 
# Preprocessing 
# Nov 21st 2020
# 
# Libraries
library(tidyverse)
library(haven)

#### Load data ####
path_to_hilda <- list.files(
  path = '~/Dropbox/HILDA/data',
  pattern = '^Combined.*.dta$',
  full.names = TRUE
)

hilda <- list()
for (pathtofile in path_to_hilda) {
  df <- read_dta(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}

# Helper function
source('../../src/gather_hilda.R')

# DEMOGRAPHIC VARIABLES 
gather_hilda(hilda, c(
  "esbrd",   # employment status (broad)
  "hhda10",  # SEIFA 2001 Decile of socio-economic advantage (higher is better)
  "hgage",   # age
  "hgsex",   # sex (male = 1)
  "helth",   # long term health condition (yes = 1)
  "hhpers",  # no. persons in household
  "hhrhid",  # household id (within wave)
  "mrcurr",  # current marital status (married/de facto ≤ 2)
  "edhigh1", # highest education achieved
  "edfts"    # current fulltime student (yes = 1)
)) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) -> demographic_items

# MODEL VARIABLES (household wealth, satisfaction, happiness):
# Note that regular HILDA income variables were used (not total income, which 
# include irregular income) to match with the ABS, and because more consistent
# prior to 2012. See Appendix in Wilkins (2014) 'Derived income variables in the
# HILDA survey' for explanation
# 
gather_hilda(hilda, c(
  "hifdip", # household disposable regular income
  'losat', # Life satisfaction
  'gh9a', # Vitality: feel full of life (lower is better)*
  'gh9b', # Mental H: Been a nervous person (higher is better)
  'gh9c', # Mental H: Felt so down in the dumps (higher is better)
  'gh9d', # Mental H: Felt calm and peaceful (lower is better)*
  'gh9e', # Vitality: Have a lot of energy (lower is better)*
  'gh9f', # Mental H: Felt down (higher is better)
  'gh9g', # Vitality: Felt worn out (higher is better)
  'gh9h', # Mental H: Been happy (lower is better)*
  'gh9i'  # Vitality: Felt tired (higher is better)
)) %>%
  # Recode missing to NA
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) -> model_items

reversed_items <- c('gh9a', 'gh9d', 'gh9e', 'gh9h')

model_items %>%
  select(xwaveid, wave, starts_with("gh9")) %>%
  # Reverse score
  mutate_at(reversed_items, list(~ 7 - .)) %>%
  gather(code, val, -xwaveid, -wave) %>%
  # Impute average if less than half missing (see ghmh data dictionary)
  group_by(xwaveid, wave) %>%
  mutate(
    sum_na = sum(is.na(val)),
    mean_na = mean(val, na.rm = TRUE),
    imputed = ifelse(is.na(val) & sum_na < 5, mean_na, val),
    sum_imputed = sum(imputed),
    code = "gh9_sum"
  ) %>% 
  select(xwaveid, wave, gh9 = sum_imputed) %>%
  distinct() %>%
  ungroup() -> gh9_imputed

# Join all the data sources
model_items %>%
  select(xwaveid, wave, hifdip, losat) %>%
  left_join(gh9_imputed, by = c("xwaveid", "wave")) %>%
  left_join(demographic_items, by = c("xwaveid", "wave")) %>%
  group_by(wave) %>%
  mutate(year = which(letters == wave[1]) + 2000) %>%
  ungroup() %>%
  select(xwaveid, year, everything(), -wave) -> happywealth

thresholds <- read_csv("../../docs/Frequencies 180c/HILDA-thresholds-by-wave-180.csv") %>%
  filter(variable %in% c("hifdip")) %>%
  gather("wave", "threshold", -variable, -label) %>%
  separate(wave, into = c("wave", "year"), sep = "e") %>%
  mutate(
    variable = paste0(variable, "_thld"),
    year = as.numeric(year) + 2000) %>%
  select(-label, -wave) %>%
  spread(variable, threshold)

happywealth <- happywealth %>%
  left_join(thresholds, by = "year")

# write_rds(happywealth, "../results/happywealth.rds")

#### Covariates ####
#
# CPI values from https://www.pc.gov.au/research/completed/rising-inequality
CPI = data.frame(
  year = 2002:2018,
  deflator = c(1.48590604,
               1.445169713,
               1.408396947,
               1.373449132,
               1.340193705,
               1.2887078,
               1.262257697,
               1.208515284,
               1.191603875,
               1.155532359,
               1.115927419,
               1.102589641,
               1.076848249,
               1.045325779,
               1.029767442,
               1.019337017,
               1)) %>% mutate(inflator = rev(deflator))

# Dollar values converted to base year 2002:
# https://www.rba.gov.au/calculator/annualDecimal.html
happywealth %>%
  filter(year %in% c(2002:2018)) %>%
  left_join(CPI, by = "year") %>% 
  transmute(
    year,
    xwaveid,
    losat,
    gh9,
    hhsize = as.numeric(hhpers),
    adj_hifdip = (hifdip * deflator) / sqrt(hhsize),
    top_hifdip = hifdip > hifdip_thld,
    male = hgsex == 1,
    age = as.numeric(hgage),
    decade = floor(age/10),
    student = if_else(edfts == 1 | age <= 17, TRUE, FALSE, missing = FALSE),
    edu = case_when( 
      edhigh1 == 10 ~ NA_character_,           # recode undetermined 
      edhigh1 == 1 ~  "Postgrad",              # PhD
      edhigh1 == 2 ~  "Grad",                  # Grad diploma
      edhigh1 == 3 ~  "Grad",                  # Bachelors
      edhigh1 == 4 ~  "Trade",                 # Diploma
      edhigh1 == 5 ~  "Trade",                 # Certificate
      edhigh1 == 8 ~  "Highschool",            # Year 12
      edhigh1 == 9 ~  "None",        # Year 11 or less
      age <= 17 ~  "In school",
      edhigh1 < 0 ~   NA_character_            # recode missing values
    ),
    edu = ordered(edu, levels = c("None", 
                                  "In school", 
                                  "Highschool", 
                                  "Trade", 
                                  "Grad", 
                                  "Postgrad")),
    workforce = if_else(esbrd %in% 1:2, TRUE, FALSE, missing = FALSE),
    unemployed = if_else(esbrd == 2, TRUE, FALSE, missing = FALSE),
    chronic = if_else(helth == 1, TRUE, FALSE, missing = FALSE),
    single = if_else(mrcurr == 6, TRUE, FALSE, missing = FALSE),
    coupled = if_else(mrcurr <= 2, TRUE, FALSE, missing = FALSE),
    SEIFA = hhda10
  ) -> happywealth_covs

write_rds(happywealth_covs, "../data/happywealth_covs.rds")