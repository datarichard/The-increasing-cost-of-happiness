#### The Increasing Cost of Happiness ####
# RW Morris, N Kettlewell, N Glozier
# 
# Preprocessing 
# Jan 12th 2021
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
source('~/Dropbox/HILDA/src/gather_hilda.R')

#### DEMOGRAPHICS ####
# Demographic variables were selected to match the sample design of HILDA, so 
# age, sex, employment status, marital status, and region (State) were included.

state_key = c(`1` = "NSW", `2` = "VIC", `3` = "QLD", `4` = "SA", `5` = "WA", 
              `6` = "TAS", `7` = "NT", `8` = "ACT")

region_key = c(`0` = "capital", `1` = "urban", `2` = "regional", `3` = "rural")

gather_hilda(hilda, c(
  "esbrd",   # employment status (broad)
  "hhda10",  # SEIFA 2001 Decile of socio-economic advantage (higher is better)
  "hgage",   # age
  "hgsex",   # sex (male = 1)
  "helth",   # long term health condition (yes = 1)
  "hhpers",  # no. persons in household
  # "hhrhid",  # household id (within wave)
  "mrcurr",  # current marital status (married/de facto ≤ 2)
  "edhigh1", # highest education achieved
  "edfts",   # current fulltime student (yes = 1)
  "hhstate", # household State of residence (NSW, VIC, QLD, SA, WA, TAS, NT, ACT)
  "hhssos",  # household section of State (major urban, other urban, boundary, rural)
  "hhwte"    # enumarated persons cross-sectional weight (sex, broad age, 
             # region, employment, marital status)
  )) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(hhssos = replace_na(hhssos, 3),
         hhssos = recode(hhssos, !!!region_key),
         hhstate = recode(hhstate, !!!state_key)
         ) -> demographic_items

demographics <- demographic_items %>%
  transmute(
    wave,
    xwaveid,
    hhsize = as.numeric(hhpers),
    male = hgsex == 1,
    age = as.numeric(hgage),
    student = if_else(edfts == 1 | age <= 17, TRUE, FALSE, missing = FALSE),
    edu = case_when( 
      edhigh1 == 10 ~ NA_character_,           # recode undetermined 
      edhigh1 == 1 ~  "Grad",                  # PhD
      edhigh1 == 2 ~  "Grad",                  # Grad diploma
      edhigh1 == 3 ~  "Grad",                  # Bachelors
      edhigh1 == 4 ~  "Highschool",            # Diploma
      edhigh1 == 5 ~  "Highschool",            # Certificate
      edhigh1 == 8 ~  "Highschool",            # Year 12
      edhigh1 == 9 ~  "None",                  # Year 11 or less
      age <= 17 ~  "In school",
      edhigh1 < 0 ~   NA_character_            # recode missing values
      ),
    edu = ordered(edu, levels = c("None", 
                                  "In school", 
                                  "Highschool", 
                                  "Grad")),
    workforce = if_else(esbrd %in% 1:2, TRUE, FALSE, missing = FALSE),
    unemployed = if_else(esbrd == 2, TRUE, FALSE, missing = FALSE),
    chronic = if_else(helth == 1, TRUE, FALSE, missing = FALSE),
    single = if_else(mrcurr == 6, TRUE, FALSE, missing = FALSE),
    coupled = if_else(mrcurr <= 2, TRUE, FALSE, missing = FALSE),
    SEIFA = as.integer(extract_numeric(hhda10)),
    region = paste(hhstate, hhssos, sep = "_"),
    weights = hhwte
  )

#### SATISFACTION ####
satisfaction <- gather_hilda(hilda, c(
  "losat"   # Life satisfaction
  # "hhwtrps") # sample weights for life satisfaction (RP)
  )) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) 

#### HAPPINESS ####
gh9_items <- gather_hilda(hilda, c(
  # "hhwtscs", # sample weights for gh9 (SCQ)
  'gh9a',    # Vitality: feel full of life (lower is better)*
  'gh9b',    # Mental H: Been a nervous person (higher is better)
  'gh9c',    # Mental H: Felt so down in the dumps (higher is better)
  'gh9d',    # Mental H: Felt calm and peaceful (lower is better)*
  'gh9e',    # Vitality: Have a lot of energy (lower is better)*
  'gh9f',    # Mental H: Felt down (higher is better)
  'gh9g',    # Vitality: Felt worn out (higher is better)
  'gh9h',    # Mental H: Been happy (lower is better)*
  'gh9i')    # Vitality: Felt tired (higher is better)
  ) %>%
  # Recode missing to NA
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) 

reversed_items <- c('gh9a', 'gh9d', 'gh9e', 'gh9h')

happiness <- gh9_items %>%
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
  ungroup() %>%
  select(xwaveid, wave, gh9 = sum_imputed) %>%
  distinct() %>%
  left_join(select(gh9_items, xwaveid, wave), 
            by = c("xwaveid", "wave")
            ) %>%
  transmute(xwaveid, 
            wave, 
            gh9 = scales::rescale(gh9, to = c(1, 100))) 

# Split gh9 into positive and negative components
positive_items <- c("gh9a", "gh9d", "gh9e", "gh9h")
negative_items <- c("gh9b", "gh9c", "gh9f", "gh9g", "gh9i")

positive_affect <- gh9_items %>%
  select(xwaveid, wave, all_of(positive_items)) %>%
  # Reverse score
  mutate_at(vars(any_of(reversed_items)), list(~ 7 - .)) %>%
  gather(code, val, -xwaveid, -wave) %>%
  # Impute average if less than half missing (see ghmh data dictionary)
  group_by(xwaveid, wave) %>%
  mutate(
    sum_na = sum(is.na(val)),
    mean_na = mean(val, na.rm = TRUE),
    imputed = ifelse(is.na(val) & sum_na < 2, mean_na, val),
    sum_imputed = sum(imputed),
    code = "gh9_sum"
  ) %>% 
  ungroup() %>%
  select(xwaveid, wave, gh9 = sum_imputed) %>%
  distinct() %>%
  left_join(select(gh9_items, xwaveid, wave), 
            by = c("xwaveid", "wave")
  ) %>%
  transmute(xwaveid, 
            wave, 
            gh9_positive = scales::rescale(gh9, to = c(1, 100))) 

negative_affect <- gh9_items %>%
  select(xwaveid, wave, all_of(negative_items)) %>%
  # Reverse score
  mutate_at(vars(any_of(reversed_items)), list(~ 7 - .)) %>%
  gather(code, val, -xwaveid, -wave) %>%
  # Impute average if less than half missing (see ghmh data dictionary)
  group_by(xwaveid, wave) %>%
  mutate(
    sum_na = sum(is.na(val)),
    mean_na = mean(val, na.rm = TRUE),
    imputed = ifelse(is.na(val) & sum_na < 3, mean_na, val),
    sum_imputed = sum(imputed),
    code = "gh9_sum"
  ) %>% 
  ungroup() %>%
  select(xwaveid, wave, gh9 = sum_imputed) %>%
  distinct() %>%
  left_join(select(gh9_items, xwaveid, wave), 
            by = c("xwaveid", "wave")
  ) %>%
  transmute(xwaveid, 
            wave, 
            gh9_negative = scales::rescale(gh9, to = c(1, 100))) 

# Join all gh9, positive and negative components into a single df
happiness <- left_join(happiness, positive_affect) %>%
  left_join(negative_affect)

#### HOUSEHOLD INCOME ####
# Note that regular HILDA income variables were used (not total income, which 
# include irregular income) to match with the ABS, and because more consistent
# prior to 2012. See Appendix in Wilkins (2014) 'Derived income variables in the
# HILDA survey' for explanation
# 
# CPI values from https://www.rba.gov.au/calculator/annualDecimal.html
CPI = data.frame(
  wave = letters[1:19],
  deflator = c(1.542713570,
               1.498048150,
               1.458201390,
               1.424814360,
               1.387466100,
               1.339831250,
               1.309354560,
               1.254768390,
               1.232931730,
               1.197970860,
               1.159657520,
               1.139569410,
               1.112318840,
               1.085316990,
               1.069189690,
               1.055708390,
               1.035529570,
               1.016107680,
               1.000000000)) #%>% mutate(inflator = rev(deflator))

thresholds <- read_csv("~/Dropbox/HILDA/data/HILDA-thresholds-by-wave-190.csv") %>%
  filter(variable %in% c("hifdip")) %>%
  gather("wave", "threshold", -variable, -label) %>%
  separate(wave, into = c("temp", "wave"), sep = "e") %>%
  mutate(
    variable = paste0(variable, "_thld"),
    wave = letters[as.numeric(wave)]) %>%
  select(-label, -temp) %>%
  spread(variable, threshold)

income <- gather_hilda(hilda, c(
  "hifdip",  # household disposable regular income (positive values)
  "hifdin",  # household disposable regular income (negative values)
  "hhpers",
  "hh0_4",
  "hh5_9",
  "hh10_14",
  "hhadult")  
  ) %>%
  spread(code, val) %>%
  left_join(CPI, by = "wave") %>%
  left_join(thresholds, by = "wave") %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(
    hh_disp_inc = (hifdip - hifdin) * deflator, # convert to base year 2019
    hifdip_rl2019 = hifdip * deflator,
    hhchild = hh0_4 + hh5_9 + hh10_14,
    OECD_mod = 1 + ((hhadult-1)*(0.5)) + (hhchild*(0.3))
    ) %>%
  transmute(
    xwaveid, 
    wave,
    top_hifdip = hifdip > hifdip_thld,
    hifdip_adj = hifdip_rl2019/sqrt(hhpers), # for backward compatibility
    hh_disp_inc_eq = hh_disp_inc/OECD_mod # convert to base and adjust by size
    ) 

#### FINAL JOIN (household wealth, satisfaction, happiness) ####


# Join all the data sources
income %>%
  left_join(happiness, by = c("xwaveid", "wave")) %>%
  left_join(satisfaction, by = c("xwaveid", "wave")) %>%
  left_join(demographics, by = c("xwaveid", "wave")) %>%
  group_by(wave) %>%
  mutate(year = which(letters == wave[1]) + 2000) %>%
  ungroup() %>%
  select(xwaveid, year, everything(), -wave) -> hilda_data

write_rds(hilda_data, "../data/hilda_data.rds")
