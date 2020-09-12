#### Preprocessing ####

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

# demographic data 
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

# financial data
gather_hilda(hilda, c(
  "wsfei",   # financial year gross
  "tifdip",  # Disposable regular income
  "tifeftp", # Gross total income (including benefits, pensions, foreign, etc)
  "hiwsfei", # household financial year gross
  "hwnwip",  # household net worth
  "hifdip",  # household disposable regular income
  "hifditp"  # household disposable total income
)) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) -> wealth_items

# Affective wellbeing:
gather_hilda(hilda, c(
  'losat',
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
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) -> happiness_items

reversed_items <- c('gh9a', 'gh9d', 'gh9e', 'gh9h')

happiness_items %>%
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
happiness_items %>%
  select(xwaveid, wave, losat) %>%
  left_join(gh9_imputed, by = c("xwaveid", "wave")) %>%
  left_join(
    select(wealth_items, xwaveid, wave, tifdip, tifeftp, hifdip, hwnwip),
    by = c("xwaveid", "wave")) %>%
  left_join(demographic_items, by = c("xwaveid", "wave")) %>%
  group_by(wave) %>%
  mutate(year = which(letters == wave[1]) + 2000) %>%
  ungroup() %>%
  select(xwaveid, year, everything(), -wave) -> happywealth

thresholds <- read_csv("../../docs/Frequencies 180c/HILDA-thresholds-by-wave-180.csv") %>%
  filter(variable %in% c("hwnwip", "hifdip", "tifdip")) %>%
  gather("wave", "threshold", -variable, -label) %>%
  separate(wave, into = c("wave", "year"), sep = "e") %>%
  mutate(
    variable = paste0(variable, "_thld"),
    year = as.numeric(year) + 2000) %>%
  select(-label, -wave) %>%
  spread(variable, threshold)

happywealth <- happywealth %>%
  left_join(thresholds, by = "year")

write_rds(happywealth, "../results/happywealth.rds")

#### Covariates ####
# Dollar values converted to base year 2002:
# https://www.rba.gov.au/calculator/annualDecimal.html
happywealth %>%
  filter(year %in% c(2002, 2006, 2010, 2014, 2018)) %>%
  mutate_at(vars("tifdip", "hifdip", "hwnwip"),
            ~ case_when(
              year == 2018 ~ .x*1.45,
              year == 2014 ~ .x*1.38,
              year == 2010 ~ .x*1.25,
              year == 2006 ~ .x*1.12,
              TRUE ~ .x)
  ) %>%
  transmute(
    year,
    xwaveid,
    losat,
    gh9,
    tifdip,
    hhsize = as.numeric(hhpers),
    adj_hwnwip = hwnwip / sqrt(hhsize),
    adj_hifdip = hifdip / sqrt(hhsize),
    top_hwnwip = hwnwip > hwnwip_thld,
    top_hifdip = hifdip > hifdip_thld,
    top_tifdip = tifdip > tifdip_thld,
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
    edu = ordered(edu, levels = c("None", "In school", "Highschool", "Trade", "Grad", "Postgrad")),
    edu_years = case_when( 
      edhigh1 == 10 ~ 0,        # recode undetermined 
      edhigh1 == 1 ~  7,        # PhD
      edhigh1 == 2 ~  4,        # Grad diploma
      edhigh1 == 3 ~  3,        # Bachelors
      edhigh1 == 4 ~  1,        # Diploma
      edhigh1 == 5 ~  0,        # Certificate
      edhigh1 == 8 ~  0,        # Year 12
      edhigh1 == 9 ~ -1,        # Year 11 or less
      age <= 17 ~ -1,
      edhigh1 < 0 ~ NA_real_,   # recode missing values as NA
    ),
    workforce = if_else(esbrd %in% 1:2, TRUE, FALSE, missing = FALSE),
    unemployed = if_else(esbrd == 2, TRUE, FALSE, missing = FALSE),
    chronic = if_else(helth == 1, TRUE, FALSE, missing = FALSE),
    single = if_else(mrcurr == 6, TRUE, FALSE, missing = FALSE),
    coupled = if_else(mrcurr <= 2, TRUE, FALSE, missing = FALSE),
    SEIFA = hhda10
  ) -> happywealth_covs

write_rds(happywealth_covs, "../data/happywealth_covs.rds")