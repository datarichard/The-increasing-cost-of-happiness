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


#### Satisfaction ####
satisfaction <- gather_hilda(hilda, c(
  "losat",   # Life satisfaction
  "hhwtrps") # sample weights for life satisfaction (RP)
  ) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  transmute(xwaveid, wave, losat, losat_wt = losat * hhwtrps)

#### Income ####
# CPI values from https://www.pc.gov.au/research/completed/rising-inequality
CPI = data.frame(
  wave = letters[1:18],
  deflator = c(1.52547554, # 2000/01
               1.48315720, # 2001/02
               1.43988458, # 2002/03
               1.40607389, # 2003/04
               1.37297463, # 2004/05
               1.33027251, # 2005/06
               1.29200230, # 2006/07
               1.24993042, # 2007/08
               1.21214575, # 2008/09
               1.18464785, # 2009/10
               1.14888718, # 2010/11
               1.12303076, # 2011/12
               1.09804401, # 2012/13
               1.06903118, # 2013/14
               1.05101802, # 2014/15
               1.03670360, # 2015/16
               1.01929187, # 2016/17
               1)) %>% mutate(inflator = rev(deflator))

thresholds <- read_csv("../../docs/Frequencies 180c/HILDA-thresholds-by-wave-180.csv") %>%
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
  "hhadult",
  "hhwtes")  # sample weights for hifdip (enumerated persons)
  ) %>%
  spread(code, val) %>%
  left_join(CPI, by = "wave") %>%
  left_join(thresholds, by = "wave") %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(
    hh_disp_inc = (hifdip - hifdin) * deflator, # convert to base year 2017/18
    hifdip_rl2018 = hifdip * deflator,
    hhchild = hh0_4 + hh5_9 + hh10_14,
    OECD_mod = 1 + ((hhadult-1)*(0.5)) + (hhchild*(0.3))
    ) %>%
  transmute(
    xwaveid, 
    wave,
    top_hifdip = hifdip > hifdip_thld,
    hifdip_adj = hifdip_rl2018/sqrt(hhpers), # for backward compatibility
    hh_disp_inc_eq = hh_disp_inc/OECD_mod, # convert to base and adjust by size
    hh_disp_inc_eq_wt = hh_disp_inc_eq * hhwtes)


#### Happiness ####
gh9_items <- gather_hilda(hilda, c(
  "hhwtscs", # sample weights for gh9 (SCQ)
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
  left_join(select(gh9_items, xwaveid, wave, hhwtscs), 
            by = c("xwaveid", "wave")
            ) %>%
  transmute(xwaveid, 
            wave, 
            gh9 = scales::rescale(gh9, to = c(1, 100)), 
            gh9_wt = gh9*hhwtscs)

#### Plot income, happiness and satisfaction
dat <- income %>%
  left_join(satisfaction, by = c("xwaveid", "wave")) %>%
  left_join(happiness, by = c("xwaveid", "wave")) %>%
  group_by(wave) %>%
  mutate(year = which(letters == wave[1]) + 2000) %>%
  ungroup() %>%
  select(year, everything(), -wave)

p <- dat %>%
  group_by(year) %>%
  summarise(
    `Median household income \n($000s)` = round(median(hh_disp_inc_eq/1000), 2),
    `Mean life satisfaction \n(0-10)` = round(mean(losat, na.rm=T), 2),
    `Mean happiness \n(1-100)` = round(mean(gh9, na.rm=T), 2),
  ) %>%
  gather("key", "value", -year) %>%
  mutate(
    key = paste0(key, "\n"),
    key = fct_relevel(key, "Median household income \n($000s)\n")) %>%
  ggplot(aes(x = year, y = value, color = key)) +
  geom_line(aes(group = key), size = 2, alpha = 0.5) +
  labs(title = "Income & wellbeing in Australia between 2001-2018",
       caption = "Source: HILDA, Melbourne University
                  Household equivalised income per person (modified OECD scale)",
       x = "  ", y = "") +
  # geom_vline(aes(xintercept = 2009), color = "red", linetype = 3) +
  facet_wrap(~key, scales = "free_y") +
  scale_y_continuous(expand = expansion(add = 1)) +
  theme(legend.position = "none")

p + 
  labs(subtitle = "  ") +
  ggthemes::scale_color_economist() +
  ggthemes::theme_economist(base_family="Verdana", base_size = 8) +
  theme(legend.position = "none",
        strip.text = element_text(size = 10)
  )

p +
  ggthemes::scale_colour_wsj("colors6", "") +
  ggthemes::theme_wsj() +
  theme(legend.position = "none",
        plot.title = element_text(size = 19),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 12))
