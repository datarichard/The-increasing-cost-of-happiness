#### Comparing models ####
# https://bookdown.org/ajkurz/Statistical_Rethinking_recoded/overfitting-
# regularization-and-information-criteria.html#the-problem-with-parameters
# 
# WAIC is a (penalized) estimate of out-of-sample deviance so smaller values (closer to 
# zero) are better. Note also that p_waic is the penalty term of WAIC, and so it
# will be close to the number of dimensions in the posterior of each model  

lm_fit <- readRDS("../results/fit_lm_hifdip_losat_cov_2018.RDS")
pw_fit <- readRDS("../results/fit_cp_hifdip_losat_cov_2018.RDS")

lm_fit <- add_criterion(lm_fit, "waic")
pw_fit <- add_criterion(pw_fit, "waic")

# Comparing WAIC using loo_compare()
# The best model will be reported at the top of the list with elpd_diff = 0
ans <- loo_compare(lm_fit, pw_fit, criterion = "waic")

print(ans, simplify = F)
#         elpd_diff se_diff
# pw_fit   0.0       0.0  
# lm_fit -69.6      12.2  

waic_results <- tibble()

for (i in c(2002:2018)) {
  
  lm_fit <- readRDS(paste0("../results/fit_lm_hifdip_losat_cov_", i, ".RDS"))
  pw_fit <- readRDS(paste0("../results/fit_cp_hifdip_losat_cov_", i, ".RDS"))
  
  lm_fit <- add_criterion(lm_fit, "waic")
  pw_fit <- add_criterion(pw_fit, "waic")

  saveRDS(lm_fit, paste0("../results/fit_lm_hifdip_losat_cov_", i, ".RDS"))
  saveRDS(pw_fit, paste0("../results/fit_cp_hifdip_losat_cov_", i, ".RDS"))
  
  ans <- loo_compare(lm_fit, pw_fit, criterion = "waic")
  
  as_tibble(ans, rownames = "model") %>%
    mutate(year = i) %>%
    mutate(across(elpd_diff:year, as.numeric)) -> tb
  
  # Add R-squared
  lm_r2 <- bayes_R2(lm_fit) %>%
    as_tibble() %>%
    rename(R2 = Estimate, R2.Error = Est.Error, R2_Q2.5 = Q2.5, R2_Q97.5 = Q97.5) %>%
    mutate(year = i, model = "lm_fit")
  
  pw_r2 <- bayes_R2(pw_fit) %>%
    as_tibble() %>%
    rename(R2 = Estimate, R2.Error = Est.Error, R2_Q2.5 = Q2.5, R2_Q97.5 = Q97.5) %>%
    mutate(year = i, model = "pw_fit")
  
  tb %>%
    left_join(bind_rows(pw_r2, lm_r2)) -> tb
  
  waic_results <- bind_rows(waic_results, tb)
  
}

saveRDS(waic_results, "../results/model_comparisons_waic_R2_losat.RDS")
waic_results <- readRDS("../results/model_comparisons_waic_R2_losat.RDS")

#### Plot results ####
waic_results %>%
  select(model, year, elpd_diff, se_diff) %>%
  filter(elpd_diff != 0) %>%
  filter(year %in% c(2002, 2006, 2010, 2014, 2018)) %>%
  rowwise() %>%
  mutate(samples = list(rnorm(n = 1e5, 
                              mean = elpd_diff * -2, 
                              sd   = se_diff *  2)),
         year = paste0("AD", year)
         ) %>%
  select(year, samples) %>%
  group_by(year) %>%
  unnest(samples) %>% 
  mutate(sample_id = c(1:1e5)) %>%
  pivot_wider(id_col = "sample_id", names_from = "year", values_from = "samples") %>%
  select(-sample_id) -> d1

mcmc_intervals(x = d1,
               pars = rev(colnames(d1)),
               prob = 0.00,
               prob_outer = 0.95
  ) +
  geom_vline(aes(xintercept = 0), color = "grey90") +
  theme_test() +
  labs(
    subtitle = "Happiness",
    x = "WAIC difference (95% interval)")
