#### The Increasing Cost of Happiness ####
# RW Morris, N Kettlewell, N Glozier
# 
# Model specification and fitting
# Nov 21st 2020
# 
#### libraries ####
library(dplyr)
library(ggplot2)
library(brms)
options(mc.cores = 4)

#### Import ####
happywealth <- readRDS("../data/happywealth.rds")

#### Preprocessing ####
happywealth %>%
  filter(!student, year == 2001) %>%
  transmute(
    y = as.vector(scale(gh9)), # gh9
    dollars = hifdip_adj/10000, 
    male = as.numeric(male),
    age = as.vector(scale(age)),
    age_sq = age^2,
    grad = as.numeric(edu %in% c("Grad", "Postgrad"))
  ) %>%
  na.omit() -> df

# Checking preprocessing...
df %>%
  mutate(decile = ntile(dollars, 10)) %>% 
  group_by(decile) %>%
  summarise(dollars = mean(dollars), 
            y = mean(y)) %>%
  ggplot(aes(x = dollars, y = y)) + geom_point()

summary(df$dollars)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   2.697   4.176   4.758   6.065  40.311 2002 | gh9
#  0.000   3.588   5.362   6.173   7.706  85.139 2018 | gh9

summary(df$y)
#  Min.     1st Qu.  Median  Mean    3rd Qu. Max. 
#  -4.0191  -0.6054  0.1824  0.0000  0.7076  1.8893  2002 | gh9
#  -4.90231 -0.54261 0.08021 0.00000 0.70302 1.32584 2002 | losat

hist(df$dollars, breaks = 100)
hist(df$y, breaks = 10)

#### Fit the model ####
#
# This model was suggested at: 
# https://discourse.mc-stan.org/t/piecewise-linear-mixed-models-with-a-random-change-point/5306/5
# 
# Model definition
# b0 is the intercept denoting the expected value of wellbeing (y) at the changepoint
# b1 is the linear slope before the changepoint
# b2 is the linear slope after the changepoint
# alpha is the coefficient of wealth (x) at the changepoint

bfixed <- bf(
  y ~ b0 + b1 * (dollars - omega) * step(omega - dollars) + 
    b2 * (dollars - omega) * step(dollars - omega) + b3*grad + b4*age + b5*age_sq + b6*male,
  b0 + b1 + b2 + b3 + b4 + b5 + b6 + alpha ~ 1,  # fixed intercepts for each year
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 100),
  nl = TRUE
)

# priors                                      I tried tighter priors:
bprior <- prior(normal(0, 3), nlpar = "b0") + # normal(0, 1) did not converge Sep 6th
  prior(normal(0, 3), nlpar = "b1") +         # normal(0, 1) did not converge Sep 6th
  prior(normal(0, 3), nlpar = "b2") +         # normal(0, 1) did not converge Sep 6th
  prior(normal(0, 3), nlpar = "b3") +  
  prior(normal(0, 3), nlpar = "b4") +  
  prior(normal(0, 3), nlpar = "b5") +  
  prior(normal(0, 3), nlpar = "b6") +
  prior(normal(0, 3), nlpar = "alpha")      # normal(0, 3) with init = "0" 12/09/2020
                                            # normal(0, sqrt(3)) with default init didn't converge

# e.g., plot prior for alpha
hist(inv_logit_scaled(rnorm(1000, mean = 0, sd = 3), 0, 100))
inv_logit_scaled(1, 0, 100)

# fitting (try more iterations)
fit <- brm(bfixed, data = df, 
           prior = bprior, 
           iter = 4000,
           warmup = 1000,
           control = list(adapt_delta = 0.95),
           inits = "0", # 12/09/2020. Defaults to "random"
           chains = 4)

summary(fit)

# Family: gaussian 
#   Links: mu = identity; sigma = identity 
# Formula: y ~ b0 + b1 * (dollars - omega) * step(omega - dollars) + 
#   b2 * (dollars - omega) * step(dollars - omega) + 
#   b3 * grad + b4 * age + b5 * age_sq + b6 * male 
#         b0 ~ 1
#         b1 ~ 1
#         b2 ~ 1
#         b3 ~ 1
#         b4 ~ 1
#         b5 ~ 1
#         b6 ~ 1
#         alpha ~ 1
#         omega ~ inv_logit(alpha) * 100
#    Data: df (Number of observations: 10791) 
# Samples: 4 chains, each with iter = 4000; warmup = 1000; thin = 1;
#          total post-warmup samples = 12000
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# b0_Intercept       -0.00      0.03    -0.06     0.06 1.00     5323     3825
# b1_Intercept        0.11      0.02     0.08     0.14 1.00     5744     4414
# b2_Intercept        0.01      0.00     0.00     0.02 1.00     8616     6197
# b3_Intercept        0.06      0.02     0.01     0.11 1.00    10752     8542
# b4_Intercept        0.06      0.01     0.04     0.08 1.00    10756     8857
# b5_Intercept        0.00      0.01    -0.02     0.02 1.00    11163     9103
# b6_Intercept        0.15      0.02     0.11     0.18 1.00     9774     8327
# alpha_Intercept    -3.07      0.11    -3.24    -2.80 1.00     4863     2996
# 
# Family Specific Parameters: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.99      0.01     0.97     1.00 1.00    12558     8578
# 
# Samples were drawn using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).


#### Check the results #####
#
# Plot the marginal effects
breakpoint <- inv_logit_scaled(fixef(fit)['alpha_Intercept', 'Estimate'], 0, 100)
p1 <- conditional_effects(fit)

plot(p1, plot = FALSE)[[1]] +
  geom_vline(xintercept = breakpoint)


# Posteriors
posteriors <- as.data.frame(fit)

# > colnames(posteriors)
# [1] "b_b0_Intercept"    "b_b1_Intercept"    "b_b2_Intercept"    "b_b3_Intercept"   
# [5] "b_b4_Intercept"    "b_b5_Intercept"    "b_b6_Intercept"    "b_alpha_Intercept"
# [9] "sigma"             "lp__"  

# Covariance matrix
select(posteriors, b_b0_Intercept:b_b2_Intercept, sigma) %>%
  cor()

##### Calculate the changepoint dollar value ####
posteriors %>%
  select(b_alpha_Intercept) %>%
  mutate(omega = inv_logit_scaled(b_alpha_Intercept, 0, 100)*10) %>%
  tidyr::gather(parameters) %>%
  group_by(parameters) %>%
  summarise(
    Mean = mean(value),
    SD = sd(value),
    `2.5` = quantile(value, probs = .025),
    `97.5` = quantile(value, probs = .975)
  )

# A tibble: 2 x 5
# parameters         Mean    SD `2.5` `97.5`
# <chr>             <dbl> <dbl> <dbl>  <dbl>
# b_alpha_Intercept -3.07 0.111 -3.24  -2.80
# omega             $44.6K 5.17  37.7   57.4   2002
# omega             $54.5K 6.22  45.9   70.5   2014
# omega             $72.6K 4.83  62.9   82.1   2018

saveRDS(fit, "../results/fit_cp_hifdip_gh9_cov_2001.RDS")

#### Piece-wise fits ####
for (i in c(2018:2010)) {
  
  #### Preprocessing ####
  happywealth_covs %>%
    filter(!student, year == i) %>%
    transmute(
      y = as.vector(scale(losat)), # gh9
      dollars = adj_hifdip/10000, 
      male = as.numeric(male),
      age = as.vector(scale(age)),
      age_sq = age^2,
      grad = as.numeric(edu %in% c("Grad", "Postgrad"))
    ) %>%
    na.omit() -> df
  
  # fitting (try more iterations)
  fit <- brm(bfixed, data = df, 
             prior = bprior, 
             iter = 4000,
             warmup = 1000,
             control = list(adapt_delta = 0.95),
             inits = "0", # 12/09/2020. Defaults to "random"
             chains = 4)
  
  filepath <- paste0("../results/fit_cp_hifdip_losat_cov_", i, ".RDS")
  saveRDS(fit, file = filepath)
}


#### Linear fits ####
# using default priors
for (i in c(2002:2018)) {
  
  #### Preprocessing ####
  happywealth_covs %>%
    filter(!student, year == i) %>%
    transmute(
      y = as.vector(scale(losat)), # gh9
      dollars = adj_hifdip/10000, 
      male = as.numeric(male),
      age = as.vector(scale(age)),
      age_sq = age^2,
      grad = as.numeric(edu %in% c("Grad", "Postgrad"))
    ) %>%
    na.omit() -> df
  
  # fitting 
  fit <- brm(formula = y ~ 1 + dollars + grad + age + age_sq + male,
             data = df, 
             iter = 4000,
             warmup = 1000,
             control = list(adapt_delta = 0.95),
             inits = "0", # 12/09/2020. Defaults to "random"
             chains = 4)
  
  filepath <- paste0("../results/fit_lm_hifdip_losat_cov_", i, ".RDS")
  saveRDS(fit, file = filepath)
}






