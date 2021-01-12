#### Author: RMorris
#### Date: Jan 12th 2021

library(tidyverse)
library(brms)

tibble(
  year = rep(1:16, each = 20),
  dollars = rep(1:10, 32)
  ) %>% 
  rowwise() %>%
  mutate(
    year = as.character(year),
    dollars = dollars + rnorm(1),
    b0 = 8,
    b1 = 2,
    b2 = 0.05,
    omega = 5, #ceiling((year + 2)/2.5),
    y = -log(1/(dollars + 1.5)) + rnorm(1),
    age = sample(18:100, 1),
    male = as.logical(sample(0:1, 1)),
    grad = as.logical(rbinom(1, 1, 0.25))) %>%
  ungroup() %>%
  mutate(
    y = as.vector(scale(y)),
    age = as.vector(scale(age)),
    agesq = age^2
    ) -> df

ggplot(df, aes(x = dollars, y = y)) +
  geom_point() +
  facet_wrap(~year)

hist(df$dollars)
hist(df$y)
summary(df$dollars)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.094   3.038   5.559   5.478   7.930  12.232 

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
    b2 * (dollars - omega) * step(dollars - omega) + b3*age + b4*agesq + b5*male + b6*grad,
  # b0 + b1 + b2 + b3 + b4 + b5 + b6 + alpha ~ 1 + year, # fixed intercept/year (complete pooling)
  # b0 + b1 + b2 + b3 + b4 + b5 + b6 + alpha ~ 1 + (1|year), # random intercept/year (no pooling)
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 15),
  nl = TRUE
)

# priors                                      
bprior <- prior(normal(0, 3), nlpar = "b0") + 
  prior(normal(0, 3), nlpar = "b1") +         
  prior(normal(0, 3), nlpar = "b2") +         
  prior(normal(0, 3), nlpar = "b3") +
  prior(normal(0, 3), nlpar = "b4") +
  prior(normal(0, 3), nlpar = "b5") +
  prior(normal(0, 3), nlpar = "b6") +
  prior(normal(0, sqrt(3)), nlpar = "alpha")      # normal(0, 3) with init = "0" 12/09/2020

# fitting 
fit <- brm(bfixed, data = select(df, y, dollars, age, agesq, male, grad, year), 
           prior = bprior, 
           # iter = 4000,
           # warmup = 1000,
           # control = list(adapt_delta = 0.9),
           # inits = "0", # 12/09/2020. Defaults to "random"
           chains = 4)

summary(fit)

bayes_R2(fit)
# No pooling (random intercept for year): R2 0.3394252 0.03968166 0.2574402 0.4173602
# Complete pooling (fixed intercept for year): R2 0.5103379 0.02689008 0.4557274 0.5587105
# Partial pooling


# Plot the marginal effect of the breakpoint
breakpoint <- inv_logit_scaled(fixef(fit)['alpha_Intercept', 'Estimate'], 0, 15)
p1 <- conditional_effects(fit)

plot(p1, plot = FALSE)[[1]] +
  geom_vline(xintercept = breakpoint)

ans <- pp_expect(fit)
ans <- posterior_predict(fit)
fitted_values <- fitted(fit)

#### Get posterior values for new data ####
# Create new data
newdat = data.frame(
  dollars = c(4, 5, 6),
  age = c(0, 0, 0),
  agesq = c(0, 0, 0),
  male = c(NA, NA, NA), # NA values in factors interpreted as 0
  grad = c(NA, NA, NA)
)

# Fitted: an alias for pp_expect(). By definition, these predictions have smaller 
# variance than the posterior predictions performed by posterior_predict() method. 
# This is because only the uncertainty in the mean is incorporated in the samples
# computed by pp_expect while any residual error is ignored. (estimated means 
# should be very similar)
fitted(fit, newdata = newdat) %>% as_tibble()

       Estimate  Est.Error        Q2.5     Q97.5
[1,] 0.03297785 0.04745212 -0.05885062 0.1265556
[2,] 0.64673966 0.05080681  0.54451983 0.7411650
[3,] 0.70732762 0.04623016  0.61830409 0.8008886
       
posterior_predict(fit, newdata = newdat) %>%
  as_tibble() %>%
  summarise(across(everything(), list(Estimate = mean, 
                                      Est.Error = sd,
                                      Q2.5 = ~quantile(.x, probs = c(0.025))[[1]],
                                      Q97.5 = ~quantile(.x, probs = c(0.975))[[1]])
                   )) %>%
  gather(p, val) %>%
  separate(col = p, into = c("person", "param"), sep = "_") %>%
  spread(param, val) %>%
  select(Estimate, Est.Error, everything(), -person)




dat <- as.data.frame(cbind(Y = standata(fit)$Y, fitted_values))
ggplot(dat) + geom_point(aes(x = Estimate, y = Y))



##### Piecewise simulation #####
df <- tibble(
  year = rep(1:5, each = 100),
  dollars = rep(1:10, times = 50),
  omega = year + 2
)

df$y = 0.5*df$dollars*(df$dollars < df$omega) + 0.8*df$year*(df$dollars >= df$omega) + 
  rnorm(n=nrow(df), sd=0.5)
df$dollars = df$dollars + rnorm(n=nrow(df), sd = 0.25)
df$year = as.character(df$year)

# Scale the outcome variable
df %>%
  # group_by(year) %>%
  mutate(y = as.vector(scale(y))) %>%
  ungroup() -> sdf

ggplot(sdf, aes(x = dollars, y = y)) +
  geom_point() +
  facet_wrap(~year)

# Nb. Partial pooling still best if scaled by year, scaled overall or no scaling.

# priors                                      
bprior <- prior(normal(0, 3), nlpar = "Intercept") + 
  prior(normal(0, 3), nlpar = "slope1") +         
  prior(normal(0, 3), nlpar = "slope2") +         
  prior(normal(0, 3), nlpar = "alpha")      # normal(0, 3) with init = "0" 12/09/2020

###### A no pooling model #####
no_pool <- bf(
  y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
    slope2 * (dollars - omega) * step(dollars - omega),
  Intercept + slope1 + slope2 ~ 1 + (1|year), 
  alpha ~ 0 + year, # single fixed intercept for each year (i.e., no pooling)
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)

get_prior(no_pool, sdf)

# fitting 
fit_np <- brm(no_pool, data = select(sdf, y, dollars, year), 
           prior = bprior, 
           chains = 4)

summary(fit_np)


bayes_R2(fit_np)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.8115902 0.006896374 0.796865 0.8240293

fixef(fit_np) %>%
  as.data.frame() %>%
  rownames_to_column("Param") %>%
  filter(str_detect(Param, "alpha")) %>%
  transmute(
    Param,
    omega = inv_logit_scaled(Estimate, 0, 10),
    true_omega = 3:7,
    delta = abs(omega - true_omega)) %>%
  add_row(delta = mean(.$delta))

#       Param    omega true_omega     delta
# alpha_year1 2.237699          3 0.7623007
# alpha_year2 3.649569          4 0.3504309
# alpha_year3 3.595123          5 1.4048768
# alpha_year4 6.661551          6 0.6615507
# alpha_year5 7.652675          7 0.6526751
#                                 0.7663668 (largest error due to overfitting)


###### A partial pooling model ####
bfixed <- bf(
  y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
    slope2 * (dollars - omega) * step(dollars - omega),
  Intercept + slope1 + slope2 + alpha ~ 1 + (1|year), # fixed for all years, random each year
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)

# fitting 
fit_pp <- brm(bfixed, data = select(sdf, y, dollars, year), 
           prior = bprior, 
           chains = 4)

summary(fit_pp)

bayes_R2(fit_pp)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.8118319 0.006811583 0.7973636 0.8238251


ranef(fit_pp)$year[, ,4] %>%
  as.data.frame() %>%
  rownames_to_column("Year") %>%
  transmute(
    Year,
    overall_omega = inv_logit_scaled(fixef(fit_pp)['alpha_Intercept', 'Estimate'], 0, 10),
    random_omega = inv_logit_scaled(Estimate, 0, 10),
    true_omega = 3:7,
    delta = abs(random_omega - true_omega)
  ) %>%
  add_row(delta = mean(.$delta))

# Year overall_omega random_omega true_omega     delta
#    1      4.858804     2.642111          3 0.3578888
#    2      4.858804     3.866522          4 0.1334778
#    3      4.858804     3.763470          5 1.2365305
#    4      4.858804     6.755202          6 0.7552020
#    5      4.858804     7.622047          7 0.6220470
#                                            0.6210292 0.3221 0.47719 (smallest error)

fit_np <- add_criterion(fit, "waic")
fit_pp <- add_criterion(fit_pp, "waic")

print(loo_compare(fit_np, fit_pp, criterion = "waic"), simplify = F)

###### A complete pooling model #####
bfixed <- bf(
  y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
    slope2 * (dollars - omega) * step(dollars - omega),
  Intercept + slope1 + slope2 + alpha ~ 1, # complete pooling
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)

# This model produces a single alpha parameter for all years. A single alpha 
# will have the largest L1 difference from the true alphas, which increase over 
# time. 

















##### Testing covariates #####
df %>%
  mutate(
    age = sample(18:100, nrow(df), replace = T),
    male = sample(0:1, nrow(df), replace = T),
    grad = rbinom(nrow(df), 1, 0.25)) %>% 
  transmute(
    year,
    y = as.vector(scale(y)),
    dollars,
    age = as.vector(scale(age)),
    agesq = age^2,
    male = as.logical(male),
    grad = as.logical(grad)
  ) -> df_cov

mixed_cov <- bf(
  y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
    slope2 * (dollars - omega) * step(dollars - omega) + 
    b3*age + b4*agesq + b5*male + b6*grad,
  Intercept + slope1 + slope2 ~ 1 + (1|year),
  b3 + b4 + b5 + b6 ~ 1 + (1|year),
  # alpha ~ 1, # single fixed intercept for all years (complete pooling)
  alpha ~ 0 + year, # fixed intercept for each year (no pooling)
  # alpha ~ 1 + (1|year), # fixed overall years and random for each year (partial pooling)
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)
  

# priors                                      
bpriors <- prior(normal(0, 3), nlpar = "Intercept") + 
  prior(normal(0, 3), nlpar = "slope1") +         
  prior(normal(0, 3), nlpar = "slope2") +         
  prior(normal(0, 3), nlpar = "b3") +
  prior(normal(0, 3), nlpar = "b4") +
  prior(normal(0, 3), nlpar = "b5") +
  prior(normal(0, 3), nlpar = "b6") +
  prior(normal(0, 3), nlpar = "alpha")
  

get_prior(mixed_cov, df_cov)

# fitting 
np_cov <- brm(mixed_cov, data = df_cov, 
              prior = bpriors, 
              chains = 4)

summary(np_cov)

bayes_R2(np_cov)
#     Estimate   Est.Error      Q2.5     Q97.5
# R2 0.8118319 0.006811583 0.7973636 0.8238251  pp_cov
# R2 0.8105453 0.007149228 0.7951895 0.8231396  np_cov


year_effect_table <- function(.fit, pp = T) {
  
  if (pp) {
    ranef(.fit)$year[, ,4] %>%
      as.data.frame() %>%
      rownames_to_column("Year") %>%
      transmute(
        Year,
        fixed_omega = inv_logit_scaled(fixef(.fit)['alpha_Intercept', 'Estimate'], 0, 10),
        random_omega = inv_logit_scaled(Estimate, 0, 10),
        true_omega = 3:7,
        delta = abs(random_omega - true_omega)
      ) %>%
      add_row(delta = mean(.$delta))
    
  } else {
    fixef(.fit) %>%
      as.data.frame() %>%
      rownames_to_column("Param") %>%
      filter(str_detect(Param, "alpha")) %>%
      transmute(
        Param,
        fixed_omega = inv_logit_scaled(Estimate, 0, 10),
        true_omega = 3:7,
        delta = abs(fixed_omega - true_omega)) %>%
      add_row(delta = mean(.$delta))
  }
}


year_effect_table(pp_cov)

# Partial pooling with fixed covariates
# Year fixed_omega random_omega true_omega     delta
#    1    4.758764     2.101341          3 0.8986595
#    2    4.758764     3.414090          4 0.5859097
#    3    4.758764     5.244360          5 0.2443597
#    4    4.758764     6.175476          6 0.1754764
#    5    4.758764     8.223196          7 1.2231960
#                                          0.6255203

# Partial pooling with covariates (also partial pooled)
#   Year overall_omega random_omega true_omega     delta
# 1    1      4.812979     4.947426          3 1.9474255
# 2    2      4.812979     5.053259          4 1.0532593
# 3    3      4.812979     5.037792          5 0.0377917
# 4    4      4.812979     4.978598          6 1.0214019
# 5    5      4.812979     4.993688          7 2.0063119
#                                              1.2132381

year_effect_table(np_cov, pp = F)

# No pooling for alpha but everything else (slopes, cov) partially pooled
#         Param    omega true_omega      delta
# 1 alpha_year1 1.853979          3 1.14602094
# 2 alpha_year2 3.149105          4 0.85089521
# 3 alpha_year3 5.022325          5 0.02232532
# 4 alpha_year4 5.939788          6 0.06021220
# 5 alpha_year5 8.222124          7 1.22212351
#                                   0.66031543

# No pooling with fixed estimates for each year (alpha, slopes, cov, etc)
#         Param    omega true_omega      delta
# 1 alpha_year1 1.275881          3 1.72411947
# 2 alpha_year2 3.368558          4 0.63144177
# 3 alpha_year3 4.891789          5 0.10821140
# 4 alpha_year4 5.934890          6 0.06510964
# 5 alpha_year5 9.393122          7 2.39312195
# 6                                 0.98440085

# Conclusions
# Using fixed year effects for alpha tends to exagerate the change in alpha
# over time (relative to true alpha). Using partial pooling (or any random 
# effect?) for other variables seems to reduce error for the fixed alphas.
# 
# Thus:
# y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
#     slope2 * (dollars - omega) * step(dollars - omega) + 
#     b3*age + b4*agesq + b5*male + b6*grad,
# Intercept + slope1 + slope2 + b3 + b4 + b5 + b6 ~ 1 + (1|year), (partial pooling)
# alpha ~ 0 + year (no pooling)





##### Appendix #####
mixed <- bf(
  y ~ Intercept + slope1 * (dollars - omega) * step(omega - dollars) + 
    slope2 * (dollars - omega) * step(dollars - omega),
  Intercept + slope1 + slope2 ~ 1,
  # alpha ~ 1, # single fixed intercept for all years (complete pooling)
  # alpha ~ 0 + (1|year), # random intercept for each year (no pooling)
  alpha ~ 1 + (1|year), # fixed overall years and random for each year (partial pooling)
  # Sigmoid transform to keep omega within the range of 0 to x
  nlf(omega ~ inv_logit(alpha) * 10),
  nl = TRUE
)

# priors                                      
bprior <- prior(normal(0, 3), nlpar = "Intercept") + 
  prior(normal(0, 3), nlpar = "slope1") +         
  prior(normal(0, 3), nlpar = "slope2") +         
  prior(normal(0, 3), nlpar = "alpha")      # normal(0, 3) with init = "0" 12/09/2020

# fitting 
fit <- brm(mixed, data = select(sdf, y, dollars, year), 
           prior = bprior, 
           chains = 4)

summary(fit)

bayes_R2(fit)
# complete pooling: R2 0.2591992  0.028214 0.2016516 0.3125608 (alpha ~ 1)
# partial pooling:  R2 0.6794199 0.0133884 0.6519889 0.703303 (alpha ~ 1 + (1|year))
# highest R2 from a model with fixed parameters each year (0.82!) - probably overfit!

# Plot the marginal effect of the breakpoint
breakpoint <- inv_logit_scaled(fixef(fit)['alpha_Intercept', 'Estimate'], 0, 10)
p1 <- conditional_effects(fit)

plot(p1, plot = FALSE)[[1]] +
  geom_vline(xintercept = breakpoint)

as_tibble(ranef(fit)[[1]]) %>%
  mutate(
    fixed_omega = inv_logit_scaled(fixef(fit)['alpha_Intercept', 'Estimate'], 0, 10),
    total_omega = inv_logit_scaled(fixef(fit)['alpha_Intercept', 'Estimate'] - 
                                     Estimate.alpha_Intercept, 0 ,10),
  )