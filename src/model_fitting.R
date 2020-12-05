#### The Increasing Cost of Happiness ####
# RW Morris, N Kettlewell, N Glozier
# 
# Model specification and fitting
# Dec 4th 2020
# 
#### libraries ####
library(dplyr)
library(Rcpp)
library(brms)
options(mc.cores = parallel::detectCores())
ncores = parallel::detectCores()

#### Import ####
happywealth <- readRDS("../data/happywealth.rds")

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

#### Piece-wise fits ####
for (i in c(2001:2018)) {
  
  #### Preprocessing ####
  happywealth %>%
    filter(!student, !top_hifdip, year == i) %>%
    transmute(
      y = as.vector(scale(losat)), # gh9
      dollars = hifdip_adj/10000, 
      male = as.logical(male),
      age = as.vector(scale(age)),
      age_sq = age^2,
      grad = edu %in% c("Grad", "Postgrad")
  ) %>%
  na.omit() -> df

# nonlinear fitting 
fit_nl <- brm(formula = bfixed, 
              data = df, 
              prior = bprior, 
              control = list(adapt_delta = 0.95),
              inits = "0", # 12/09/2020. Defaults to "random"
              chains = ncores)

filepath_nl <- paste0("../results/fit_nl_hifdip_losat_cov_", i, ".RDS")
saveRDS(fit_nl, file = filepath_nl)

# linear fitting 
fit_lm <- brm(formula = y ~ 1 + dollars + grad + age + age_sq + male,
              data = df, 
              control = list(adapt_delta = 0.95),
              inits = "0", # 12/09/2020. Defaults to "random"
              chains = ncores)

filepath_lm <- paste0("../results/fit_lm_hifdip_losat_cov_", i, ".RDS")
saveRDS(fit_lm, file = filepath_lm)
}









