library(tidyverse)
library(brms)

tibble(
  year = rep(1:16, each = 20),
  dollars = rep(1:10, 32)
  ) %>% 
  rowwise() %>%
  mutate(
    dollars = dollars + rnorm(1),
    b0 = 8,
    b1 = 2,
    b2 = 0.05,
    omega = 5, #ceiling((year + 2)/2.5),
    y = b0 + b1*(dollars - omega)*(omega > dollars) + b2*(dollars - omega)*(dollars >= omega) + rnorm(1),
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

hist(df$wealth)
hist(df$y)
summary(df$dollars)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.094   3.038   5.559   5.478   7.930  12.232 

bfixed <- bf(
  y ~ b0 + b1 * (dollars - omega) * step(omega - dollars) + 
    b2 * (dollars - omega) * step(dollars - omega) + b3*age + b4*agesq + b5*male + b6*grad,
  b0 + b1 + b2 + b3 + b4 + b5 + b6 + alpha ~ 1, #+ (1|male + grad),  # fixed intercept + (random intercept per sex and grad)
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
fit <- brm(bfixed, data = select(df, y, dollars, age, agesq, male, grad), 
           prior = bprior, 
           # iter = 4000,
           # warmup = 1000,
           # control = list(adapt_delta = 0.9),
           # inits = "0", # 12/09/2020. Defaults to "random"
           chains = 4)

summary(fit)

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