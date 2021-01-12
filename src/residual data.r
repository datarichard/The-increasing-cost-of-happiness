#### create residual plot ####

library(tidyverse)

path_to_fits <- list.files(
  path = 'results',
  pattern = '^fit_cp_hifdip_gh9_cov_.*.RDS$',
  full.names = TRUE
)

residual_df <- data.frame()

for (path in path_to_fits) {
  fit <- readRDS(path)
  resids = resid(fit)[, "Estimate"]
  fitteds = fitted(fit)[, "Estimate"]
  df <- data.frame(resids, fitteds)
  df$year = substring(path, 31, 34)
  
  residual_df <- bind_rows(residual_df, df)
}

write_rds(residual_df, "results/residual_data.RDS")

resids = resid(pw_fit)[, "Estimate"]
fitteds = fitted(pw_fit)[, "Estimate"]
cor(fitteds, resids)
hist(resids)
ggplot() + geom_point(data = NULL, aes(y = resids, x = fitteds))