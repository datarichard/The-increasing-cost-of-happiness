library(bayesplot)


fit <- read_rds("../results/fit_cp_hifdip_losat_cov_2006.RDS")

plot(fit)

path_to_fits <- list.files(
  path = "../results",#'~/Dropbox/HILDA/data',
  pattern = '^fit_cp_hifdip_losat_cov_.*.RDS$',
  full.names = TRUE
)

alphas <- list()
for (pathtofile in path_to_fits) {
  df <- read_rds(pathtofile) %>%
    as.data.frame() %>%
    select(b_alpha_Intercept) 
  
  # colnames(df) <- paste0("b_alpha_", substring(pathtofile, 34, 37)) # gh9
  colnames(df) <- paste0("b_alpha_", substring(pathtofile, 36, 39)) # losat
  
  alphas <- bind_cols(alphas, df)
}

alphas %>%
  mutate_all(.funs = ~brms::inv_logit_scaled(., 0, 100)*10) -> posteriors

# OPTIONAL: Trim posteriors for 2003 outlier
posteriors %>%
  mutate(b_alpha_2003 = (b_alpha_2002 + b_alpha_2004)/2) -> posteriors

# OPTIONAL: Trim posteriors for 2006 outlier (did not converge)
posteriors %>%
  mutate(b_alpha_2006 = (b_alpha_2005 + b_alpha_2007)/2) -> posteriors

color_scheme_set("blue")

mcmc_areas_ridges(
  posteriors,
  pars = rev(colnames(posteriors)),
  prob = 0.90, # 80% intervals
  prob_outer = .99 # 99%
  ) +
  theme_test() +
  geom_vline(aes(xintercept = median(posteriors$b_alpha_2002)), 
             color = "gray", 
             linetype = 3) +
  labs(subtitle = "Cost of happiness by year and household wealth",
       x = "Household wealth ($000s)")

happywealth_covs %>%
  select(year, xwaveid, adj_hifdip) %>%
  mutate(
    year = paste0("b_alpha_", year),
    adj_hifdip = adj_hifdip/1000) %>%
  left_join(
    gather(
      summarise_all(posteriors, list(median)), year, change_point
    )) %>%
  mutate(
    year = extract_numeric(year)
    ) %>%
  group_by(year) %>%
  summarise(
    `Median household wealth ($000s)\n` = round(median(adj_hifdip), 2),
    # `mean ($000s)` = round(mean(adj_hifdip), 2),
    `Cost of happiness ($000s)\n` = round(mean(change_point), 2),
    `Percent pop. afford happiness\n` = round(mean(adj_hifdip > mean(change_point))*100, 1)
    ) -> results


# plot the results
library(ggthemes)


p <- results %>%
  gather("key", "value", -year) %>%
  mutate(key = fct_relevel(key, "Median household wealth ($000s)\n")) %>%
  ggplot(aes(x = year, y = value, color = key)) +
    geom_line(aes(group = key), size = 2, alpha = 0.8) +
    labs(title = "Wealth & happiness in Australia between 2002-2018",
         caption = "Source: HILDA, Melbourne Institute\nVertical dotted line: GFC",
         x = "  ", y = "") +
    geom_vline(aes(xintercept = 2009), color = "red", linetype = 3) +
    facet_wrap(~key)

p +
  scale_colour_wsj("colors6", "") +
  ggthemes::theme_wsj() +
  theme(legend.position = "none",
        plot.title = element_text(size = 19),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 12))

p +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        strip.text = element_text(size = 12),
        panel.grid.major.x = element_blank())

p + 
  labs(subtitle = "  ") +
  scale_color_economist() +
  theme_economist(base_family="Verdana", base_size = 8) +
  theme(legend.position = "none",
        strip.text = element_text(size = 12)
        )




