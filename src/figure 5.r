
predicted_fits <- list()
for (pathtofile in path_to_fits) {
  fit <- read_rds(pathtofile) 
  
  ans <- model.frame(fit) %>%
    summarise_all(median)
  
  newdat = data.frame(
    dollars = c(5, ans[, "dollars"], 8),
    age = rep(ans[, "age"], 3),
    age_sq = rep(ans[, "age_sq"], 3),
    male = rep(ans[, "male"], 3), 
    grad = rep(ans[, "grad"], 3)
  )
  
  df <- fitted(fit, newdata = newdat) %>% 
    as_tibble() %>%
    mutate(
      Dollars = c("$50K", "\u03bc", "$80K"),
      year = as.numeric(substring(pathtofile, 34, 37)))
  
  predicted_fits <- bind_rows(predicted_fits, df)
}

#### plot those fits
predicted_fits %>%
  mutate(Dollars = fct_relevel(Dollars, "$80K", "\u03bc")) %>%
  ggplot(aes(x = year, y = Estimate, color = Dollars)) +
    geom_point(size = 4, alpha = 0.25) +
    geom_smooth(method = "loess", se = F, span = 3) +
    facet_wrap(~Dollars) +
    labs(title = "Predicted happiness at varying income levels") +
    ggthemes::scale_colour_wsj() +
    ggthemes::theme_wsj() +
    theme(legend.position = "none",
          plot.title = element_text(size = 19),
          plot.caption = element_text(size = 12),
          strip.text = element_text(size = 12))

#### plot those fits
predicted_fits %>%
  select(Estimate, Dollars, year) %>%
  filter(Dollars %in% c("$80K", "$50K")) %>%
  spread(Dollars, Estimate) %>%
  # mutate(`∆` = `$80K` - `$50K`) %>%
  gather(income, happiness, -year) %>%
  mutate(income = fct_relevel(income, "$50K", "$80K")) %>%
  ggplot(aes(x = year, y = happiness, color = income)) +
  geom_point(size = 4, alpha = 0.25) +
  geom_smooth(method = "loess", se = F, span = 3) +
  facet_wrap(~income) +
  labs(title = "Estimated happiness at varying income\nin Australia from 2002-2018",
       caption = "Source: HILDA, Melbourne University") -> p

p +
  ggthemes::scale_colour_wsj() +
  ggthemes::theme_wsj() +
  theme(legend.position = "none",
        plot.title = element_text(size = 19),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 12))
