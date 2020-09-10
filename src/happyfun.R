plot_linear <- function(.data, swb_col, dollars_col) {
 

  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, 
           year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb > 0) %>%
    filter(dollars > 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) -> df
  
  df %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(dollars, 10)
    ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T)/1000,
      n = n()
    ) -> df.summary
  
  df %>%
    mutate(
      wealth = dollars/1000,
      wellbeing = swb
    ) -> df.plot
  
  # general additive model
  ggplot(df.plot, aes(x = wealth, y = wellbeing)) +
    geom_smooth(
      method = "lm", 
      size = 0.5,
      se = TRUE) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, ncol = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(linear)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test() +
    theme(
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.075, 0.925)) -> p1
  
  p1 +
    geom_point(data = df.summary) 
  
}

plot_inflection_piecewise <- function(.data, swb_col, dollars_col) {
  
  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb >= 0) %>%
    filter(dollars >= 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) -> df
  
  df %>%
    mutate(
      wealth = dollars/1000,
      wellbeing = swb
    ) -> df.plot
  
  df %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(dollars, 10),
      knots = attr(splines::bs(dollars, degree = 1, df = 2), which = "knots")
    ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T)/1000,
      knot = unique(knots)/1000,
      n = n()
    ) -> df.summary
  
  # piecewise model
  ggplot(df.summary, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_smooth(data = df.plot,
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1),
                size = 0.5) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, ncol = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(piecewise)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}

plot_inflection_gam <- function(.data, swb_col, dollars_col) {
  
  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)

  .data %>%
    select(xwaveid, 
           year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb > 0) %>%
    filter(dollars > 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) -> df
  
  df %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(dollars, 10)
    ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T)/1000,
      n = n()
    ) -> df.summary
  
  df %>%
    mutate(
      wealth = dollars/1000,
      wellbeing = swb
    ) -> df.plot
  
  # general additive model
  ggplot(df.plot, aes(x = wealth, y = wellbeing)) +
    geom_smooth(
      method = "gam", 
      formula = y ~ s(x, bs = "cs"),
      size = 0.5,
      se = TRUE) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, ncol = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(GAM)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test() +
    theme(
      legend.background = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.075, 0.925)) -> p1
  
  # ggplot_build(p1)$data[[1]] %>%
  #   group_by(PANEL) %>% 
  #   mutate(delta_y = abs(round(y, 3) - round(lag(y), 3))) %>%
  #   summarise(
  #     wealth = x[which.min(delta_y)],
  #     wellbeing = y[which.min(delta_y)]
  #   ) %>%
  #   mutate(
  #     year = recode(as.numeric(PANEL), 2002, 2006, 2010, 2014, 2018)
  #   ) -> df.inflect
  
  p1 +
    geom_point(data = df.summary) 

}

plot_inflection_cubic <- function(.data, swb_col, dollars_col) {
  
  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb >= 0) %>%
    filter(dollars >= 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) -> df
  
  df %>%
    mutate(
      wealth = dollars/1000,
      wellbeing = swb
    ) -> df.plot
  
  df %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(dollars, 10)
    ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T)/1000,
      n = n()
    ) -> df.summary

  # b-spline cubic model
  ggplot(df.summary, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_smooth(data = df.plot,
                method = lm, 
                formula = y ~ splines::bs(x, df = 4, degree = 3),
                size = 0.5, se = FALSE) +
    geom_smooth(data = df.plot,
                method = lm, 
                formula = y ~ splines::bs(x, df = 5, degree = 3),
                colour = "blue3",
                size = 0.5,
                se = FALSE) +
    geom_smooth(data = df.plot,
                method = lm, 
                formula = y ~ splines::bs(x, df = 6, degree = 3),
                colour = "blue4",
                size = 0.5,
                se = FALSE) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, ncol = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(b-spline)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}

compare_bic_year <- function(.data, swb_col, dollars_col) {
  
  require(broom)
  
  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb >= 0) %>%
    filter(dollars > 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) %>%
    group_by(year) %>%
    # mutate(
    #   swb = scale(swb),
    #   dollars = scale(dollars)
    # ) %>%
    nest() %>%
    mutate(
      fit_broken = map(data, ~ lm(formula = swb ~ splines::bs(dollars, df = 2, degree = 1), 
                                  data = .x)),
      fit_unbroken = map(data, ~ lm(formula = swb ~ dollars, 
                                    data = .x)),
      glanced_broken = map(fit_broken, glance),
      glanced_unbroken = map(fit_unbroken, glance)
    ) -> results
  
  results %>%
    unnest(glanced_unbroken) %>%
    mutate(model = "unbroken") %>%
    select(model, year, r.squared:df.residual) -> unbroken_results
  
  results %>%
    unnest(glanced_broken) %>%
    mutate(model = "broken") %>%
    select(model, year, r.squared:df.residual) %>%
    bind_rows(unbroken_results) %>%
    ungroup() -> results_table
  
  select(results_table, Year = year, model, BIC) %>%
    spread(model, BIC) %>%
    mutate(
      `BIC difference` = broken - unbroken,
      Evidence = case_when(
        `BIC difference` < -10 ~ "Very strong evidence for broken",
        `BIC difference` < -5 ~ "Strong evidence for broken",
        `BIC difference` < -2 ~ "Positive evidence for broken",
        `BIC difference` < -0 ~ "Weak evidence for broken",
        `BIC difference` > 10 ~ "Very strong evidence for unbroken",
        `BIC difference` > 5 ~ "Strong evidence for unbroken",
        `BIC difference` > 2 ~ "Positive evidence for unbroken",
        `BIC difference` > 0 ~ "Weak evidence for unbroken",
        TRUE ~ "None"
      )) %>%
    flextable() %>%
    color(i = ~ `BIC difference` > 0, color = "red")
}

plot_inflection_seifa <- function(.data, swb_col, dollars_col) {
  
  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, year, SEIFA, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb >= 0) %>%
    filter(dollars >= 0) %>%
    filter(year %in% c(2002, 2006, 2010, 2014, 2018)) %>%
    group_by(SEIFA) %>%
    mutate(
      swb = scale(swb),
      dollars = scale(dollars)
    ) -> df
  
  df %>%
    mutate(
      wealth = dollars,
      wellbeing = swb
    ) -> df.plot
  
  df %>%
    group_by(SEIFA) %>%
    mutate(
      dollar_bin = ntile(dollars, 10),
      knots = attr(splines::bs(dollars, degree = 1, df = 2), which = "knots")
    ) %>%
    group_by(SEIFA, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T),
      knot = unique(knots),
      n = n()
    ) -> df.summary
  
  # piecewise
  ggplot(df.summary, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_smooth(data = df.plot,
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1),
                size = 0.5) +
    geom_vline(aes(xintercept = knot),
               color = "red", linetype = 3, size = 0.5) +
    xlim(min(df.summary$wealth), max(df.summary$wealth)) +
    facet_wrap(~SEIFA, scales = "free_x") +
    labs(subtitle = rlang::as_name(swb_colname),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}