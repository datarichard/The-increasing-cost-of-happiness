plot_linear <- function(.data, swb_col, dollars_col) {
 

  swb_colname <- rlang::enexpr(swb_col)
  dollars_colname <- rlang::enexpr(dollars_col)
  
  .data %>%
    select(xwaveid, 
           year, 
           # swb = enexpr(swb_col), dollars = enexpr(dollars_col)) %>%
           swb = !!swb_colname, dollars = !!dollars_colname) %>%
    filter(swb >= 0) %>%
    filter(dollars >= 0) %>%
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
    facet_wrap(~year, nrow = 1) +
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
                colour = "deepskyblue",
                size = 0.5,
                se = FALSE) +
    geom_smooth(data = df.plot,
                method = lm, 
                formula = y ~ splines::bs(x, df = 6, degree = 3),
                colour = "navy",
                size = 0.5,
                se = FALSE) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(b-spline)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}

plot_inflection_piecewise <- function(.data, swb_col, dollars_col, knots) {
  
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
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(piecewise)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}

plot_inflection_custom <- function(.data, swb_col, dollars_col, knots) {
  
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
  
  # piecewise model
  ggplot(df.summary, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_smooth(data = filter(df.plot, year == 2002),
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[1]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2006),
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[2]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2010),
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[3]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2014),
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[4]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2018),
                aes(x = wealth, y = wellbeing),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[5]),
                size = 0.5) +
    coord_cartesian(xlim = c(0, max(df.summary$wealth)),
                    ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(piecewise)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}