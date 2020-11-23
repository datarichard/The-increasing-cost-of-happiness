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
    summarise(n = n()) %>%
    mutate(note = paste("n =", format(n, big.mark = ","))) -> df.sum
  
  df %>%
    group_by(year) %>%
    mutate(
      dollar_bin = ntile(dollars, 10)
    ) %>%
    group_by(year, dollar_bin) %>%
    summarize(
      wellbeing = mean(swb, na.rm=T),
      wealth = mean(dollars, na.rm=T)/1000
      ) -> df.deciles
  
  df %>%
    mutate(
      wealth = dollars/1000,
      wellbeing = swb
    ) -> df.plot
  
  # Linear model
  ggplot(df.plot, aes(x = wealth, y = wellbeing)) +
    geom_smooth(aes(fill = "manual1", color = "manual2"),
      method = "lm", 
      size = 0.5
      ) +
    coord_cartesian(xlim = c(0, max(df.deciles$wealth)),
                    ylim = c(min(df.deciles$wellbeing), max(df.deciles$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(linear)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test() +
    theme(
      legend.position = "none") -> p1
  
  p1 +
    geom_point(data = df.deciles) + # size = 3, alpha = 0.25
    geom_text(data = df.sum, aes(x = Inf, y = -Inf, label = note),
              hjust = 1.1, vjust = -.75, colour = 1, size = 3)
  
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
    group_by(year) %>%
    summarise(n = n()) %>%
    mutate(note = paste("n =", format(n, big.mark = ","))) -> df.sum
  
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
      wealth = mean(dollars, na.rm=T)/1000
      ) -> df.deciles
  
  # piecewise model
  ggplot(df.deciles, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_text(data = df.sum, aes(x = Inf, y = -Inf, label = note),
              hjust = 1.1, vjust = -.75, colour = 1, size = 3) +
    geom_smooth(data = df.plot,
                aes(x = wealth, y = wellbeing,
                fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1),
                size = 0.5) +
    coord_cartesian(xlim = c(0, max(df.deciles$wealth)),
                    ylim = c(min(df.deciles$wellbeing), max(df.deciles$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(piecewise)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test() +
    theme(legend.position = "none")
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
    group_by(year) %>%
    summarise(n = n()) %>%
    mutate(note = paste("n =", format(n, big.mark = ","))) -> df.sum
  
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
    ) -> df.deciles
  
  # piecewise model
  ggplot(df.deciles, aes(x = wealth, y = wellbeing)) +
    geom_point() +
    geom_text(data = df.sum, aes(x = Inf, y = -Inf, label = note),
              hjust = 1.1, vjust = -.75, colour = 1, size = 3) +
    geom_smooth(data = filter(df.plot, year == 2002),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[1]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2006),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[2]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2010),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[3]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2014),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[4]),
                size = 0.5) +
    geom_smooth(data = filter(df.plot, year == 2018),
                aes(x = wealth, y = wellbeing,
                    fill = "manual1", color = "manual2"),
                method = lm, 
                formula = y ~ splines::bs(x, df = 2, degree = 1, knots = knots[5]),
                size = 0.5) +
    coord_cartesian(xlim = c(0, max(df.deciles$wealth)),
                    ylim = c(min(df.deciles$wellbeing), max(df.deciles$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(piecewise)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test() +
    theme(legend.position = "none")
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
    # coord_cartesian(xlim = c(0, max(df.summary$wealth)),
    #                 ylim = c(min(df.summary$wellbeing), max(df.summary$wellbeing))) +
    facet_wrap(~year, nrow = 1) +
    labs(subtitle = paste(rlang::as_name(swb_colname), "(b-spline with knots = 4, 5, 6)"),
         x = paste(rlang::as_name(dollars_colname), "($000s)"),
         y = "") +
    theme_test()
}



