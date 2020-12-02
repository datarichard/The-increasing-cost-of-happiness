#' ggplot2 theme for FT graphs.
#' @param legend_right Logical indicating whether legend should be placed to
#' the right of the plot. If FALSE, the default, legend is positioned above the
#' plot.
#' @param base_size The base font size
#' @param base_family Font family
#' @param base_line_size Default
#' @param base_rect_size Default
#' @importFrom ggplot2 %+replace%
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(displ, hwy, color = class)) +
#'   geom_point() +
#'   ft_theme()
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   facet_wrap(vars(class)) +
#'   ft_theme()
#'
ft_theme <- function(legend_right = FALSE,
                     base_size = 12,
                     base_family = "",
                     base_line_size = base_size / 170,
                     base_rect_size = base_size / 170) {
  
  half_line <- base_size/2
  grid_line_color <- ft_colors("black-20")
  grid_line_size <- 0.2
  title_text_color <- ft_colors("black")
  other_text_color <- ft_colors("black-70")
  
  if(legend_right == TRUE){
    spec_legend_position <- "right"
    spec_legend_direction <- "vertical"
    legend_justification_spec <- "center"
    legend_box_spacing_spec = ggplot2::unit(2 * half_line, "pt")
  } else {
    spec_legend_position <- "top"
    spec_legend_direction <- "horizontal"
    legend_justification_spec <- c(0,0)
    legend_box_spacing_spec <- ggplot2::unit(0, "char")
  }
  
  ggplot2::theme_minimal(base_size = base_size,
                         base_family = base_family,
                         base_line_size = base_line_size) %+replace%
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        color = title_text_color,
        size = ggplot2::rel(1.2),
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.subtitle = ggplot2::element_text(
        color = other_text_color,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = half_line)
      ),
      plot.caption = ggplot2::element_text(
        color = other_text_color,
        hjust = 0,
        size = ggplot2::rel(0.8),
        margin = margin(t = half_line)
      ),
      axis.title = ggplot2::element_text(
        color = other_text_color,
        size = ggplot2::rel(0.9),
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        color = other_text_color,
        size = ggplot2::rel(0.8),
        margin = ggplot2::margin()
      ),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = -0.8 * half_line / 2), hjust = 1),
      axis.line = ggplot2::element_line(
        colour = grid_line_color,
        size = grid_line_size
      ),
      axis.line.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0.5,"char"),
      panel.grid.major.y = ggplot2::element_line(
        color = grid_line_color,
        size = grid_line_size
      ),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = spec_legend_position,
      legend.justification = legend_justification_spec,
      legend.direction = spec_legend_direction,
      legend.title = ggplot2::element_text(hjust = 0,
                                           color = other_text_color,
                                           size = ggplot2::rel(0.9),
                                           face = "bold"),
      legend.spacing.x = ggplot2::unit(1, "char"),
      legend.text = ggplot2::element_text(
        color = other_text_color,
        hjust = 0,
        size = ggplot2::rel(0.8)
      ),
      legend.margin = ggplot2::margin(),
      legend.box.spacing = legend_box_spacing_spec,
      plot.margin = ggplot2::margin(1,1,1,1, unit = "char"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      complete = TRUE
    )
}

# FT Origami Colours
# https://registry.origami.ft.com/components/o-colors@4.9.0
ft_o_colors <- c(
  paper = "#FFF1E5",
  black = "#000000",
  white = "#FFFFFF",
  claret = "#990F3D",
  oxford = "#0F5499",
  teal = "#0D7680",
  wheat = "#F2DFCE",
  sky = "#CCE6FF",
  slate = "#262A33",
  velvet = "#593380",
  mandarin = "#FF8833",
  lemon = "#FFEC1A",
  jade = "#00994D",
  wasabi = "#96CC28",
  crimson = "#CC0000",
  candy = "#FF7FAA",
  `black-5` = "#F2E5DA",
  `black-10` = "#E6D9CE",
  `black-20` = "#CCC1B7",
  `black-30` = "#B3A9A0",
  `black-40` = "#999189",
  `black-50` = "#807973",
  `black-60` = "#66605C",
  `black-70` = "#4D4845",
  `black-80` = "#33302E",
  `black-90` = "#1A1817",
  `white-10` = "#FFF2E8",
  `white-20` = "#FFF4EA",
  `white-40` = "#FFF7EF",
  `white-60` = "#FFF9F5",
  `white-80` = "#FFFCFA",
  `claret-30` = "#4D081F",
  `claret-40` = "#660A29",
  `claret-50` = "#800D33",
  `claret-60` = "#990F3D",
  `claret-70` = "#B31247",
  `claret-80` = "#CC1452",
  `claret-90` = "#E6175C",
  `claret-100` = "#FF1A66",
  `oxford-30` = "#082A4D",
  `oxford-40` = "#0A3866",
  `oxford-50` = "#0D4680",
  `oxford-60` = "#0F5499",
  `oxford-70` = "#1262B3",
  `oxford-80` = "#1470CC",
  `oxford-90` = "#177EE6",
  `oxford-100` = "#1A8CFF",
  `teal-20` = "#052F33",
  `teal-30` = "#08474D",
  `teal-40` = "#0A5E66",
  `teal-50` = "#0D7680",
  `teal-60` = "#0F8E99",
  `teal-70` = "#12A5B3",
  `teal-80` = "#14BDCC",
  `teal-90` = "#17D4E6",
  `teal-100` = "#1AECFF"
)

ft_colors <- function(...){
  #' FT Colors
  #'
  #' Get hex codes for ft brand colors.
  #' @param ... Names of the colors
  #' @details Use \code{ft_colors()} to see the full list of colours. This list
  #'   comes from
  #'   \href{https://registry.origami.ft.com/components/o-colors}{Origami
  #'   o-colors}.
  #' @examples
  #' # Full list
  #' ft_colors()
  #'
  #' # Choose colors
  #' ft_colors("paper")
  #' ft_colors("oxford", "claret")
  #'
  #' @export
  
  cols <- c(...)
  
  if(is.null(cols)){
    return(ft_o_colors)
  }
  
  not_found <- which(!(cols %in%  names(ft_o_colors)))
  if(length(not_found) > 0){
    warning(paste0("Could not find colors ", paste0(cols[not_found], collapse = ", "), ". Returned NAs instead.\n"))
  }
  
  unname(ft_o_colors[cols])
}


# FT Origami Palettes
ft_o_palettes <- list(
  `main` = ft_colors("oxford","teal-90","wasabi","candy"),
  `origami primary` = ft_colors("paper", "black", "white", "claret", "oxford", "teal"),
  `origami secondary` = ft_colors("wheat","sky","slate","velvet","mandarin","lemon"),
  `origami tertiary` = ft_colors("jade","wasabi","crimson","candy"),
  `black` = ft_colors("black-5","black-10","black-20","black-30","black-40","black-50","black-60","black-70","black-80","black-90"),
  `white` = ft_colors("white-10","white-20","white-40","white-60","white-80"),
  `claret` = ft_colors("claret-30","claret-40","claret-50","claret-60","claret-70","claret-80","claret-90","claret-100"),
  `oxford` = ft_colors("oxford-30","oxford-40","oxford-50","oxford-60","oxford-70","oxford-80","oxford-90","oxford-100"),
  `teal` = ft_colors("teal-20","teal-30","teal-40","teal-50","teal-60","teal-70","teal-80","teal-90","teal-100")
)


#' FT Colour Palettes Generator.
#'
#' Returns a function that can interplotate between colors in a palette.
#'
#' @param palette Character name of a palette
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
ft_pal <- function(palette = "main", reverse = FALSE, ...){
  
  assertthat::assert_that(palette %in% names(ft_o_palettes),
                          msg = paste0("Palette not found. Please use one of: ", paste0(names(ft_o_palettes), collapse = ", ")))
  
  pal <- ft_o_palettes[[palette]]
  
  if(reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)
}