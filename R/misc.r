# //NOTE: Difference between specified and calculated power for power analysis
diff_power <- function(n0, n1, d, alpha, power) {
  delta <- d / sqrt(1 / n1 + 1 / n0)
  df <- n1 + n0 - 2
  critical <- qt(alpha / 2, df = df)
  calculated_power <- pt(-critical, df = df, ncp = delta, lower.tail = FALSE) +
    pt(critical, df = df)
  specified_power <- power
  specified_power - calculated_power
}

# //NOTE: Standard error
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x) / length(x))
}

# //NOTE: Template for ggplot
simplegg <- function(
    title_size = 13,
    axis_title_size = 13,
    axis_text_size = 12,
    legend_title_size = 12,
    legend_text_size = 12,
    caption_size = 11,
    legend_key_size = 1,
    font_family = NULL,
    flip = FALSE) {
  my_theme <- ggplot2::theme_minimal(base_family = font_family) +
    ggplot2::theme(
      # panel grid
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      # axis
      axis.text = ggplot2::element_text(
        size = axis_text_size, family = font_family
      ),
      axis.title = ggplot2::element_text(
        size = axis_title_size, family = font_family
      ),
      axis.ticks.length = grid::unit(0.25, "cm"),
      axis.ticks.x = ggplot2::element_line(),
      axis.ticks.y = ggplot2::element_line(),
      axis.line = ggplot2::element_line(),
      # legend
      legend.text = ggplot2::element_text(
        size = legend_text_size, family = font_family
      ),
      legend.key.size = grid::unit(legend_key_size, "cm"),
      legend.title = ggplot2::element_text(
        size = legend_title_size, family = font_family
      ),
      legend.position = "bottom",
      # facet_wrap
      strip.text = ggplot2::element_text(
        size = axis_title_size, family = font_family
      ),
      # caption
      plot.caption = ggplot2::element_text(
        size = caption_size, family = font_family
      )
    )

  if (flip) {
    my_theme <- my_theme +
      theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line()
      )
  }

  return(my_theme)
}