library(R6)

MonetaryValue <- R6::R6Class("MonetaryValue",
  public = list(
    initialize = function(data, ttest) {
      wtp <- data %>%
        dplyr::filter(coupon2019 == 1) %>%
        dplyr::filter(exp_antibody == 0) %>%
        group_by(wtp_vaccine) %>%
        summarize(N = n()) %>%
        arrange(desc(wtp_vaccine)) %>%
        mutate(cum_prop = cumsum(N) / sum(N))

      private$demand_func <- with(wtp, approxfun(cum_prop, wtp_vaccine))
      private$stats <- wtp
      private$ttest <- ttest
      private$baseline$prop <- subset(wtp, wtp_vaccine == 0)$cum_prop + 
        subset(ttest, nudge == "MHLW (Control)")$mu / 100
      private$baseline$wtp <- private$demand_func(private$baseline$prop)
    },
    demand_curve = function() {
      baseprop <- private$baseline$prop
      basewtp <- private$baseline$wtp

      private$stats %>%
        ggplot(aes(x = cum_prop, y = wtp_vaccine)) +
          geom_point(aes(x = baseprop, y = basewtp), size = 3, shape = 17) +
          geom_point(size = 3) +
          geom_line() +
          geom_segment(
            aes(x = -Inf, xend = baseprop, y = basewtp, yend = basewtp),
            linetype = 3
          ) +
          geom_segment(
            aes(x = baseprop, xend = baseprop, y = -Inf, yend = basewtp),
            linetype = 3
          ) +
          scale_y_continuous(breaks = seq(-6000, 5000, by = 1000)) +
          scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
          labs(
            y = "WTP (Price of vaccination=JPY 5,000)",
            x = "Cumulative fraction"
          ) +
          theme_classic(base_size = 15)
    },
    value_table = function(title = "", notes = "") {
      tbl <- private$ttest %>%
        ungroup() %>%
        select(nudge, mu) %>%
        mutate(
          effect = (mu - mu[1]) / 100,
          prop = effect + private$baseline$prop,
          wtp = abs(private$demand_func(prop)) - abs(private$baseline$wtp),
          total = (646 - 117) * wtp / 100000,
          wtp_usd = wtp / 100,
          total_usd = (total / 110) * 1000
        ) %>%
        select(-mu) %>%
        dplyr::filter(nudge != "MHLW (Control)")

      tbl %>%
        knitr::kable(
          caption = title,
          col.names = c(
            "Text messages", "Effect", "Baseline + effect",
            "pp", "total", "pp", "total"
          ),
          digits = 3,
          align = "lcccccc",
          booktabs = TRUE,
          linesep = ""
        ) %>%
        kable_styling(font_size = 9) %>%
        add_header_above(c(" " = 3, "Monetary value (JPY)" = 2, "Monetary value (USD)" = 2)) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    ttest = NULL,
    stats = NULL,
    demand_func = NULL,
    baseline = list()
  )
)