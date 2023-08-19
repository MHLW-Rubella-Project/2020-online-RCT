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
        subset(ttest, outcome == "A. Antibody Testing (Behavior)" & nudge == "MHLW (Control)")$mu
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
          scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
          labs(
            y = "WTP (Price of vaccination=JPY 5,000)",
            x = "Cumulative fraction"
          ) +
          simplegg(axis_text_size = 12)
    },
    value_table = function(title = "", notes = "") {
      tbl <- private$ttest %>%
        dplyr::filter(outcome == "A. Antibody Testing (Behavior)") %>%
        select(-outcome, -se, -p, -label) %>%
        mutate(
          effect = mu - mu[1],
          prop = effect + private$baseline$prop,
          wtp = abs(demand_func(prop)) - abs(private$baseline$wtp),
          total = (646 - 117) * wtp / 100000,
          wtp_usd = wtp / 100,
          total_usd = (total / 110) * 1000
        ) %>%
        select(-mu) %>%
        dplyr::filter(nudge != "MHLW (Control)")

      tbl %>%
        knitr::kable(
          title = title,
          col.names = c(
            "Text messages", "Effect", "Baseline + effect",
            "pp", "total", "pp", "total"
          ),
          digits = 3,
          align = "lcccccc"
        ) %>%
        kable_styling(font_size = 9, latex_options = "scale_down") %>%
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