library(R6)
library(estimatr)

EstimateEffect <- R6::R6Class("EstimateEffect",
  public = list(
    wave1 = NULL,
    wave2 = NULL,
    initialize = function(wave1, wave2, treat_labels, control) {
      self$wave1 <- wave1
      self$wave2 <- wave2
      private$treat_labels <- treat_labels
      private$covs <- control
    },
    balance_control = function(
      outcome_intention = TRUE,
      default_voucher = TRUE,
      title = "",
      notes = ""
    ) {
      use <- private$use_data(outcome_intention, default_voucher)

      use_covs <- use %>%
        summarise_at(private$covs, list(~ sum(is.na(.)))) %>%
        pivot_longer(everything()) %>%
        dplyr::filter(value == 0) %>%
        .$name

      mu_table <- use %>%
        group_by(nudge) %>%
        summarize_at(use_covs, list(~ mean(.))) %>%
        select(-coupon2019) %>%
        mutate(nudge = factor(nudge, labels = private$treat_labels)) %>%
        pivot_longer(-nudge) %>%
        pivot_wider(names_from = nudge, values_from = value)

      p <- mu_table$name %>%
        sapply(function(x) {
          mod <- reformulate("nudge", x)
          est <- lm_robust(mod, data = use)
          f <- est$fstatistic
          pf(f[1], f[2], f[3], lower.tail = FALSE)
        })

      p_table <- tibble(
        name = str_remove(names(p), ".value"),
        "F-test, p-value" = p,
      )

      tbl <- mu_table %>%
        left_join(p_table, by = "name")

      tbl %>%
        knitr::kable(
          caption = title,
          digits = 3,
          col.names = c("", colnames(tbl)[-1]),
          align = "lcccccccc"
        ) %>%
        kableExtra::kable_styling(
          font_size = 9,
          latex_options = "hold_position"
        ) %>%
        kableExtra::column_spec(2:8, width = "3em") %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    power = function(
      outcome_intention = TRUE,
      default_voucher = TRUE,
      alpha = 0.05,
      power = 0.8
    ) {
      use <- private$use_data(outcome_intention, default_voucher)
      obs <- with(use, table(nudge))

      LETTERS[2:7] %>%
        sapply(function(i) {
          uniroot(
            private$diff_power,
            c(0, 10),
            n0 = obs["A"],
            n1 = obs[i],
            alpha = alpha,
            power = power
          )$root
        })
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c(),
    use_data = function(outcome_intention = TRUE, default_voucher = TRUE) {
      val_coupon2019 <- ifelse(default_voucher, 1, 0)
      dta <- if (outcome_intention) self$wave1 else self$wave2
      subset(dta, coupon2019 == val_coupon2019)
    },
    diff_power = function(n0, n1, d, alpha, power) {
      delta <- d / sqrt(1 / n1 + 1 / n0)
      df <- n1 + n0 - 2
      critical <- qt(alpha / 2, df = df)
      calculated_power <- pt(-critical, df = df, ncp = delta, lower.tail = FALSE) +
        pt(critical, df = df)
      specified_power <- power
      specified_power - calculated_power
    }
  )
)

a <- test$main_analysis()
a$balance_control()
a$power(outcome_intention = TRUE, default_voucher = FALSE)