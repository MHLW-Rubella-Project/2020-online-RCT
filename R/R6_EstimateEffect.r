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
      outcome_int = TRUE,
      default_voucher = FALSE,
      title = "",
      notes = ""
    ) {
      val_coupon2019 <- ifelse(default_voucher, 1, 0)
      dta <- if (outcome_int) self$wave1 else self$wave2
      use <- subset(dta, coupon2019 == val_coupon2019)

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
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c()
  )
)

a <- test$main_analysis()
a$balance_control()