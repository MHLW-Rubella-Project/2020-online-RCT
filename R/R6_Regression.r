library(R6)
library(estimatr)

Regression <- R6::R6Class("Regression",
  public = list(
    reg = NULL,
    data = NULL,
    initialize = function(data, covariate, treat_labels) {
      exclude_A <- !("A" %in% unique(data$nudge))

      mod <- list(
        "(1)" = reformulate("nudge * I(1 - coupon2019)", "outcome_test"),
        "(2)" = reformulate(c("nudge * I(1 - coupon2019)", covariate), "outcome_test"),
        "(3)" = reformulate("nudge * I(1 - coupon2019)", "outcome_vacc"),
        "(4)" = reformulate(c("nudge * I(1 - coupon2019)", covariate), "outcome_vacc")
      )

      hypo <- c(
        "nudgeB:I(1 - coupon2019) + nudgeB",
        "nudgeC:I(1 - coupon2019) + nudgeC",
        "nudgeD:I(1 - coupon2019) + nudgeD",
        "nudgeE:I(1 - coupon2019) + nudgeE",
        "nudgeF:I(1 - coupon2019) + nudgeF",
        "nudgeG:I(1 - coupon2019) + nudgeG"
      )

      if (exclude_A) hypo <- hypo[-1]

      self$reg <- mod %>%
        map(~ lh_robust(., data = data, se_type = "stata", linear_hypothesis = hypo))

      attr(self$reg, "exclude_A") <- exclude_A 

      self$data <- data
      private$hypo <- hypo
      private$treat_labels <- treat_labels
    },
    reg_tab = function(title = "", notes = "") {
      main <- c(private$treat_labels[-1], "Opt-in")
      names(main) <- c(paste0("nudge", LETTERS[2:7]), "I(1 - coupon2019)")
      interaction <- paste(private$treat_labels[-1], "$\\times$ Opt-in")
      names(interaction) <- paste0("nudge", LETTERS[2:7], ":I(1 - coupon2019)")

      addtab <- tribble(
        ~term, ~mod1, ~mod2, ~mod3, ~mod4,
        "Covariates", "", "X", "", "X"
      )

      len <- 2 * (length(main) + length(interaction))
      len <- if (attr(self$reg, "exclude_A")) len - 4 else len
      attr(addtab, "position") <- c(len + 1, len + 2)

      self$reg %>%
        map(~ .$lm_robust) %>%
        modelsummary(
          title = title,
          coef_map = c(main, interaction),
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          gof_omit = "R2 Adj.|AIC|BIC|RMSE",
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = 9) %>%
        add_header_above(c(" " = 1, "Testing" = 2, "Vaccination" = 2)) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = TRUE
        )
    },
    lh_tab = function(title = "", notes = "") {
      coef_map <- private$treat_labels[-1]
      names(coef_map) <- private$hypo

      addtab <- tribble(
        ~term, ~mod1, ~mod2, ~mod3, ~mod4,
        "Covariates", "", "X", "", "X"
      )

      self$reg %>%
        map(~ .$lh) %>%
        modelsummary(
          title = title,
          coef_map = coef_map,
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          gof_omit = "Num|Rows|Columns|Share",
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kable_styling(font_size = 9) %>%
        add_header_above(c(" " = 1, "Testing" = 2, "Vaccination" = 2)) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = TRUE
        )
    }
  ),
  private = list(
    treat_labels = c(),
    hypo = c()
  )
)