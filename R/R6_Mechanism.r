library(R6)

Mechanism <- R6::R6Class("Mechanism",
  public = list(
    reg = NULL,
    data = NULL,
    initialize = function(data, covariate, treat_labels) {
      use <- data %>%
        dplyr::filter(nudge != "A") %>%
        mutate(
          nudgeC = if_else(nudge == "C", 1, 0),
          nudge_other = if_else(nudge %in% c("D", "E", "F", "G"), 1, 0)
        )
      
      models <- mod_list <- list(
        "(1)" = list(
          mod = reformulate(c("nudgeC*handicap", "nudge_other", covariate), "outcome_test"),
          lh = "nudgeC + nudgeC:handicap"
        ),
        "(2)" = list(
          mod = reformulate(c("nudgeC*generosity", "nudge_other", covariate), "outcome_test"),
          lh = "nudgeC + 5 * nudgeC:generosity"
        )
      )

      est <- models %>%
        map(~ lh_robust(
          .$mod,
          data = use,
          se_type = "stata",
          linear_hypothesis = .$lh
        ))
      
      self$data <- use
      self$reg <- est
      private$treat_labels <- treat_labels
    },
    reg_tab = function( title = "",
                        outcome_label = "Intention for testing",
                        notes = ""
    ) {
      label_c <- private$treat_labels[3]
      coef_label <- c(
        "nudgeC" = label_c,
        "nudgeC:handicap" = paste(label_c, "$\\times$ Handicap"),
        "nudgeC:generosity" = paste(label_c, "$\\times$ Generosity"),
        "nudge_other" = "Other nudges",
        "nudgeC + nudgeC:handicap" = paste(label_c, "+", label_c, "$\\times$ Handicap"),
        "nudgeC + 5 * nudgeC:generosity" = paste(label_c, "+", label_c, "$\\times$ Most generous")
      )

      addtab <- tribble(
        ~term, ~mod1, ~mod2,
        "Covariates", "X", "X"
      )
      attr(addtab, "position") <- 13

      tbl <- self$reg %>%
        modelsummary(
          title = title,
          coef_map = coef_label,
          stars = c("***" = 0.01, "**" = 0.05, "*" = 0.1),
          gof_omit = "R2 Adj.|AIC|BIC|RMSE|Std.Errors",
          add_rows = addtab,
          escape = FALSE
        ) %>%
        kableExtra::kable_styling(font_size = 9)
      
      header <- c(1, 2)
      names(header) <- c(" ", outcome_label)

      tbl %>%
        kableExtra::add_header_above(header) %>%
        kableExtra::pack_rows("Linear combination test", 9, 12) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    treat_labels = c()
  )
)