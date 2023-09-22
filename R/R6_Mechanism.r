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
          mod = reformulate(c("nudgeC*handicap", "nudge_other", ctrl), "outcome_test"),
          lh = "nudgeC + nudgeC:handicap"
        ),
        "(2)" = list(
          mod = reformulate(c("nudgeC*generosity", "nudge_other", ctrl), "outcome_test"),
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
    }
  ),
  private = list()
)