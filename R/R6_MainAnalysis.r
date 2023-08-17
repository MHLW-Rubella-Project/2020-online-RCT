library(R6)

MainAnalysis <- R6::R6Class("MainAnalysis",
  public = list(
    wave1 = NULL,
    wave2 = NULL,
    initialize = function(data, treat_labels, control) {
      dt <- data %>%
        mutate(
          aw1_negative = if_else(act_vaccine != 4, 1, 0),
          aw1_test_negative = aw1_test * aw1_negative,
          coupon_a = coupon2019 * as.numeric(nudge == "A"),
          coupon_b = coupon2019 * as.numeric(nudge == "B"),
          coupon_c = coupon2019 * as.numeric(nudge == "C"),
          coupon_d = coupon2019 * as.numeric(nudge == "D"),
          coupon_e = coupon2019 * as.numeric(nudge == "E"),
          coupon_f = coupon2019 * as.numeric(nudge == "F"),
          coupon_g = coupon2019 * as.numeric(nudge == "G")
        )
      
      self$wave1 <- dt %>%
        filter(40 <= age & age <= 56) %>%
        filter(exp_antibody != 1 & exp_vaccine != 1)
      
      self$wave2 <- dt %>%
        filter(40 <= age & age <= 56) %>%
        filter(follow == 1) %>%
        filter(
          exp_antibody != 1 &
          exp_vaccine != 1 &
          act_test != 2 &
          act_vaccine != 2
        )
      
      private$treat_labels <- treat_labels
      private$covs <- control
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c()
  )
)