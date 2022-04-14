#+ include = FALSE
library(here)
source(here("code/_common.r"), encoding = "utf8")

#+ include = FALSE
wave1 <- rct_data_wave1(here(rct_path, "shape_survey.csv"))
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

#+ include = FALSE
covmod <- ~ age + married + education +
  exercise_w1 + health_check + flushot +
  prob_social + handicap + severity +
  handwash + temp_check + avoid_out + avoid_crowd + wear_mask

int <- create_RCTtoolbox(
  test_int + vaccine_int ~ nudge,
  covmod,
  data = wave1,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

act <- create_RCTtoolbox(
  aw1_test + aw1_testvaccine ~ nudge,
  covmod,
  data = wave2,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#' <!---
#' //NOTE: バランステスト
#' --->
#'
#+ int-coupon1-balance, eval = FALSE
int$
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of Wave 1 Selection Data",
    "(Men who automatically received coupon in 2019)"
  ))

#+ int-coupon0-balance, eval = FALSE
int$
  balance(subset = coupon2019 == 0)$
  table(title = paste(
    "Balance Test of Wave 1 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  ))

#+ act-coupon1-balance, eval = FALSE
act$
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of Wave 2 Selection Data",
    "(Men who automatically received coupon in 2019)"
  ))

#+ act-coupon0-balance, eval = FALSE
act$
  balance(subset = coupon2019 == 0)$
  table(title = paste(
    "Balance Test of Wave 2 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  ))
