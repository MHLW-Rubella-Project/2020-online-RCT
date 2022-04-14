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
#' //NOTE: 検定力分析
#' --->
#'
#+ power, eval = FALSE
min_int_ef1 <- NULL
for (i in LETTERS[1:7]) {
  res <- int$
    power(
      subset = coupon2019 == 1,
      alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
    )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_int_ef1)) min_int_ef1 <- min_diff_mean
  if (min_diff_mean <= min_int_ef1) min_int_ef1 <- min_diff_mean
}

min_act_ef1 <- NULL
for (i in LETTERS[1:7]) {
  res <- act$
    power(
      subset = coupon2019 == 1,
      alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
    )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef1)) min_act_ef1 <- min_diff_mean
  if (min_diff_mean <= min_act_ef1) min_act_ef1 <- min_diff_mean
}

min_int_ef0 <- NULL
for (i in LETTERS[1:7]) {
  res <- int$
    power(
      subset = coupon2019 == 0,
      alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
    )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_int_ef0)) min_int_ef0 <- min_diff_mean
  if (min_diff_mean <= min_int_ef0) min_int_ef0 <- min_diff_mean
}

min_act_ef0 <- NULL
for (i in LETTERS[1:7]) {
  res <- act$
    power(
      subset = coupon2019 == 0,
      alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
    )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef0)) min_act_ef0 <- min_diff_mean
  if (min_diff_mean <= min_act_ef0) min_act_ef0 <- min_diff_mean
}
