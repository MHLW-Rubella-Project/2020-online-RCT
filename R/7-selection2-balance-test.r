#' ---
#' title: |
#'   Analysis to Address Recall Bias Associated with Self-Reporting of Behavior:
#'   Balance Test
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave22 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 2)

covmod <- ~ age + married + education +
  income + noinfo_income +
  exercise_w1 + health_check + flushot +
  # prob_social + handicap + severity +
  handwash + temp_check + avoid_out + avoid_crowd + wear_mask

act2 <- create_RCTtoolbox(
  abw1_test + abw1_testvaccine ~ nudge,
  covmod,
  data = wave22,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#+ act2-coupon1-balance
out.file <- file(here("tables", "act2-coupon1-balance.tex"), open = "w")

tab <- act2$
  balance(subset = coupon2019 == 1)$
  table(
    title = paste(
      "Balance Test for Default Incentive Group",
      "\\label{tab:act2-coupon1-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(
    font_size = 9,
    latex_options = "hold_position"
  ) %>%
  kableExtra::column_spec(2:8, width = "3em") %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: We use men aged 40-46",
      "who automatically received the free vouchers in FY2019.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ act2-coupon0-balance
out.file <- file(here("tables", "act2-coupon0-balance.tex"), open = "w")

tab <- act2$
  balance(subset = coupon2019 == 0)$
  table(
    title = paste(
      "Balance Test for Opt-in Incentive Group",
      "\\label{tab:act2-coupon0-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(
    font_size = 9,
    latex_options = "hold_position"
  ) %>%
  kableExtra::column_spec(2:8, width = "3em") %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: We use men aged 47-57",
      "who needed costly procedures to get the free vouchers in FY2019.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ act2-power
min_act_ef1 <- NULL
for (i in LETTERS[1:7]) {
  res <- act2$
    power(
    subset = coupon2019 == 1,
    alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
  )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef1)) min_act_ef1 <- min_diff_mean
  if (min_diff_mean <= min_act_ef1) min_act_ef1 <- min_diff_mean
}

min_act_ef0 <- NULL
for (i in LETTERS[1:7]) {
  res <- act2$
    power(
    subset = coupon2019 == 0,
    alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i
  )$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef0)) min_act_ef0 <- min_diff_mean
  if (min_diff_mean <= min_act_ef0) min_act_ef0 <- min_diff_mean
}

round(min_act_ef1 * 100, 1)
round(min_act_ef0 * 100, 1)
