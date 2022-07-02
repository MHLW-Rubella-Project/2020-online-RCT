#' ---
#' title: Results of Balance Test
#' subtitle: Preview
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave1 <- rct_data_wave1(here(rct_path, "shape_survey.csv"))
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

#+ include = FALSE
int <- create_RCTtoolbox(
  test_int + vaccine_int ~ nudge,
  ~ age + married + education +
    income + noinfo_income +
    exercise_w1 + health_check + flushot, #+
    # prob_social + handicap + severity +
    # handwash + temp_check + avoid_out + avoid_crowd + wear_mask,
  data = wave1,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

act <- create_RCTtoolbox(
  aw1_test + aw1_testvaccine ~ nudge,
  ~ age + married + education +
    income + noinfo_income +
    exercise_w1 + health_check + flushot +
    # prob_social + handicap + severity +
    handwash + temp_check + avoid_out + avoid_crowd + wear_mask,
  data = wave2,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#'
#+ int-coupon1-balance
out.file <- file(here("tables", "int-coupon1-balance.tex"), open = "w")

tab <- int$
  balance(subset = coupon2019 == 1)$
  table(
    title = paste(
      "Balance Test of Wave 1 Selection Data",
      "(Men who automatically received coupon in 2019)",
      "\\label{tab:int-coupon1-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

writeLines(tab, out.file)
close(out.file)

#+ int-coupon0-balance
out.file <- file(here("tables", "int-coupon0-balance.tex"), open = "w")

tab <- int$
  balance(subset = coupon2019 == 0)$
  table(
    title = paste(
      "Balance Test of Wave 1 Selection Data",
      "(Men who need to be processed to receive coupon in 2019)",
      "\\label{tab:int-coupon0-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

writeLines(tab, out.file)
close(out.file)

#+ act-coupon1-balance
out.file <- file(here("tables", "act-coupon1-balance.tex"), open = "w")

tab <- act$
  balance(subset = coupon2019 == 1)$
  table(
    title = paste(
      "Balance Test of Wave 2 Selection Data",
      "(Men who automatically received coupon in 2019)",
      "\\label{tab:act-coupon1-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

writeLines(tab, out.file)
close(out.file)

#+ act-coupon0-balance
out.file <- file(here("tables", "act-coupon0-balance.tex"), open = "w")

tab <- act$
  balance(subset = coupon2019 == 0)$
  table(
    title = paste(
      "Balance Test of Wave 2 Selection Data",
      "(Men who need to be processed to receive coupon in 2019)",
      "\\label{tab:act-coupon0-balance}"
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

writeLines(tab, out.file)
close(out.file)
