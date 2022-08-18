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
      "Balance Test for Default Incentive Group in First Wave Study Sample",
      "\\label{tab:int-coupon1-balance}"
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
      "Note: We use men aged 40-46,",
      "who automatically received the free vouchers in FY2019,",
      "in the first wave study sample.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ int-coupon0-balance
out.file <- file(here("tables", "int-coupon0-balance.tex"), open = "w")

tab <- int$
  balance(subset = coupon2019 == 0)$
  table(
    title = paste(
      "Balance Test for Opt-in Incentive Group in First Wave Study Sample",
      "\\label{tab:int-coupon0-balance}"
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
      "Note: We use men aged 47-57,",
      "who needed costly procedures to get the free vouchers in FY2019,",
      "in the first wave study sample.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ act-coupon1-balance
out.file <- file(here("tables", "act-coupon1-balance.tex"), open = "w")

tab <- act$
  balance(subset = coupon2019 == 1)$
  table(
    title = paste(
      "Balance Test for Default Incentive Group in Second Wave Study Sample",
      "\\label{tab:act-coupon1-balance}"
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
      "Note: We use men aged 40-46,",
      "who automatically received the free vouchers in FY2019,",
      "in the second wave study sample.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#+ act-coupon0-balance
out.file <- file(here("tables", "act-coupon0-balance.tex"), open = "w")

tab <- act$
  balance(subset = coupon2019 == 0)$
  table(
    title = paste(
      "Balance Test for Opt-in Incentive Group in Second Wave Study Sample",
      "\\label{tab:act-coupon0-balance}"
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
      "Note: We use men aged 47-57,",
      "who needed costly procedures to get the free vouchers in FY2019,",
      "in the second wave study sample.",
      "We show sample average of each variable for each experimental arm",
      "in columns 2 through 8.",
      "We show the p-value of F-test for joint null hypothesis in column 9."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)
