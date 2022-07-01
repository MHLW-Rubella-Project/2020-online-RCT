#' ---
#' title: Results of Balance Test
#' subtitle: Preview
#' output:
#'   bookdown::html_document2:
#'     toc: yes
#'     toc_float: yes
#'     number_sections: false
#' params:
#'   preview: yes
#'   appendix: yes
#' ---
#'
#+ include = FALSE
library(here)
source(here("src/_common.r"), encoding = "utf8")

#+ include = FALSE, eval = params$preview
source(here("src/_html_header.r"), encoding = "utf8")

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

#' <!---
#' //NOTE: バランステスト
#' --->
#'
#' 共変量のバランステストとして、各共変量の線形モデルを推定した。
#' このモデルの説明変数は介入群ダミーであり、厚労省メッセージ群を参照群とした。
#' 我々は推定された線形モデルの係数すべてがゼロであるという帰無仮説をF検定によって検証した。
#' そのp値を表の最右列に示した。
#' 表\@ref(tab:int-coupon1-balance)は2019年度にクーポン券を自動的に受け取った人に限定した
#' wave 1 selection dataのバランステストの結果である。
#' 表\@ref(tab:int-coupon0-balance)は
#' 2019年度にクーポン券を受け取るためにコストのかかる手続きが必要な人に限定した
#' wave 1 selection dataのバランステストの結果である。
#' 表\@ref(tab:act-coupon1-balance)は2019年度にクーポン券を自動的に受け取った人に限定した
#' wave 2 selection dataのバランステストの結果である。
#' 表\@ref(tab:act-coupon0-balance)は
#' 2019年度にクーポン券を受け取るためにコストのかかる手続きが必要な人に限定した
#' wave 2 selection dataのバランステストの結果である。
#'
#' \clearpage
#'
#'
#+ int-coupon1-balance
int$
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of Wave 1 Selection Data",
    "(Men who automatically received coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

#+ int-coupon0-balance
int$
  balance(subset = coupon2019 == 0)$
  table(title = paste(
    "Balance Test of Wave 1 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

#+ act-coupon1-balance
act$
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of Wave 2 Selection Data",
    "(Men who automatically received coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

#+ act-coupon0-balance
act$
  balance(subset = coupon2019 == 0)$
  table(title = paste(
    "Balance Test of Wave 2 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

# /*
#+
rmarkdown::render(
  here("src/3-selection1-balance-test.r"),
  output_dir = here("report/view")
)
# */