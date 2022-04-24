#' ---
#' title: |
#'   Analysis to Address Recall Bias Associated with Self-Reporting of Behavior:
#'   Balance Test
#' subtitle: Preview
#' author: Hiroki Kato
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#' params:
#'   preview: true
#' ---
#'
#+ include = FALSE
library(here)
source(here("src/_common.r"), encoding = "utf8")

#+ include = FALSE, eval = params$preview
source(here("src/_html_header.r"), encoding = "utf8")

#+ include = FALSE
wave22 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 2)

covmod <- ~ age + married + education +
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
act2$
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of New Wave 2 Selection Data",
    "(Men who automatically received coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

#+ act2-coupon0-balance
act2$
  balance(subset = coupon2019 == 0)$
  table(title = paste(
    "Balance Test of New Wave 2 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  )) %>%
  kableExtra::kable_styling(latex_options = "hold_position") %>%
  kableExtra::column_spec(2:8, width = "3em")

#' ここでは、第2回調査の抗体検査の受検行動やワクチン接種行動の回答に
#' 想起バイアスが伴うことを考慮した分析を行う。
#' 第2回調査はそれぞれの行動を第1回調査以前に行ったかどうかを調査している。
#' この時期の回答に想起バイアスが伴うならば、
#' 本論の分析のように第2回調査で第1回調査以前に行動したと回答した人を除くべきではない。
#' そこで、我々は第2回調査の行動の回答に想起バイアスが伴うことを仮定して、
#' 第1回調査の調査ですでに抗体検査もしくはワクチン接種を受けた男性だけを除いて、
#' ナッジ・メッセージの行動に対する効果を推定する（wave 1 selection dataと同じ基準）。
#' したがって、
#' 第2回調査で第1回調査以前に抗体検査もしくはワクチン接種を受けたと回答した人はサンプルに含まれている。
#'
#' 本論と同様に、
#' 我々は2019年度にクーポン券を自動的に受け取っているかどうかでサンプルを分割して、
#' サブサンプルを用いてナッジ・メッセージの効果を推定する。
#' 表\@ref(tab:act2-coupon1-balance)と
#' 表\@ref(tab:act2-coupon0-balance)は共変量のバランステストの結果であり、
#' 回答者の観察可能な特徴は群間でシステマティックに異ならないことを示している。
#'
#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ act2-power, results = "asis"
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

cat(c(
  "検定力80%・有意水準5%を保つために必要な効果の規模を計算したところ、",
  "2019年度にクーポン券が自動で送付される男性のサブサンプルを用いる場合、少なくとも",
  round(min_act_ef1 * 100, 1),
  "%ポイントの差が必要である。",
  "2019年度ではクーポン券を受け取るために手続きが必要な男性のサブサンプルを用いる場合、少なくとも",
  round(min_act_ef0 * 100, 1),
  "%ポイントの差が必要である。"
), sep = "\n")

#'
#' アウトカム変数の定義も本論のものから変更する。
#' 本論では、第1回調査以降に抗体検査を受検したら1を取るアウトカム変数と
#' 第1回調査以降に抗体検査を受検し、ワクチンによって抗体を新たに獲得したら1を取るダミー変数でを用いた。
#' 対して、この補論では、
#' 第2回調査で時期に関わらず抗体検査を受検したと回答したら1を取るダミー変数と
#' 第2回調査で時期に関わらず抗体検査を受検し、
#' 時期に関わらずワクチンによって抗体を獲得したら1を取るダミー変数である。
#'
# /*
#+
rmarkdown::render(
  "src/7-selection2-balance-test.r",
  output_dir = "report/view",
  knit_root_dir = here::here()
)
# */