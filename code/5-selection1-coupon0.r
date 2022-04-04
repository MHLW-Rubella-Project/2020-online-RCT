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

int_coupon0 <- create_RCTtoolbox(
  test_int + vaccine_int ~ nudge,
  covmod,
  data = subset(wave1, coupon2019 == 0),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

act_coupon0 <- create_RCTtoolbox(
  aw1_test + aw1_testvaccine ~ nudge,
  covmod,
  data = subset(wave2, coupon2019 == 0),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

tester_coupon0 <- create_RCTtoolbox(
  aw1_testnega + aw1_testvaccine ~ nudge,
  data = subset(wave2, coupon2019 == 0 & aw1_test == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#' <!---
#' //NOTE: バランステスト
#' --->
#'
#+ int-coupon0-balance, eval = FALSE
int_coupon0$
  balance()$
  table(
    title = "Wave 1セレクションデータの共変量のバランステスト（2019年度クーポン券配布対外）"
  )

#+ act-coupon0-balance, eval = FALSE
act_coupon0$
  balance()$
  table(
    title = "Wave 1セレクションデータの共変量のバランステスト（2019年度クーポン券配布対象外）"
  )


#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ coupon0-power, include = FALSE
min_int_ef <- NULL
for (i in LETTERS[1:7]) {
  res <- int_coupon0$
    power(alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i)$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_int_ef)) min_int_ef <- min_diff_mean
  if (min_diff_mean <= min_int_ef) min_int_ef <- min_diff_mean
}

min_act_ef <- NULL
for (i in LETTERS[1:7]) {
  res <- act_coupon0$
    power(alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i)$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef)) min_act_ef <- min_diff_mean
  if (min_diff_mean <= min_act_ef) min_act_ef <- min_diff_mean
}

#' <!---
#' //NOTE: 意向に対する効果のt検定
#' --->
#+ int-coupon0-ttest, eval = FALSE
int_coupon0$
  ttest()$
  plot(
    xlab = "介入群",
    ylab = "比率（+/- 標準誤差）",
    title = switch(outcome,
      "test_int" = "A. 抗体検査（意向）",
      "vaccine_int" = "B. ワクチン接種（意向）"),
    inplot_lab_adjust = 0.1,
    flip = TRUE,
    ylim = c(0, 1),
    ncol = 1
  )

#' <!---
#' //NOTE: 行動に対する効果のt検定
#' --->
#'
#+ act-coupon0-ttest, eval = FALSE
act_coupon0$
  ttest()$
  plot(
    xlab = "介入群",
    ylab = "比率（+/- 標準誤差）",
    title = switch(outcome,
      "aw1_test" =
        "A. 抗体検査の受検（第1回調査以降の行動）",
      "aw1_testvaccine" =
        "B. 抗体検査の受検\u00d7ワクチン接種 (第1回調査以降の行動)"),
    inplot_lab_adjust = 0.05,
    flip = TRUE,
    ylim = c(0, 0.3),
    ncol = 1
  )

#' <!---
#' //NOTE: 回帰分析
#' --->
#'
#+ int-coupon0-reg, eval = FALSE
int_coupon0$
  lm(se_type = "HC0")$
  table(
    title = paste(
      "2019年度クーポン券配布対象外の男性に限定した",
      "抗体検査とワクチン接種の意向の線形確率モデルの推定結果"
    ),
    outcome_map = c(
      "test_int" = "抗体検査",
      "vaccine_int" = "ワクチン接種"
    ),
    not_show_x = list(
      "共変量" = all.vars(covmod)
    ),
    footnote = paste(
      "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
      "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
    )
  )

#+ act-coupon0-reg, eval = FALSE
act_coupon0$
  lm(se_type = "HC0")$
  table(
    title = paste(
      "2019年度クーポン券配布対象外の男性に限定した",
      "抗体検査とワクチン接種の行動の線形確率モデルの推定結果"
    ),
    outcome_map = c(
      "aw1_test" = "抗体検査",
      "aw1_testvaccine" = "抗体検査\u00d7ワクチン接種"
    ),
    not_show_x = list(
      "共変量" = all.vars(covmod)
    ),
    footnote = paste(
      "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
      "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
    )
  )

#+ int-coupon0-altreg, eval = FALSE
int_coupon0$
  lm(se_type = "HC0", ctrl = "C")$
  table(
  title = paste(
    "利他強調メッセージと比較した意向に対する介入群の効果",
    "(2019年度クーポン券配布対象外の男性)"
  ),
  outcome_map = c(
    "test_int" = "抗体検査",
    "vaccine_int" = "ワクチン接種"
  ),
  not_show_x = list(
    "共変量" = all.vars(covmod)
  ),
  footnote = paste(
    "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
    "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
  )
)

#+ act-coupon0-altreg, eval = FALSE
act_coupon0$
  lm(se_type = "HC0", ctrl = "C")$
  table(
  title = paste(
    "利他強調メッセージと比較した意向に対する介入群の効果",
    "(2019年度クーポン券配布対象外の男性)"
  ),
  outcome_map = c(
    "aw1_test" = "抗体検査",
    "aw1_testvaccine" = "抗体検査\u00d7ワクチン接種"
  ),
  not_show_x = list(
    "共変量" = all.vars(covmod)
  ),
  footnote = paste(
    "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
    "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
  )
)

#' <!--
#' //NOTE: 抗体検査受検者の動き
#' -->
#+ tester-coupon0, eval = FALSE
tester_coupon0$chi2test(fisher = TRUE)$
  table(
    title = "2019年度クーポン券配布対象外の抗体検査受検者の動き",
    outcome_label = c(
      "aw1_testnega" = "Negative antibody",
      "aw1_testvaccine" = "Vaccination"
    ),
    value_label = c("0" = "No", "1" = "Yes")
  )