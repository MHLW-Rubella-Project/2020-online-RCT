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
  balance(subset = coupon2019 == 1)$
  table(title = paste(
    "Balance Test of Wave 2 Selection Data",
    "(Men who need to be processed to receive coupon in 2019)"
  ))

#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ coupon1-power, eval = FALSE
min_int_ef <- NULL
for (i in LETTERS[1:7]) {
  res <- int_coupon1$
    power(alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i)$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_int_ef)) min_int_ef <- min_diff_mean
  if (min_diff_mean <= min_int_ef) min_int_ef <- min_diff_mean
}

min_act_ef <- NULL
for (i in LETTERS[1:7]) {
  res <- act_coupon1$
    power(alpha = 0.05, power = 0.8, sd = 0.2, ctrl = i)$
    result$diff_mean

  min_diff_mean <- min(res, na.rm = TRUE)

  if (is.null(min_act_ef)) min_act_ef <- min_diff_mean
  if (min_diff_mean <= min_act_ef) min_act_ef <- min_diff_mean
}

cat(c(
  "始めに、2019年度にクーポン券が送付された40歳以上46歳以下の男性グループにおける、",
  "ナッジ・メッセージの意向と行動に対する効果を推定する。",
  "このサブグループにおいて、回答者の観察可能な特徴についてトリートメント間でバランスしている",
  "（補論\\@ref(addtab)の表\\@ref(tab:show-int-coupon1-balance)",
  "と表\\@ref(tab:show-act-coupon1-balance)）。",
  "したがって、本節では、",
  "厚労省メッセージと各ナッジ・メッセージ間の平均値の差の検定の結果のみを示し、",
  "回帰分析の結果は補論に示す。",
  "また、検出力80%・有意水準5%を保つために必要な効果の規模を計算したところ、",
  "意向をアウトカムとした場合は少なくとも",
  round(min_int_ef * 100, 1),
  "%ポイント必要であり、",
  "行動をアウトカムとした場合は少なくとも",
  round(min_act_ef * 100, 1),
  "%ポイント必要である。"
), sep = "\n")

#' <!---
#' //NOTE: 意向に対する効果のt検定
#' --->
#+ int-coupon1-ttest, eval = FALSE
int_coupon1$
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
#+ act-coupon1-ttest, eval = FALSE
act_coupon1$
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
#+ int-coupon1-reg, eval = FALSE
int_coupon1$
  lm(se_type = "HC0")$
  table(
    title = paste(
      "2019年度クーポン券配布対象者に限定した",
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

#+ act-coupon1-reg, eval = FALSE
act_coupon1$
  lm(se_type = "HC0")$
  table(
    title = paste(
      "2019年度クーポン券配布対象者に限定した",
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

#+ int-coupon1-altreg, eval = FALSE
int_coupon1$
  lm(se_type = "HC0", ctrl = "C")$
  table(
  title = paste(
    "利他強調メッセージと比較した意向に対する介入群の効果",
    "(2019年度クーポン券配布対象者)"
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

#+ act-coupon1-altreg, eval = FALSE
act_coupon1$
  lm(se_type = "HC0", ctrl = "C")$
  table(
  title = paste(
    "利他強調メッセージと比較した意向に対する介入群の効果",
    "(2019年度クーポン券配布対象者)"
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
#+ tester-coupon1, eval = FALSE
tester_coupon1$chi2test(fisher = TRUE)$
  table(
    title = "2019年度クーポン券配布対象の抗体検査受検者の動き",
    outcome_label = c(
      "aw1_testnega" = "Negative antibody",
      "aw1_testvaccine" = "Vaccination"
    ),
    value_label = c("0" = "No", "1" = "Yes")
  )

#+ negative-vaccine-coupon1, eval = FALSE
vrate <- with(
  subset(wave2, coupon2019 == 1 & aw1_testnega == 1),
  meanci_boot(aw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

cat(c(
  "[^bootstrap]: 1000個のブートストラップ標本を用いて、陰性者のワクチン接種率の95%信頼区間を計算すると、",
  sprintf("[%1.1f%%, %1.1f%%]", vrate$ci[1] * 100, vrate$ci[2] * 100),
  "となった。"
), sep = "\n")

#+ bayesian-coupon1, eval = FALSE
bayes1 <- cal_testcondnega_selection1(
  subset(wave2, coupon2019 == 1 & nudge == "C")
)

qt_bayes1 <- quantile(
  bayes1$boot$testcondnega,
  prob = c(0.025, 0.975), na.rm = TRUE
)

qt2_bayes1 <- quantile(
  bayes1$boot$diff,
  prob = c(0.025, 0.975), na.rm = TRUE
)

qt_bayes2 <- quantile(
  cal_testcondnega_selection1(
    subset(wave2, coupon2019 == 1 & nudge == "E")
  )$boot$diff,
  prob = c(0.025, 0.975), na.rm = TRUE
)

cat(c(
  "[^Bayesian]: これと対立する仮説として、",
  "利他強調メッセージや社会比較メッセージを読んで抗体検査を受検した人は",
  "自身に抗体を保有していないと信じているということが考えられる。",
  "そこで、陰性者の抗体検査受検率を推定することを試みる。",
  "しかしながら、陰性であるにも関わらず抗体検査を受検していない人がいるはずなので、",
  "陰性者の抗体検査受検率をデータから直接復元することはできない。",
  "我々は、ベイズ定理を用いて、間接的に推定した。",
  "陰性という事象$A$と抗体検査の受検という事象$B$の二つの事象を考える。",
  "このとき、抗体検査の受検比率は$P(B)$、抗体検査受検者の陰性比率は$P(A|B)$で表すことができ、",
  "これらの値はデータから直接推定できる。",
  "ベイズの定理より、抗体検査受検者の陰性比率は",
  "$$ P(A|B) = \\frac{P(B|A) \\cdot P(A)}{P(B)} $$",
  "と定義できる。",
  "ここで、$P(A)$は陰性比率であり、",
  "これは第\\@ref(background)節で示したNIIDのデータより0.2となる。",
  "確率$P(B|A)$は陰性者で条件づけた抗体検査の受検比率であり、我々の関心のあるパラメータである。",
  "よって、陰性者の抗体検査の受検比率は",
  "$$ \\hat{P}(B|A) = \\frac{\\hat{P}(A|B) \\cdot \\hat{P}(B)}{0.2} $$",
  "で計算できる。",
  "利他強調メッセージ群において、",
  "$\\hat{P}(A|B) = 0.5$と$\\hat{P}(B) = 0.109$なので、",
  "$\\hat{P}(B|A) = 0.273$となる（1000個のブートストラップ標本で構築した95%信頼区間は",
  sprintf("[%1.3f, %1.3f]", qt_bayes1[1], qt_bayes1[2]),
  "さらに、陰性であるかどうかによって抗体検査の受検にセレクションが生じているかどうかを",
  "検証するために、陰性という事象と抗体検査の受検という事象が独立であるという帰無仮説を検定した。",
  "$\\hat{P}(B|A) - \\hat{P}(B)$の95%信頼区間にゼロが含まれていないとき、",
  "我々は帰無仮説を5%有意水準で棄却できる。",
  "その結果、$\\hat{P}(B|A) - \\hat{P}(B)$の95%信頼区間は",
  sprintf("[%1.3f, %1.3f]", qt2_bayes1[1], qt2_bayes1[2]),
  "なので、我々は帰無仮説を棄却できる。",
  "同様に、社会比較メッセージ群の$\\hat{P}(B|A) - \\hat{P}(B)$の95%信頼区間は",
  sprintf("[%1.3f, %1.3f]", qt_bayes2[1], qt_bayes2[2]),
  "である。",
  "したがって、利他強調メッセージ群と社会比較メッセージ群では、",
  "陰性者が抗体検査を積極的に受検している傾向があるかもしれない。"
), sep = "\n")
