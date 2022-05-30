#' ---
#' title: Effect of Text-Based Nudges on Behavior
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
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
  exercise_w1 + health_check + flushot +
  # prob_social + handicap + severity +
  handwash + temp_check + avoid_out + avoid_crowd + wear_mask

act <- create_RCTtoolbox(
  aw1_test + aw1_testvaccine ~ nudge,
  covmod,
  data = wave2,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#'
#' <!---
#' //NOTE: 検出力分析
#' --->
#'
#+ act-power, eval = params$preview | !params$appendix, results = "asis"
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

cat(c(
  "次に、wave 2 target dataを用いて、",
  "我々は第1回調査以降の行動に対するナッジ・メッセージの効果を推定する。",
  "第1回から第2回にかけて若干名が脱落しているので、我々は改めてバランステストを行い、",
  "２つのサブサンプルの両方で、",
  "個人の観察可能な特徴はトリートメント間でバランスされていることを確認した（補論Bを見よ）。",
  "そこで、ここでも本稿にはt検定の結果を掲載して、回帰分析の結果は補論Cに掲載する。",
  "また、検定力80%・有意水準5%を保つために必要な効果の規模を計算したところ、",
  "2019年度にクーポン券が自動で送付される男性のサブサンプルを用いる場合、少なくとも",
  round(min_act_ef1 * 100, 1),
  "%ポイントの差が必要である。",
  "2019年度ではクーポン券を受け取るために手続きが必要な男性のサブサンプルを用いる場合、少なくとも",
  round(min_act_ef0 * 100, 1),
  "%ポイントの差が必要である。"
), sep = "\n")

#'
#' <!---
#' //NOTE: 行動に対する効果のt検定
#' --->
#'
#+ act-coupon1-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Behavior among Men for whom Coupons are Automatically Distributed in FY 2019 (N = 805). Data source: wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
act$
  ttest(subset = coupon2019 == 1)$
  plot(
    xlab = "Treatments",
    ylab = "Proportion",
    title = switch(outcome,
      "aw1_test" = "A. Antibody Test (Behavior after wave 1)",
      "aw1_testvaccine" = "B. Vaccination (Behavior after wave 1)"),
    inplot_lab_adjust = 0.1,
    flip = TRUE,
    ylim = c(-0.01, 0.3),
    ncol = 1
  )

#' ```{asis, echo = params$preview | !params$appendix}
#' まず、2019年度にクーポン券が自動で送付される男性のサブサンプルを用いて、
#' 我々は各介入群の抗体検査の受検率（パネルA）とワクチン接種率（パネルB）を
#' 図\@ref(fig:act-coupon1-ttest)に示した[^def_vaccine_rate]。
#' その結果、意向のケースと同様に、厚労省メッセージに比べて、
#' 利他強調メッセージが実際の抗体検査の受検率を高めることが分かった。
#' 厚労省メッセージの受検率が約3.5%だったのに対して、
#' 利他強調メッセージのそれは約10.9%だった。
#' したがって、利他強調メッセージの効果サイズは約7.4%ポイントであり、
#' これは統計的に5%水準で有意である。
#' この効果サイズは、上で計算した7.2%ポイントという基準よりも少しばかり大きい。
#' さらに、この利他強調メッセージは抗体検査受検率だけでなくワクチン接種率をも高める効果を持つことが示された。
#' この群の接種率は約4.7%であり、厚労省メッセージ（0.9%）に比べて3.8%ポイント高く、
#' これは10%水準で統計的に有意であった。
#'
#' [^def_vaccine_rate]: ワクチン接種は抗体検査を受検し、ワクチンを接種したら1を取るダミー変数である。
#' よって、ワクチン接種率はワクチン接種を通じて新規に抗体を獲得した人の比率とみなすこともできる。
#' これは厚生労働省の政策目標と対応するアウトカム変数である。
#'
#' また、利己強調メッセージは抗体検査の受検率を高めている可能性が示唆された。
#' この群の抗体検査の受検率は約9%であり、
#' 厚労省メッセージ（3.5%）に比べて5.5%ポイント高く、
#' これは統計的に10%水準で有意である。
#' さらに、社会比較メッセージはワクチン接種率を高めている可能性も示唆された。
#' この群のワクチン接種率は約4.9%であり、
#' 厚労省メッセージ（0.9%）よりも4%ポイント高い。
#' これは統計的に10%水準で有意である。
#' ```
#'
#+ act-coupon0-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Behaviors among Men Who Needed Costly Procedures to Receive Coupons in FY 2019 (N = 1,467). Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
act$
  ttest(subset = coupon2019 == 0)$
  plot(
  xlab = "Treatments",
  ylab = "Proportion",
  title = switch(outcome,
    "aw1_test" = "A. Antibody Test (Behavior after wave 1)",
    "aw1_testvaccine" = "B. Vaccination (Behavior after wave 1)"),
  inplot_lab_adjust = 0.05,
  flip = TRUE,
  ylim = c(-0.01, 0.15),
  ncol = 1
)

#' ```{asis, echo = params$preview | !params$appendix}
#' 次に、2019年度にクーポン券を得るためにコストのかかる手続きが必要な男性のサブサンプルを用いて、
#' 我々は各介入群の抗体検査の受検率（パネルA）とワクチン接種率（パネルB）を
#' 図\@ref(fig:act-coupon0-ttest)に示した。
#' その結果、厚労省メッセージと比較して、
#' 社会比較メッセージは抗体検査の受検率を高めているが、
#' ワクチン接種率を高めていない。
#' 厚労省メッセージを読んだ人の0.5%が抗体検査を受検しているが、
#' 誰もワクチン接種をしていない。
#' 同様に、社会比較メッセージを読んだ人の2.8%が抗体検査を受検しているが、
#' 誰もワクチン接種をしていない。
#' したがって、厚労省メッセージと比較して、
#' 社会比較メッセージは抗体検査の受検率を約2.3%ポイント引き上げていて、
#' これは統計的に10%水準で有意である。
#' しかしながら、ワクチン接種率に対する効果はゼロである。
#' ```
#'
#' <!---
#' //NOTE: 行動に対する効果の回帰分析
#' --->
#+ act-reg, eval = params$preview | params$appendix
act$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = "Linear Probability Model of Behaviors",
    outcome_map = c(
      "aw1_test" = "Antibody Test",
      "aw1_testvaccine" = "Vaccination"
    ),
    add_coef_map = c(
      "coupon2019" = "Coupon",
      "coupon_b" = "Coupon\u00d7Age expression",
      "coupon_c" = "Coupon\u00d7Altruistic",
      "coupon_d" = "Coupon\u00d7Selfish",
      "coupon_e" = "Coupon\u00d7Social comparison",
      "coupon_f" = "Coupon\u00d7Valid date",
      "coupon_g" = "Coupon\u00d7Low-cost"
    ),
    not_show_x = list(
      "Covariates" = all.vars(covmod)[8:length(all.vars(covmod))]
    ),
    footnote = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use robust standard errors.",
      "We also control for covariates obtained in wave 1 and 2.",
      "The list of covariates is presented in Table \\@ref(tab:covlist)."
    )
  )

#+ act-reg-ftest, eval = params$preview | params$appendix
covlist <- paste(all.vars(covmod)[8:length(all.vars(covmod))], collapse = "+")

actmod <- list(
  "(1)" = update(
    aw1_test ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  ),
  "(2)" = update(
    aw1_testvaccine ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  )
)

est_actmod <- actmod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = act$data, se_type = "HC0",
      linear_hypothesis = c(
        "nudgeB:coupon2019 + nudgeB = 0",
        "nudgeC:coupon2019 + nudgeC = 0",
        "nudgeD:coupon2019 + nudgeD = 0",
        "nudgeE:coupon2019 + nudgeE = 0",
        "nudgeF:coupon2019 + nudgeF = 0",
        "nudgeG:coupon2019 + nudgeG = 0"
      )
    )
  }) %>%
  lapply(function(x) {
    coef <- subset(tidy(x$lm_robust), str_detect(term, "nudge"))
    coef <- subset(coef, !str_detect(term, "coupon"))
    lh <- tidy(x$lh)
    bind_rows(coef, lh)
  }) %>%
  bind_rows() %>%
  mutate(coupon = if_else(str_detect(term, "coupon"), 1, 0)) %>%
  mutate(
    coupon = factor(coupon,
      labels = c("Costly procedure", "Automatic receiving")
    ),
    outcome = factor(outcome,
      levels = c("aw1_test", "aw1_testvaccine"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[1:7], collapse = "|")),
    nudge = factor(nudge, labels = treat_labels[-1])
  )

rawvalue <- function(x) x

est_actmod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

#+ act-reg-ftest2, eval = params$preview | params$appendix
est_actmod2 <- actmod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = act$data, se_type = "HC0",
      linear_hypothesis = c(
        # "- nudgeC = 0",
        "nudgeB - nudgeC = 0",
        "nudgeD - nudgeC = 0",
        "nudgeE - nudgeC = 0",
        "nudgeF - nudgeC = 0",
        "nudgeG - nudgeC = 0",
        # "- nudgeC:coupon2019 - nudgeC = 0",
        "nudgeB:coupon2019 + nudgeB - nudgeC:coupon2019 - nudgeC = 0",
        "nudgeD:coupon2019 + nudgeD - nudgeC:coupon2019 - nudgeC = 0",
        "nudgeE:coupon2019 + nudgeE - nudgeC:coupon2019 - nudgeC = 0",
        "nudgeF:coupon2019 + nudgeF - nudgeC:coupon2019 - nudgeC = 0",
        "nudgeG:coupon2019 + nudgeG - nudgeC:coupon2019 - nudgeC = 0"
      )
    )$lh %>%
    tidy
  }) %>%
  bind_rows() %>%
  mutate(
    coupon = if_else(str_detect(term, "coupon"), 1, 0),
    coupon = factor(coupon,
      labels = c("Costly procedure", "Automatic receiving")
    ),
    outcome = factor(outcome,
      levels = c("aw1_test", "aw1_testvaccine"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[c(2, 4:7)], collapse = "|")),
    # nudge = if_else(is.na(nudge), "A", nudge),
    nudge = factor(nudge, labels = treat_labels[c(2, 4:7)])
  )

est_actmod2 %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates",
      "(Baseline: Altruistic Message)"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

#'
#' ```{asis, echo = !params$preview & !params$appendix}
#' サブサンプルで推定されたナッジ・メッセージの効果は
#' クーポン券が自動的に送付されるかどうかだけでなく、
#' 年齢層の違いの影響を受けるので、
#' 我々はこの問題を排除するために線形確率モデルを推定した。
#' 行動の線形確率モデルは上述の結果と同じ結果を得られた。
#' それに加えて、2019年度にクーポン券が自動的に送付される男性において、
#' 社会比較メッセージの抗体検査の受検率は厚労省メッセージよりも5.7%ポイント高く、
#' これは統計的に10%水準で有意である。
#' まとめると、利他強調メッセージと社会比較メッセージが
#' 実際の抗体検査の受検率とワクチンの接種率を高める効果を持っていることが確認された。
#' ```
#'
#' ```{asis, echo = params$preview | params$appendix}
#' 意向の線形確率モデルと同じように、
#' 我々は行動を被説明変数とした線形確率モデルを推定した。
#' 表\@ref(tab:act-reg)は線形確率モデルの結果である。また、
#' 表\@ref(tab:act-reg-ftest)は線形確率モデルの推定値を用いた
#' ナッジ・メッセージの効果である。
#' その結果、二群の平均値の差の推定と同様の結果を得られた。
#' それに加えて、2019年度にクーポン券が自動的に送付される男性における
#' 社会比較メッセージの抗体検査の受検率に対する効果は5.7%ポイントで、
#' 統計的に10%水準で有意である。
#' また、表\@ref(tab:act-reg)より、
#' 利他強調メッセージの抗体検査受検率に対する効果と
#' 社会比較メッセージのワクチン接種率に対する効果は
#' クーポン券の受け取り方によって異なり、
#' これは統計的に10%水準で有意である。
#'
#' 表\@ref(tab:act-reg-ftest2)は利他強調メッセージを参照群とした
#' メッセージの効果の推定結果である。
#' 利他強調メッセージ以外のナッジ・メッセージの抗体検査受検率は
#' 利他強調メッセージのそれと有意に異ならない。
#' この意味で、他のナッジ・メッセージも抗体検査の受検を促進しているかもしれないが、
#' 検出力を十分に保てるほどの差でない。
#' ```
#'
#+ include = FALSE, eval = params$preview | !params$appendix
tester1 <- create_RCTtoolbox(
  aw1_testnega ~ nudge,
  data = subset(act$data, aw1_test == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

chi2test11 <- tester1$
  chi2test(subset = coupon2019 == 1, fisher = TRUE, bootp = TRUE)$
  result

chi2test10 <- tester1$
  chi2test(subset = coupon2019 == 0, fisher = TRUE, bootp = TRUE)$
  result

tester2 <- create_RCTtoolbox(
  aw1_testvaccine ~ nudge,
  data = subset(act$data, aw1_testnega == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

chi2test21 <- tester2$
  chi2test(subset = coupon2019 == 1, fisher = TRUE, bootp = TRUE)$
  result

chi2test20 <- tester2$
  chi2test(subset = coupon2019 == 0, fisher = TRUE, bootp = TRUE)$
  result


chi2test_result_tab <- tribble(
  ~terms, ~test1, ~negative1, ~ vaccine1, ~test0, ~negative0, ~ vaccine0,
  "Fisher's exact test (p-value)",
  "",
  sprintf("%1.2f", chi2test11$result$aw1_testnega$p),
  sprintf("%1.2f", chi2test21$result$aw1_testvaccine$p),
  "",
  sprintf("%1.2f", chi2test10$result$aw1_testnega$p),
  sprintf("%1.2f", chi2test20$result$aw1_testvaccine$p)
)

#+ tester-move, eval = params$preview | !params$appendix
tab <- act$data %>%
  mutate(coupon2019 = factor(
    coupon2019,
    levels = c(1, 0),
    labels = c(
      "w/ receiving coupon automatically",
      "w/o receiving coupon automatically"
    )
  )) %>%
  mutate(nudge = factor(nudge, labels = treat_labels)) %>%
  datasummary(
    (`Text-based nudge` = nudge) ~ coupon2019 * sum * (
      (`Antibody test` = aw1_test) +
      (`Negative test result` = aw1_testnega) +
      (`Vaccination` = aw1_testvaccine)
    ),
    title = "Movement of Antibody Test Takers",
    data = .,
    fmt = 0,
    align = "lcccccc",
    add_rows = chi2test_result_tab,
  )

if (out == "kableExtra") {
  tab %>%
    kable_styling(font_size = 9) %>%
    column_spec(column = 1, width = "9em") %>%
    column_spec(column = 2:7, width = "5em") %>%
    kableExtra::add_footnote(
      label = paste(
        "Note: Limiting our sample to antibody test takers,",
        "we tested the null hypothesis that",
        "the number of negative antibody tests does not differ",
        "between intervention groups with Fisher's exact test.",
        "Also, restricting the sample to negative individuals,",
        "we tested the null hypothesis that",
        "the number of vaccinations would not differ",
        "between intervention groups with a Fisher's exact test."
      ),
      notation = "none",
      threeparttable = TRUE
    )
}

#' ```{asis, echo = params$preview | !params$appendix}
#' クーポン券が自動的に送付されるかどうかに関わらず、
#' すべての群において、ワクチン接種率は抗体検査の受験率より低い。
#' これは陰性であるにも関わらずワクチンを接種していない人が多いからではなく、
#' もともと抗体を持っていたためにワクチンを接種するべき人が少ない
#' という外生的な要因によるものであると考えられる。
#' この点を明らかにするために、
#' 我々は抗体検査の受検者数・抗体検査が陰性であった人の数・ワクチンを接種した人数を
#' 介入群ごとに計算し、表\@ref(tab:tester-move)に示した。
#'
#' 表\@ref(tab:tester-move)より、
#' クーポン券が自動的に送付されかどうかや介入群に関わらず
#' 抗体検査の結果が陰性である人のほとんどがワクチンを接種している[^bootstrap]。
#' 2019年度にクーポン券を自動的に受け取った男性に限定すると、
#' 利他強調メッセージ・低コストメッセージを除くすべての群で
#' 陰性者はワクチンを接種している。
#' また、利他強調メッセージ・低コストメッセージ群においても
#' ワクチンを接種していない陰性者は少数である。
#' 同様に、2019年度にクーポン券を受け取るためにはコストのかかる手続きが必要な男性に限定すると、
#' 年齢表現メッセージ・社会比較メッセージを除くすべての群で
#' 陰性者はワクチンを接種している。
#'
#' さらに、表\@ref(tab:tester-move)より、
#' 抗体検査の陰性件数が介入群間でバラツキがあることが分かる。
#' 2019年度にクーポン券を自動的に受け取った男性に限定したとき、
#' 厚労省メッセージの抗体検査の陰性比率は25%($=1/4$)である。
#' これに対して、ワクチン接種率に対して効果のある
#' 利他強調メッセージ・社会比較メッセージの抗体検査の陰性比率は
#' それぞれ50%（$=7/14$）・55%（$=5/9$）である。
#' 逆に、抗体検査受検率のみに効果のある利己強調メッセージの抗体検査の陰性比率は
#' 30%($=3/10$)であり、
#' 厚労省メッセージのそれと近い値を取る。
#' 2019年度にクーポン券を受け取るためにはコストのかかる手続きが必要な男性に限定するとき、
#' 抗体検査受検率のみに効果のある社会比較メッセージの抗体検査の陰性比率は16%（$=1/6$）である。
#' したがって、
#' ワクチン接種率が高い介入群は陰性比率が高く、
#' それがワクチン接種率に対する効果に影響を与えたといえる。
#'
#' ただし、抗体検査の陰性比率の介入群間のばらつきは統計的な誤差による可能性が高い。
#' 我々はクーポン券を自動的に送付される対象かどうかでサンプルを分割し、
#' 抗体検査の陰性件数が介入群間で異ならないという帰無仮説を
#' フィッシャーの正確検定で検証した。
#' その結果、二つのサブサンプルでこの帰無仮説を棄却できない。
#' よって、我々のデータの抗体検査の陰性比率は介入群間で異なっているが、
#' 母集団のそれは介入群間で異ならないかもしれない[^Bayesian]。
#' ```
#'
#+ eval = params$preview | !params$appendix, results = "asis"
vrate1 <- with(
  subset(act$data, coupon2019 == 1 & aw1_testnega == 1),
  meanci_boot(aw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

vrate0 <- with(
  subset(act$data, coupon2019 == 0 & aw1_testnega == 1),
  meanci_boot(aw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

cat(c(
  "[^bootstrap]: 2019年度にクーポン券を自動的に受け取った男性に限定すると、",
  "陰性者のワクチン接種率は87.5%($=21/24$)であり、",
  "1000個のブートストラップ標本を用いて構築した95%信頼区間は",
  sprintf("[%1.1f%%, %1.1f%%]", vrate1$ci[1] * 100, vrate1$ci[2] * 100),
  "である。",
  "2019年度にクーポン券を受け取るためにはコストのかかる手続きが必要な男性に限定すると、",
  "陰性者のワクチン接種率は66.7%($=4/6$)であり、",
  "1000個のブートストラップ標本を用いて構築した95%信頼区間は",
  sprintf("[%1.1f%%, %1.1f%%]", vrate0$ci[1] * 100, vrate0$ci[2] * 100),
  "である。"
), sep = "\n")

#+ eval = params$preview | !params$appendix, results = "asis"
bayes1 <- cal_testcondnega_selection1(
  subset(act$data, coupon2019 == 1 & nudge == "C")
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
  "ベイズ定理を用いて、我々は間接的に推定した。",
  "陰性という事象$A$と抗体検査の受検という事象$B$の二つの事象を考える。",
  "このとき、抗体検査の受検比率は$P(B)$、抗体検査受検者の陰性比率は$P(A|B)$で表すことができ、",
  "これらの値はデータから直接推定できる。",
  "ベイズの定理より、抗体検査受検者の陰性比率は",
  "$$ P(A|B) = \\frac{P(B|A) \\cdot P(A)}{P(B)} $$",
  "と定義できる。",
  "ここで、$P(A)$は陰性比率であり、",
  "これはNIIDの抗体保有率のデータより0.2となる。",
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

# /*
#+
rmarkdown::render(
  here("src/5-selection1-behavior.r"),
  output_dir = here("report/view")
)
# */