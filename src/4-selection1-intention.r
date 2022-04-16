#' ---
#' title: Effect of Text-Based Nudges on Intentions
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

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
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

#' ```{asis, echo = params$preview | !params$appendix}
#' はじめに、我々は意向に対するナッジ・メッセージの効果を推定する。
#' ナッジ・メッセージによって行動変容を促したい対象は
#' 第1回調査時点で抗体検査やワクチン接種を受けていない男性である。
#' そこで、第1回調査で過去に抗体検査とワクチン接種を受けていないと回答した人に限定したデータを用いる
#' （wave 1 selection data）。
#' さらに、我々の関心は
#' クーポン券の自動送付によってクーポン券を取得するための取引費用が減少したという意味での
#' インセンティブの有無のもとでのナッジ・メッセージの効果である。
#' 自治体は2019年度に40歳から46歳の男性に無料クーポン券を送付し、
#' 2020年度以降に47歳から56歳の男性にクーポン券を送付する。
#' そのために、我々は、2019年4月時点の年齢が46歳以下であるかどうかでサブサンプルを構築し、
#' 各サブサンプルにおけるナッジ・メッセージの効果を推定した。
#' 二つのサブサンプルにおいて、
#' 個人の観察可能な特徴はトリートメント間でバランスされているので、
#' t検定の結果のみを示し、回帰分析の結果は補論Cに示す（バランステストの結果は補論Bを見よ）。
#' ```
#'
#' <!---
#' //NOTE: 検出力分析
#' --->
#'
#+ int-power, eval = params$preview | !params$appendix, results = "asis"
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

cat(c(
  "また、検定力80%・有意水準5%を保つために必要な効果の規模を計算したところ、",
  "2019年度にクーポン券が自動で送付される男性のサブサンプルを用いる場合、少なくとも",
  round(min_int_ef1 * 100, 1),
  "%ポイントの差が必要である。",
  "2019年度ではクーポン券を受け取るために手続きが必要な男性のサブサンプルを用いる場合、少なくとも",
  round(min_int_ef0 * 100, 1),
  "%ポイントの差が必要である。"
), sep = "\n")

#'
#' <!---
#' //NOTE: 意向に対する効果のt検定
#' --->
#'
#+ int-coupon1-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Intentions among Men for whom Coupons are Automatically Distributed in FY 2019. Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
int$
  ttest(subset = coupon2019 == 1)$
  plot(
    xlab = "Treatments",
    ylab = "Proportion",
    title = switch(outcome,
      "test_int" = "A. Antibody Test (Intention)",
      "vaccine_int" = "B. Vaccination (Intention)"),
    inplot_lab_adjust = 0.1,
    flip = TRUE,
    ylim = c(0, 1),
    ncol = 1
  )

#' ```{asis, echo = params$preview | !params$appendix}
#' 2019年度にクーポン券が自動で送付される男性のサブサンプルを用いて、
#' 我々は各介入群の抗体検査（パネルA）とワクチン接種（パネルB）の意向の比率を
#' 図\@ref(fig:int-coupon1-ttest)に示した。
#' その結果、利他強調メッセージは厚労省メッセージより抗体検査の意向を高めている。
#' 厚労省メッセージ群の抗体検査の意向の比率は約20.8%であるのに対して、
#' 利他強調メッセージ群の抗体検査の意向の比率は約35.1%である。
#' したがって、厚労省メッセージと比較して、
#' 利他強調メッセージは抗体検査の意向を約14.3%ポイント高めていて、
#' これは統計的に1%水準で有意である。
#' また、厚労省メッセージと比較して、
#' すべてのナッジ・メッセージはワクチン接種の意向を統計的に有意に高めていない[^stimulate1]。
#'
#' [^stimulate1]: また、すべての介入群のワクチン接種の意向の比率は抗体検査のそれよりも高い。
#' これはワクチン接種の意向を引き出す質問の刺激によるものだと考えられる。
#' 我々はワクチン接種の意向を回答者に尋ねるとき、抗体を保有していないことを条件にしている。
#' この条件がワクチン接種の必要性を強く刺激している可能性がある。
#' ```
#'
#+ int-coupon0-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Intentions among Men Who Needed Costly Procedures to Receive Coupons in FY 2019. Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
int$
  ttest(subset = coupon2019 == 0)$
  plot(
  xlab = "Treatments",
  ylab = "Proportion",
  title = switch(outcome,
    "test_int" = "A. Antibody Test (Intention)",
    "vaccine_int" = "B. Vaccination (Intention)"),
  inplot_lab_adjust = 0.1,
  flip = TRUE,
  ylim = c(0, 1),
  ncol = 1
)

#' ```{asis, echo = params$preview | !params$appendix}
#' 2019年度にクーポン券を得るためにコストのかかる手続きが必要な男性のサブサンプルを用いて、
#' 我々は各介入群の抗体検査（パネルA）とワクチン接種（パネルB）の意向の比率を
#' 図\@ref(fig:int-coupon0-ttest)に示した。
#' その結果、厚労省メッセージと比較して、
#' すべてのナッジ・メッセージは抗体検査の意向を統計的に有意に高めていない。
#' 対照的に、
#' 社会比較メッセージは厚労省メッセージよりもワクチン接種の意向を低めている。
#' 厚労省メッセージのワクチン接種の意向比率は約52.8%であるのに対し、
#' 社会比較メッセージのワクチン接種の意向比率は約44.6%である[^stimulate0]。
#' したがって、厚労省メッセージと比較して、
#' 社会比較メッセージはワクチン接種の意向を約8.2%ポイント低めており、
#' これは統計的に10%水準で有意である。
#'
#' [^stimulate0]: 2019年度にクーポン券が自動で送付される男性のサブサンプルを用いた結果と同様に、
#' すべての介入群のワクチン接種の意向比率は抗体検査のそれよりも高い。
#' これはワクチン接種の意向を引き出す質問の刺激によるものだと考えられる。
#'
#' この効果の原因の一つとして、ワクチン接種のただのりが挙げられる。
#' 社会比較メッセージは「5人に1人が抗体を持っていない」ことを強調している。
#' 裏返せば、5人に4人が抗体を持っているということである。
#' このメッセージを読んだ人は、
#' 仮に風しんの抗体を保有していないとしても、全体の80% が抗体を持っているので、
#' 自身が感染する機会は少ないと考えたのかもしれない。
#' クーポン券を受け取るために手続きが必要なとき、
#' この信念がワクチンを接種することの価値を低め、
#' ワクチン接種の意向の比率を厚労省メッセージよりも下げた可能性がある。
#' ```
#'
#' <!---
#' //NOTE: 意向に対する効果の回帰分析
#' --->
#+ int-reg, eval = params$preview | params$appendix
int$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = "Linear Probability Model of Intentions",
    outcome_map = c(
      "test_int" = "Antibody Test",
      "vaccine_int" = "Vaccination"
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
    )
  )

#+ int-reg-ftest, eval = params$preview | params$appendix
covlist <- paste(all.vars(covmod)[8:length(all.vars(covmod))], collapse = "+")

intmod <- list(
  "(1)" = update(
    test_int ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  ),
  "(2)" = update(
    vaccine_int ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  )
)

est_intmod <- intmod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = int$data, se_type = "HC0",
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
      levels = c("test_int", "vaccine_int"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[1:7], collapse = "|")),
    nudge = factor(nudge, labels = treat_labels[-1])
  )

rawvalue <- function(x) x

est_intmod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Intentions",
      "Using Linear Probability Model Estimates"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

#'
#' ```{asis, echo = params$preview | !params$appendix}
#' クーポン券が自動的に送付されるかどうかは年齢で決まるので、
#' サブサンプルを用いたナッジ・メッセージの効果は
#' クーポン券が自動的に送付されるかどうかだけでなく、
#' 二つのサブサンプルの年齢の違いの影響を受けている。
#' この問題を排除するために、意向の線形確率モデルを推定した。
#' 説明変数はナッジ・メッセージのダミー変数、
#' ナッジ・メッセージのダミー変数とクーポン券が自動的に送付されることを示すダミー変数の交差項、
#' そして年齢を含んだ共変量である。
#' 意向の線形確率モデルは上述の結果と同じ結果を得られた（詳細は補論を参照せよ）。
#' ```
#'
#' ```{asis, echo = params$preview | params$appendix}
#' クーポン券が自動的に送付されるかどうかは年齢で決まるので、
#' サブサンプルを用いたナッジ・メッセージの効果は
#' クーポン券が自動的に送付されるかどうかだけでなく、
#' 二つのサブサンプルの年齢の違いの影響を受けている。
#' この問題を排除するために、我々は以下のような意向の線形確率モデルを推定した。
#' \begin{align}
#'   Y_{ij} = \alpha + \sum_j \beta_j \text{Message}_j
#'            + \sum_j \gamma_j (\text{Message}_j \times \text{Coupon}_i)
#'            + \delta \text{Coupon}_i + \lambda X'_{ij} + \epsilon_{ij},
#' \end{align}
#' ここで、$\text{Message}_j$は厚労省メッセージ群をコントロールとした介入群ダミーであり、
#' $\text{Coupon}_i$はクーポン券の自動送付を受け取ったことを示すダミー変数である。
#' $X$は個人の共変量ベクトルであり、年齢を含む。
#'
#' 関心のあるパラメータは$\beta_j$と$\gamma_j$である。
#' クーポン券の自動送付の対象でない男性におけるナッジ・メッセージ$j$の効果は$\hat{\beta}_j$である。
#' 一方で、クーポン券の自動送付の対象である男性におけるナッジ・メッセージ$j$の効果は
#' $\hat{\beta}_j + \hat{\gamma}_j$である。
#'
#' 表\@ref(tab:int-reg)は線形確率モデルの結果である。また、
#' 表\@ref(tab:int-reg-ftest)は線形確率モデルの推定値を用いたナッジ・メッセージの効果である。
#' 2019年度にクーポン券が自動的に送付される男性における
#' 利他強調メッセージの抗体検査の意向に対する効果は14%ポイントであり、
#' t検定の結果と一致する。
#' 対して、2019年度にクーポン券を取得するために手続きが必要な男性における
#' 利他強調メッセージの抗体検査の意向に対する効果は5.1%ポイントであり、
#' 統計的に非有意である。
#' 表\@ref(tab:int-reg)より、この二つの効果の差は統計的に非有意である。
#'
#' また、2019年度にクーポン券が自動的に送付される男性における
#' 社会比較メッセージのワクチン接種の意向に対する効果は2.9%ポイントであり、
#' 統計的に非有意である。
#' 対して、2019年度にクーポン券を取得するために手続きが必要な男性における
#' 社会比較メッセージのワクチン接種の意向に対する効果は-9.8%ポイントであり、
#' t検定で推定された効果より若干大きくなった。
#' 表\@ref(tab:int-reg)より、この二つの効果の差は統計的に10%水準で有意である。
#'
#' さらに、2019年度にクーポン券を取得するために手続きが必要な男性における
#' 年齢表現メッセージのワクチン接種の意向に対する効果は-9.9%ポイントであり、
#' 統計的に5%水準で有意である。
#' t検定で推定された効果の規模は-6.6%ポイントであり、
#' 共変量の有無で効果の規模が大きく異なる。
#' ```
#'
# /*
#+
rmarkdown::render(
  here("src/4-selection1-intention.r"),
  output_dir = here("report/view")
)
# */