#' ---
#' title: |
#'   Analysis to Address Recall Bias Associated with Self-Reporting of Behavior
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

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
  exercise_w1 + health_check + flushot +
  prob_social + handicap + severity +
  handwash + temp_check + avoid_out + avoid_crowd + wear_mask

act2 <- create_RCTtoolbox(
  abw1_test + abw1_testvaccine ~ nudge,
  covmod,
  data = wave22,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#' <!---
#' //NOTE: t検定
#' --->
#'
#+ act2-coupon1-ttest, fig.cap = "Effect of Text-Based Nudges on Behavior among Men for whom Coupons are Automatically Distributed in FY 2019. Data source: new wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
act2$
  ttest(subset = coupon2019 == 1)$
  plot(
    xlab = "Treatments",
    ylab = "Proportion",
    title = switch(outcome,
      "abw1_test" = "A. Antibody Test (Behavior)",
      "abw1_testvaccine" = "B. Vaccination (Behavior)"
    ),
    inplot_lab_adjust = 0.1,
    flip = TRUE,
    ylim = c(-0.01, 0.3),
    ncol = 1
  )

#+ act2-coupon0-ttest, fig.cap = "Effect of Text-Based Nudges on Behaviors among Men Who Needed Costly Procedures to Receive Coupons in FY 2019. Data source: new wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
act2$
  ttest(subset = coupon2019 == 0)$
  plot(
    xlab = "Treatments",
    ylab = "Proportion",
    title = switch(outcome,
      "abw1_test" = "A. Antibody Test (Behavior)",
      "abw1_testvaccine" = "B. Vaccination (Behavior)"
    ),
    inplot_lab_adjust = 0.05,
    flip = TRUE,
    ylim = c(-0.01, 0.15),
    ncol = 1
  )

#'
#' 2019年度にクーポン券を自動的に受け取った男性に限定して、
#' 我々は各介入群の抗体検査受検率（パネルA）とワクチン接種率（パネルB）を
#' 図\@ref(fig:act2-coupon1-ttest)に示した。
#' その結果、利他強調メッセージは厚労省メッセージよりも抗体検査受検率が高い。
#' 厚労省メッセージの抗体検査受検率は6.6%であるのに対し、
#' 利他強調メッセージの抗体検査受検率は14.1%である。
#' したがって、厚労省メッセージと比較して、
#' 利他強調メッセージは抗体検査の受検率を7.5%ポイント高めていて、
#' これは統計的に5%水準で有意である。
#' この効果の規模は本論の結果と一致している。
#' また、利他強調メッセージのワクチン接種率に対する効果は統計的に非有意である。
#'
#' 2019年度にクーポン券を受け取るためにはコストのかかる手続きが必要な男性に限定して、
#' 我々は各介入群の抗体検査受検率（パネルA）とワクチン接種率（パネルB）を
#' 図\@ref(fig:act2-coupon0-ttest)に示した。
#' その結果、利他強調メッセージと低コストメッセージは厚労省メッセージよりも抗体検査の受検率を高めていて、
#' 低コストメッセージのみが厚労省メッセージよりもワクチン接種率を高めている。
#' 厚労省メッセージの抗体検査の受検率は2.5%であり、
#' 利他強調メッセージと低コストメッセージの受検率はそれぞれ5.7%と6.8%である。
#' したがって、利他強調メッセージと低コストメッセージの抗体検査の受検率に対する効果はそれぞれ
#' 3.2%ポイント・4.3%ポイントであり、
#' これらは統計的に有意である。
#' また、厚労省メッセージのワクチン接種率は1.7%であり、
#' 低コストメッセージの受検率は5%である。
#' したがって、低コストメッセージのワクチン接種率に対する効果は3.3%ポイントであり、
#' これは統計的に10%水準で有意である。
#'
#' <!---
#' //NOTE: 回帰分析
#' --->
#'
#+ act2-reg
act2$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = paste(
      "Linear Probability Model of Behaviors",
      "Using New Wave 2 Selection Data"
    ),
    outcome_map = c(
      "abw1_test" = "Antibody Test",
      "abw1_testvaccine" = "Vaccination"
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

#+ act2-reg-ftest
covlist <- paste(all.vars(covmod)[8:length(all.vars(covmod))], collapse = "+")

act2mod <- list(
  "(1)" = update(
    abw1_test ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  ),
  "(2)" = update(
    abw1_testvaccine ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  )
)

est_act2mod <- act2mod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = act2$data, se_type = "HC0",
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
      levels = c("abw1_test", "abw1_testvaccine"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[1:7], collapse = "|")),
    nudge = factor(nudge, labels = treat_labels[-1])
  )

rawvalue <- function(x) x

est_act2mod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates",
      "(Data: New Wave 2 Selection Data)"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

#+ act2-reg-ftest2, eval = FALSE
est_act2mod2 <- act2mod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = act2$data, se_type = "HC0",
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
      levels = c("abw1_test", "abw1_testvaccine"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[c(2, 4:7)], collapse = "|")),
    # nudge = if_else(is.na(nudge), "A", nudge),
    nudge = factor(nudge, labels = treat_labels[c(2, 4:7)])
  )

est_act2mod2 %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates",
      "(Baseline: Altruistic Message, Data: New Wave 2 Selection Data)"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

#'
#' サブサンプルで推定されたナッジ・メッセージの効果は
#' クーポン券が自動的に送付されるかどうかだけでなく、
#' 年齢の違いの影響を受けるので、
#' 我々はこの問題を排除するために線形確率モデルを推定した。
#' 基本的に、二群の平均値の差の検定の結果と整合的である。
#' それに加えて、表\@ref(tab:act2-reg-ftest)より、
#' 2019年度にクーポン券を自動的に受け取った男性における
#' 利己強調メッセージの抗体検査受検率に対する効果は6.2%ポイントであり、
#' これは統計的に10%水準で有意である。
#' また、2019年度にクーポン券を受け取るためにはコストのかかる手続きが必要な男性における
#' 利他強調メッセージの抗体検査受検率に対する効果は
#' 効果の規模が変化していないにも関わらず、統計的に非有意である。
#' さらに、表\@ref(tab:act2-reg)より、
#' クーポン券を自動的に送付されるかどうかによる
#' ナッジ・メッセージの効果の異質性は統計的に非有意である。
#'
#' <!--
#' //NOTE: 抗体検査受検者の動き（クーポンあり）
#' -->
#'
#+ include = FALSE
tester21 <- create_RCTtoolbox(
  abw1_testnega ~ nudge,
  data = subset(act2$data, abw1_test == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

chi2test211 <- tester21$
  chi2test(subset = coupon2019 == 1, fisher = TRUE, bootp = TRUE)$
  result

chi2test210 <- tester21$
  chi2test(subset = coupon2019 == 0, fisher = TRUE, bootp = TRUE)$
  result

tester22 <- create_RCTtoolbox(
  abw1_testvaccine ~ nudge,
  data = subset(act2$data, abw1_testnega == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

chi2test221 <- tester22$
  chi2test(subset = coupon2019 == 1, fisher = TRUE, bootp = TRUE)$
  result

chi2test220 <- tester22$
  chi2test(subset = coupon2019 == 0, fisher = TRUE, bootp = TRUE)$
  result


chi2test_result_tab2 <- tribble(
  ~terms, ~test1, ~negative1, ~vaccine1, ~test0, ~negative0, ~vaccine0,
  "Fisher's exact test (p-value)",
  "",
  sprintf("%1.2f", chi2test211$result$abw1_testnega$p),
  sprintf("%1.2f", chi2test221$result$abw1_testvaccine$p),
  "",
  sprintf("%1.2f", chi2test210$result$abw1_testnega$p),
  sprintf("%1.2f", chi2test220$result$abw1_testvaccine$p)
)

#+ tester2-move
tab2 <- act2$data %>%
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
      (`Antibody test` = abw1_test) +
        (`Negative test result` = abw1_testnega) +
        (`Vaccination` = abw1_testvaccine)
    ),
    title = paste(
      "Movement of Antibody Test Takers",
      "(Data: New Wave 2 Selection Data)"
    ),
    data = .,
    fmt = 0,
    align = "lcccccc",
    add_rows = chi2test_result_tab2,
  )

if (out == "kableExtra") {
  tab2 %>%
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

#'
#' 表\@ref(tab:tester2-move)は各介入群の抗体検査の受検者数・陰性件数・ワクチン接種件数を示した。
#' 本論と同様に、
#' クーポン券を自動的に受け取ったかどうか・介入群に関わらず、
#' 抗体検査の結果が陰性である人のほとんどがワクチンを接種している。
#' よって、ワクチン接種率に対するナッジ・メッセージの効果は介入群の陰性比率に強く依存している。
#' 事実、手続きが必要な男性に限定したとき、
#' 低コストメッセージの陰性比率は87%($=13/15$)と非常に高い。
#' その結果、低コストメッセージはワクチン接種率に統計的に有意な効果を持っている。
#' しかしながら、介入群間の陰性比率のバラツキは統計的な誤差である可能性が高い。
#' 我々は抗体検査の受検者をクーポン券が自動的に送付されるかどうかで分割し、
#' 抗体検査の陰性件数が介入群間で同じであるという帰無仮説をフィッシャーの正確検定で検証した。
#' その結果、二つのサブサンプルで帰無仮説を棄却できない。
#'
# /*
#+
rmarkdown::render(
  "src/8-selection2-behavior.r",
  output_dir = "report/view/",
  knit_root_dir = here::here()
)
# */