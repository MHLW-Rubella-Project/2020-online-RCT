#' ---
#' title: Effect of Text-Based Nudges on Behavior
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
  income + noinfo_income +
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
#+ act-power
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

#'
#' <!---
#' //NOTE: 行動に対する効果のt検定
#' --->
#'
#+ act-coupon1-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Behavior among Men for whom Coupons are Automatically Distributed in FY 2019 (N = 805). Data source: wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_act1 <- act$
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

ggsave(
  here("figures", "act-coupon1-ttest.pdf"),
  plot = plot_act1,
  width = 10,
  height = 6
)

#'
#+ act-coupon0-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Behaviors among Men Who Needed Costly Procedures to Receive Coupons in FY 2019 (N = 1,467). Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_act0 <- act$
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

ggsave(
  here("figures", "act-coupon0-ttest.pdf"),
  plot = plot_act0,
  width = 10,
  height = 6
)

#'
#' <!---
#' //NOTE: 行動に対する効果の回帰分析
#' --->
#+ act-reg
out.file <- file(here("tables", "act-reg.tex"), open = "w")

tab <- act$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = "Linear Probability Model of Behaviors \\label{tab:act-reg}",
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
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(
    "Antibody Test" = 1, "Vaccination" = 1
  )) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use robust standard errors.",
      "We also control for covariates obtained in wave 1 and 2.",
      "The list of covariates is presented in Table \\@ref(tab:covlist)."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file, useBytes = TRUE)
close(out.file)

#+ act-reg-ftest
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

out.file <- file(here("tables", "act-reg-ftest.tex"), open = "w")

tab <- est_actmod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates",
      "\\label{tab:act-reg-ftest}"
    ),
    fmt = 3,
    align = "llcccccc",
    output = "latex"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

writeLines(tab, out.file)
close(out.file)

#+ act-reg-ftest2
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

out.file <- file(here("tables", "act-reg-ftest2.tex"), open = "w")

tab <- est_actmod2 %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Behaviors",
      "Using Linear Probability Model Estimates",
      "(Baseline: Altruistic Message)",
      "\\label{tab:act-reg-ftest2}"
    ),
    fmt = 3,
    align = "llcccccc",
    output = "latex"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

writeLines(tab, out.file)
close(out.file)

#'
#+ tester-move, include = FALSE
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

out.file <- file(here("tables", "tester-move.tex"), open = "w")

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
    title = "Movement of Antibody Test Takers \\label{tab:tester-move}",
    data = .,
    fmt = 0,
    align = "lcccccc",
    add_rows = chi2test_result_tab,
    output = "latex"
  ) %>%
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

writeLines(tab, out.file)
close(out.file)

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
