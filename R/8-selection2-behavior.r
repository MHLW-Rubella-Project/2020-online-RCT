#' ---
#' title: |
#'   Analysis to Address Recall Bias Associated with Self-Reporting of Behavior
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave22 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 2)

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
  income + noinfo_income +
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

#' <!---
#' //NOTE: t検定
#' --->
#'
#+ act2-coupon1-ttest, fig.cap = "Effect of Text-Based Nudges on Behavior among Men for whom Coupons are Automatically Distributed in FY 2019 (N = 881). Data source: new wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_act1 <- act2$
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

ggsave(
  here("figures", "act2-coupon1-ttest.pdf"),
  plot = plot_act1,
  width = 10,
  height = 6
)

#+ act2-coupon0-ttest, fig.cap = "Effect of Text-Based Nudges on Behaviors among Men Who Needed Costly Procedures to Receive Coupons in FY 2019 (N = 1,578). Data source: new wave 2 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_act0 <- act2$
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

ggsave(
  here("figures", "act2-coupon0-ttest.pdf"),
  plot = plot_act0,
  width = 10,
  height = 6
)

#'
#' <!---
#' //NOTE: 回帰分析
#' --->
#'
#+ act2-reg
out.file <- file(here("tables", "act2-reg.tex"), open = "w")

tab <- act2$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = paste(
      "Linear Probability Model of Behaviors",
      "\\label{tab:act2-reg}"
    ),
    add_coef_map = c(
      "coupon2019" = "Coupon",
      "coupon_b" = "Coupon\u00d7MHLW (Age)",
      "coupon_c" = "Coupon\u00d7Altruistic",
      "coupon_d" = "Coupon\u00d7Selfish",
      "coupon_e" = "Coupon\u00d7Social Comparison",
      "coupon_f" = "Coupon\u00d7Deadline",
      "coupon_g" = "Coupon\u00d7Convenient"
    ),
    not_show_x = list(
      "Covariates" = all.vars(covmod)[8:length(all.vars(covmod))]
    ),
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::add_header_above(c(
    " " = 1, "Antibody Test" = 1, "Vaccination" = 1
  )) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use robust standard errors.",
      "We also control for covariates obtained in wave 1 and 2.",
      "The list of covariates is presented in",
      "Table \\\\ref{tab:tab:covariate-list}."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file, useBytes = TRUE)
close(out.file)

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
      labels = c("Opt-in incentive", "Default incentive")
    ),
    outcome = factor(outcome,
      levels = c("abw1_test", "abw1_testvaccine"),
      labels = c("Antibody testing", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[1:7], collapse = "|")),
    nudge = factor(nudge, labels = treat_labels[-1])
  )

rawvalue <- function(x) x

out.file <- file(here("tables", "act2-reg-ftest.tex"), open = "w")

tab <- est_act2mod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text Messages on Behaviors for Two Groups",
      "Using Linear Probability Model Estimates",
      "\\label{tab:act2-reg-ftest}"
    ),
    fmt = 3,
    align = "llcccccc",
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::column_spec(1, width = "5em") %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: We estimate the effect for the default incentive group",
      "(men aged 40-46) and the opt-in incentive group (men aged 47-57)",
      "using results of the linear probability model presented in",
      "\\\\ref{tab:act2-reg}.",
      "The effect for the opt-in incentive group is the estimate $\\\\beta_j$.",
      "The effect for the default incentive group is a linear combination",
      "of the estimates, $\\\\beta_j + \\\\gamma_j$.",
      "We use the F-test for the null hypothesis of the linear combination.",
      "We use robust standard errors."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)

#' //NOTE: Not evaluate this chunk
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
      labels = c("Opt-in incentive", "Default incentive")
    ),
    outcome = factor(outcome,
      levels = c("abw1_test", "abw1_testvaccine"),
      labels = c("Antibody testing", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[c(2, 4:7)], collapse = "|")),
    # nudge = if_else(is.na(nudge), "A", nudge),
    nudge = factor(nudge, labels = treat_labels[c(2, 4:7)])
  )

est_act2mod2 %>%
  datasummary(
    (`Group` = coupon) *
      (`Text messages` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text Messages on Behaviors for Two Groups",
      "Using Linear Probability Model Estimates",
      "(Baseline: Altruistic Message)"
    ),
    fmt = 3,
    align = "llcccccc"
  ) %>%
  kableExtra::kable_styling(font_size = 9) %>%
  kableExtra::column_spec(1, width = "5em") %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: We estimate the effect for the default incentive group",
      "(men aged 40-46) and the opt-in incentive group (men aged 47-57)",
      "using results of the linear probability model presented in",
      "\\\\ref{tab:act2-reg}.",
      "The effect for the opt-in incentive group is a linear combination",
      "of the estimates, $\\\\beta_j - \\\\beta_{\\\\text{Altruistic}}$.",
      "The effect for the default incentive group is a linear combination",
      "of the estimates, $\\\\beta_j + \\\\gamma_j -",
      "(\\\\beta_{\\\\text{Altruistic}} + \\\\gamma_{\\\\text{Altruistic}})$.",
      "We use the F-test for the null hypothesis of the linear combination.",
      "We use robust standard errors."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

#'
#' <!--
#' //NOTE: 抗体検査受検者の動き（クーポンあり）
#' -->
#'
#+ tester2-move, include = FALSE
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

out.file <- file(here("tables", "tester2-move.tex"), open = "w")

tab2 <- act2$data %>%
  mutate(coupon2019 = factor(
    coupon2019,
    levels = c(1, 0),
    labels = c(
      "Default incentive group",
      "Opt-in incentive group"
    )
  )) %>%
  mutate(nudge = factor(nudge, labels = treat_labels)) %>%
  datasummary(
    (`Text messages` = nudge) ~ coupon2019 * sum * (
      (`Antibody test` = abw1_test) +
        (`Negatives` = abw1_testnega) +
        (`Vaccination` = abw1_testvaccine)
    ),
    title = paste(
      "Classification of Antibody Test Takers",
      "\\label{tab:tester2-move}"
    ),
    data = .,
    fmt = 0,
    align = "lcccccc",
    add_rows = chi2test_result_tab2,
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
      "among experimental arms with Fisher's exact test.",
      "Also, restricting our sample to the negatives,",
      "we tested the null hypothesis that",
      "the number of vaccinations would not differ",
      "among experimental arms with a Fisher's exact test."
    ),
    notation = "none",
    threeparttable = TRUE
  )

writeLines(tab2, out.file)
close(out.file)
