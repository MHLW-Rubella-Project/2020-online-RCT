#' ---
#' title: Effect of Text-Based Nudges on Intentions
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave1 <- rct_data_wave1(here(rct_path, "shape_survey.csv"))

covmod <- ~ coupon2019 +
  coupon_b + coupon_c + coupon_d + coupon_e +
  coupon_f + coupon_g +
  age + married + education +
  income + noinfo_income +
  exercise_w1 + health_check + flushot #+
  # prob_social + handicap + severity +
  # handwash + temp_check + avoid_out + avoid_crowd + wear_mask,

int <- create_RCTtoolbox(
  test_int + vaccine_int ~ nudge,
  covmod,
  data = wave1,
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#' <!---
#' //NOTE: 検出力分析
#' --->
#'
#+ int-power
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

round(min_int_ef1 * 100, 1)
round(min_int_ef0 * 100, 1)

#'
#' <!---
#' //NOTE: 意向に対する効果のt検定
#' --->
#'
#+ int-coupon1-ttest, fig.cap = "Effect of Text-Based Nudges on Intentions among Men for whom Coupons are Automatically Distributed in FY 2019 (N = 927). Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_int1 <- int$
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

ggsave(
  here("figures", "int-coupon1-ttest.pdf"),
  plot = plot_int1,
  width = 10,
  height = 6
)

#'
#+ int-coupon0-ttest, eval = params$preview | !params$appendix, fig.cap = "Effect of Text-Based Nudges on Intentions among Men Who Needed Costly Procedures to Receive Coupons in FY 2019 (N = 1,688). Data source: wave 1 selection data. Note: Numbers in the figure indicate the proportion of each group. Error bars indicate standard error of the mean. Asterisks are p-values for t-tests of the difference in means from the MHLW message group: * p < 0.1, ** p < 0.05, *** p < 0.01.", out.extra = ""
plot_int0 <- int$
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

ggsave(
  here("figures", "int-coupon0-ttest.pdf"),
  plot = plot_int0,
  width = 10,
  height = 6
)

#'
#' <!---
#' //NOTE: 意向に対する効果の回帰分析
#' --->
#+ int-reg
out.file <- file(here("tables", "int-reg.tex"), open = "w")

tab <- int$
  lm(se_type = "HC0", only_dmod = FALSE)$
  table(
    title = "Linear Probability Model of Intentions \\label{tab:int-reg}",
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
    " " = 1, "Antibody Test" = 1, "Vaccination" = 1
  )) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note: * p < 0.1, ** p < 0.05, *** p < 0.01.",
      "We use robust standard errors.",
      "We also control for covariates obtained in wave 1.",
      "The list of covariates is presented in Table \\@ref(tab:covlist)."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file, useBytes = TRUE)
close(out.file)

#+ int-reg-ftest
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

out.file <- file(here("tables", "int-reg-ftest.tex"), open = "w")

tab <- est_intmod %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Intentions",
      "Using Linear Probability Model Estimates",
      "\\label{tab:int-reg-ftest}"
    ),
    fmt = 3,
    align = "llcccccc",
    output = "latex"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

writeLines(tab, out.file)
close(out.file)

#+ int-reg-ftest2
est_intmod2 <- intmod %>%
  lapply(function(x) {
    lh_robust(
      x,
      data = int$data, se_type = "HC0",
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
      levels = c("test_int", "vaccine_int"),
      labels = c("Antibody Test", "Vaccination")
    ),
    nudge = str_extract(term, paste(LETTERS[c(2, 4:7)], collapse = "|")),
    # nudge = if_else(is.na(nudge), "A", nudge),
    nudge = factor(nudge, labels = treat_labels[c(2, 4:7)])
  )

out.file <- file(here("tables", "int-reg-ftest2.tex"), open = "w")

tab <- est_intmod2 %>%
  datasummary(
    (`How to get coupons` = coupon) *
      (`Text-based nudges` = nudge) ~ outcome * rawvalue *
      (estimate + std.error + p.value),
    data = .,
    title = paste(
      "Effects of Text-Based Nudges on Intentions",
      "Using Linear Probability Model Estimates",
      "(Baseline: Altruistic Message)",
      "\\label{tab:int-reg-ftest2}"
    ),
    fmt = 3,
    align = "llcccccc",
    output = "latex"
  ) %>%
  kableExtra::column_spec(1, width = "5em")

writeLines(tab, out.file)
close(out.file)
