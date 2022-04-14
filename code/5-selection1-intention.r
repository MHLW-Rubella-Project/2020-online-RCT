#+ include = FALSE
library(here)
source(here("code/_common.r"), encoding = "utf8")

#+ include = FALSE
wave1 <- rct_data_wave1(here(rct_path, "shape_survey.csv"))

#+ include = FALSE
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

#' <!---
#' //NOTE: 意向に対する効果のt検定
#' --->
#+ int-coupon1-ttest, eval = FALSE
int$
  ttest(subset = coupon2019 == 1)$
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

#+ int-coupon0-ttest, eval = FALSE
int$
  ttest(subset = coupon2019 == 0)$
  plot(
  xlab = "介入群",
  ylab = "比率（+/- 標準誤差）",
  title = switch(outcome,
    "test_int" = "A. 抗体検査（意向）",
    "vaccine_int" = "B. ワクチン接種（意向）"
  ),
  inplot_lab_adjust = 0.1,
  flip = TRUE,
  ylim = c(0, 1),
  ncol = 1
)

#' <!---
#' //NOTE: 意向に対する効果の回帰分析
#' --->
#+ int-reg, eval = FALSE
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

#+ int-reg-ftest, eval = FALSE
covlist <- paste(all.vars(covmod)[8:length(all.vars(covmod))], collapse = "+")

intmod <- list(
  "(1)" = update(
    test_int ~ nudge * coupon2019,
    formula(paste(". ~ . +", covlist))
  ),
  "()" = update(
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
      labels = c(
        "w/o receiving coupon automatically",
        "w/ receiving coupon automatically"
      )
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
    nudge ~ outcome * coupon * rawvalue * (estimate + std.error + p.value),
    data = .,
    fmt = 3
  )
