library(here)
library(R6)
library(tidyverse)
library(modelsummary)
library(kableExtra)
source(here("R/R6_EstimateEffect.r"))

SetData <- R6::R6Class("SetData",
  public = list(
    data = NULL,
    initialize = function(path) self$data <- read_csv(path),
    add_control = function(x) {
      private$covs <- x
      invisible(private$covs)
    },
    summary_control = function(description_path) {
      description <- read_csv(description_path, locale = locale(encoding = "cp932")) %>%
        dplyr::filter(vars %in% private$covs) %>%
        select(Description)
      
      attr(description, "position") <- 2

      paste(private$covs, collapse = "+") %>%
        paste("~ Mean + (`Std.Dev.` = SD)") %>%
        as.formula() %>%
        datasummary(
          data = self$data,
          add_columns = description,
          align = "llcc",
          title = "List of Covariates \\label{tab:covariate-list}",
          output = "latex"
        ) %>%
        kableExtra::kable_styling(
          font_size = 9, latex_options = "hold_position"
        ) %>%
        kableExtra::column_spec(2, width = "30em")
    },
    summary_assign = function(message_path) {
      message_list <- read_csv(message_path, locale = locale(encoding = "cp932")) %>%
        select(Contents)

      attr(message_list, "position") <- 2

      self$data %>%
        mutate(age_group = case_when(
          age == 39 ~ "39",
          age <= 46 ~ "40--46",
          age <= 56 ~ "47--56",
          age <= 59 ~ "57--59"
        )) %>%
        mutate(nudge = factor(nudge, labels = private$treat_labels)) %>%
        rename(Message = nudge) %>%
        datasummary_crosstab(
          Message ~ age_group,
          statistic = ~ 1 + N,
          add_columns = message_list,
          align = "llcccccc",
          data = .,
          title = "List of Text Message Reminders \\label{tab:nudge-list}",
          linesep = "\\addlinespace",
          output = "latex"
        ) %>%
        kableExtra::kable_styling(font_size = 9) %>%
        kableExtra::column_spec(2, width = "20em") %>%
        kableExtra::add_header_above(
          c(" " = 3, "Age (as of Apr 2019)" = 4, " " = 1)
        )
    },
    balance_attrition = function() {
      est <- anova(lm(I(1 - follow) ~ nudge, data = self$data))
      f <- est[["F value"]][1]
      p <- est[["Pr(>F)"]][1]
      sprintf("F-value = %1.3f (p-value = %1.3f)", f, p)
    },
    main_analysis = function() {
      dt <- self$data %>%
        mutate(
          aw1_negative = if_else(act_vaccine != 4, 1, 0),
          aw1_test_negative = aw1_test * aw1_negative,
          coupon_a = coupon2019 * as.numeric(nudge == "A"),
          coupon_b = coupon2019 * as.numeric(nudge == "B"),
          coupon_c = coupon2019 * as.numeric(nudge == "C"),
          coupon_d = coupon2019 * as.numeric(nudge == "D"),
          coupon_e = coupon2019 * as.numeric(nudge == "E"),
          coupon_f = coupon2019 * as.numeric(nudge == "F"),
          coupon_g = coupon2019 * as.numeric(nudge == "G")
        )
      
      wave1 <- dt %>%
        filter(40 <= age & age <= 56) %>%
        filter(exp_antibody != 1 & exp_vaccine != 1) %>%
        rename(
          outcome_test = test_int,
          outcome_vacc = vaccine_int
        )
      
      wave2 <- dt %>%
        filter(40 <= age & age <= 56) %>%
        filter(follow == 1) %>%
        filter(
          exp_antibody != 1 &
          exp_vaccine != 1 &
          act_test != 2 &
          act_vaccine != 2
        ) %>%
        rename(
          outcome_test = aw1_test,
          outcome_vacc = aw1_testvaccine
        )
      
      EstimateEffect$new(wave1, wave2, private$treat_labels, private$covs)
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c(
      "MHLW (Control)",
      "MHLW (Age)",
      "Altruistic",
      "Selfish",
      "Social Comparison",
      "Deadline",
      "Convenient"
    )
  )
)

test <- SetData$new(here("data/2020-online-survey/shape_survey.csv"))

cov <- c(
  "age", "married", "education",
  "income", "noinfo_income",
  "exercise_w1", "health_check", "flushot",
  "handwash", "temp_check", "avoid_out", "avoid_crowd", "wear_mask"
)

test$add_control(cov)
test$summary_control(here("assets/vars_descript.csv"))
test$summary_assign(here("assets/nudge_descript.csv"))
test$balance_attrition()

analysis <- test$main_analysis()
analysis$balance_control(outcome_intention = FALSE)
analysis$power(outcome_intention = TRUE, default_voucher = FALSE)
analysis$ttest(default_voucher = FALSE)

reg <- analysis$lm(exclude_A = TRUE)
reg$reg_tab()
reg$lh_tab()