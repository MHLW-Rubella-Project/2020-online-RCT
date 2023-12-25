library(here)
library(R6)
library(tidyverse)
library(modelsummary)
library(kableExtra)
source(here("R/R6_EstimateEffect.r"))

StartAnalysis <- R6::R6Class("StartAnalysis",
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
        arrange(match(vars, private$covs)) %>%
        select(Description)
      
      attr(description, "position") <- 2

      paste(private$covs, collapse = "+") %>%
        paste("~ Mean + (`Std.Dev.` = SD)") %>%
        as.formula() %>%
        datasummary(
          data = self$data,
          add_columns = description,
          align = "llcc",
          title = "List of the Covariates"
        ) %>%
        kableExtra::kable_styling(
          font_size = 9, latex_options = "hold_position"
        ) %>%
        kableExtra::column_spec(2, width = "30em")
    },
    summary_assign = function(message_path, notes = "") {
      message_list <- read_csv(message_path, locale = locale(encoding = "cp932")) %>%
        select(Content)

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
          title = "List of the Text Message Reminders",
          linesep = "\\addlinespace"
        ) %>%
        kableExtra::kable_styling(font_size = 9) %>%
        kableExtra::column_spec(2, width = "20em") %>%
        kableExtra::add_header_above(
          c(" " = 3, "Age (as of April 2019)" = 4, " " = 1)
        ) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    balance_attrition = function() {
      est <- anova(lm(I(1 - follow) ~ nudge, data = self$data))
      f <- est[["F value"]][1]
      p <- est[["Pr(>F)"]][1]
      sprintf("F-value = %1.3f (p-value = %1.3f)", f, p)
    },
    main = function() {
      dt <- self$data %>%
        mutate(
          aw1_negative = if_else(act_vaccine != 4, 1, 0),
          aw1_test_negative = aw1_test * aw1_negative
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
    },
    robust = function() {
      dt <- self$data %>%
        mutate(
          aw1_negative = if_else(act_vaccine != 4, 1, 0),
          abw1_test_negative = abw1_test * aw1_negative
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
        filter(exp_antibody != 1 & exp_vaccine != 1) %>%
        rename(
          outcome_test = abw1_test,
          outcome_vacc = abw1_testvaccine
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