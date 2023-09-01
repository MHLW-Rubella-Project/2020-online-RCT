library(R6)
source("R/misc.r")
source("R/R6_Regression.r")
source("R/R6_MonetaryValue.r")

EstimateEffect <- R6::R6Class("EstimateEffect",
  public = list(
    wave1 = NULL,
    wave2 = NULL,
    ttest_res = list(
      default = list(intention = NULL, behavior = NULL),
      optin = list(intention = NULL, behavior = NULL)
    ),
    initialize = function(wave1, wave2, treat_labels, control) {
      self$wave1 <- wave1
      self$wave2 <- wave2
      private$treat_labels <- treat_labels
      private$covs <- control
    },
    balance_control = function(
      outcome_intention = TRUE,
      default_voucher = TRUE,
      title = "",
      notes = ""
    ) {
      data <-private$choose_wave(outcome_intention)
      use <- private$subset_tickets(data, !default_voucher)
      use_covs <- private$noNA_control(private$covs, use)

      mu_table <- use %>%
        group_by(nudge) %>%
        summarize_at(use_covs, list(~ mean(.))) %>%
        mutate(nudge = factor(nudge, labels = private$treat_labels)) %>%
        pivot_longer(-nudge) %>%
        pivot_wider(names_from = nudge, values_from = value)

      p <- mu_table$name %>%
        sapply(function(x) {
          mod <- reformulate("nudge", x)
          est <- lm_robust(mod, data = use)
          f <- est$fstatistic
          pf(f[1], f[2], f[3], lower.tail = FALSE)
        })

      p_table <- tibble(
        name = str_remove(names(p), ".value"),
        "F-test, p-value" = p,
      )

      tbl <- mu_table %>%
        left_join(p_table, by = "name")

      tbl %>%
        knitr::kable(
          caption = title,
          digits = 3,
          col.names = c("", colnames(tbl)[-1]),
          align = "lcccccccc",
          booktabs = TRUE,
          linesep = ""
        ) %>%
        kableExtra::kable_styling(
          font_size = 9,
          latex_options = "hold_position"
        ) %>%
        kableExtra::column_spec(2:8, width = "3em") %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    power = function(
      outcome_intention = TRUE,
      default_voucher = TRUE,
      alpha = 0.05,
      power = 0.8
    ) {
      data <- private$choose_wave(outcome_intention)
      use <- private$subset_tickets(data, !default_voucher)
      obs <- with(use, table(nudge))

      LETTERS[2:7] %>%
        sapply(function(i) {
          uniroot(
            diff_power,
            c(0, 10),
            n0 = obs["A"],
            n1 = obs[i],
            alpha = alpha,
            power = power
          )$root
        })
    },
    ttest = function(
      outcome_intention = TRUE,
      default_voucher = TRUE,
      label_y_pos = 0.6,
      y_lim_max = 1
    ) {
      use <- private$use_data(outcome_intention, default_voucher)

      stats <- use %>%
        group_by(nudge) %>%
        summarize_at(
          vars(starts_with("outcome")),
          list(
            mu = ~ mean(.),
            se = ~ se(.)
          )
        ) %>%
        pivot_longer(
          -nudge,
          names_prefix = "outcome_",
          names_to = c("outcome", ".value"),
          names_pattern = "(.*)_(.*)"
        )

      nudge <- use$nudge
      test <- use$outcome_test
      vacc <- use$outcome_vacc

      t_test <- LETTERS[1:7] %>%
        sapply(function(i) t.test(test[nudge == "A"], test[nudge == i])$p.value)

      t_vacc <- LETTERS[1:7] %>%
        sapply(function(i) t.test(vacc[nudge == "A"], vacc[nudge == i])$p.value)

      t_res <- tibble(
        nudge = c(names(t_test), names(t_vacc)),
        outcome = c(rep("test", length(t_test)), rep("vacc", length(t_vacc))),
        p = c(t_test, t_vacc)
      )

      outcome_labels <- if (outcome_intention) {
        c("A. Antibody Testing (Intention)", "B. Vaccination (Intention)")
      } else {
        c("A. Antibody Testing (Behavior)", "B. Vaccination (Behavior)")
      }

      plot_data <- stats %>%
        left_join(t_res, by = c("nudge", "outcome")) %>%
        mutate(
          nudge = factor(nudge, labels = private$treat_labels),
          outcome = factor(
            outcome,
            levels = c("test", "vacc"),
            labels = outcome_labels
          ),
          label = case_when(
            p <= 0.01 ~ sprintf("%1.3f***", mu),
            p <= 0.05 ~ sprintf("%1.3f**", mu),
            p <= 0.1 ~ sprintf("%1.3f*", mu),
            TRUE ~ sprintf("%1.3f", mu)
          )
        )
      
      if (default_voucher) {
        if (outcome_intention) {
          self$ttest_res$default$intention <- plot_data
        } else {
          self$ttest_res$default$behavior <- plot_data
        }
      } else {
        if (outcome_intention) {
          self$ttest_res$optin$intention <- plot_data
        } else {
          self$ttest_res$optin$behavior <- plot_data
        }
      }
      
      plot_data %>%
        ggplot(aes(x = fct_rev(nudge), y = mu, ymin = mu - se, ymax = mu + se)) +
        geom_hline(aes(yintercept = 0)) +
        geom_bar(stat = "identity", fill = "grey80", color = "black") +
        geom_errorbar(width = 0.5) +
        geom_text(aes(y = label_y_pos, label = label), size = 5) +
        labs(x = "Treatments", y = "Proportion") +
        facet_wrap(~outcome, ncol = 1, scales = "free") +
        scale_y_continuous(limits = c(0, y_lim_max), breaks = seq(0, 1, by = 0.1)) +
        coord_flip() +
        simplegg(flip = TRUE)
    },
    lm = function(outcome_intention = TRUE, exclude_A = FALSE) {
      dta <- private$choose_wave(outcome_intention)
      dta <- if (!exclude_A) dta else subset(dta, nudge != "A")
      covariate <- private$noNA_control(private$covs, dta)
      Regression$new(dta, covariate, private$treat_labels)
    },
    monetary_value = function() {
      MonetaryValue$new(self$wave2, self$ttest_res$default$behavior)
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c(),
    choose_wave = function(intention = TRUE) if (intention) self$wave1 else self$wave2,
    choose_outcome = function(data, test = TRUE) {
      if (test) with(data, outcome_test) else with(data, outcome_vacc)
    },
    subset_tickets = function(data, opt_in = TRUE) {
      val_coupon2019 <- ifelse(!opt_in, 1, 0)
      subset(data, coupon2019 == val_coupon2019)
    },
    noNA_control = function(covs, data) {
      data %>%
        summarise_at(covs, list(~ sum(is.na(.)))) %>%
        pivot_longer(everything()) %>%
        dplyr::filter(value == 0) %>%
        .$name
    }
  )
)
