library(R6)
source("R/misc.r")
source("R/R6_Regression.r")
source("R/R6_MonetaryValue.r")
source("R/R6_Mechanism.r")

EstimateEffect <- R6::R6Class("EstimateEffect",
  public = list(
    wave1 = NULL,
    wave2 = NULL,
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
    summary_behavior = function(
      default_voucher = TRUE,
      title = "",
      notes = ""
    ) {
      use <- private$subset_tickets(self$wave2, !default_voucher)

      summary_tbl <- use %>%
        group_by(nudge) %>%
        summarize(
          n = n(),
          test = sum(outcome_test),
          prop_test = 100 * test / n,
          negative = test - sum(aw1_testhave),
          cond_prop_negative = 100 * negative / test,
          prop_negative = 100 * negative / n,
          vaccine = sum(outcome_vacc),
          cond_prop_vaccine = 100 * vaccine / negative,
          prop_vaccine = 100 * vaccine / n
        ) %>%
        ungroup() %>%
        mutate(nudge = factor(nudge, labels = private$treat_labels))
      
      summary_tbl %>%
        knitr::kable(
          caption = title,
          col.names = c(
            "Text message",
            "Sample size",
            "N",
            "% of sample",
            "N",
            "% of test",
            "% of sample",
            "N",
            "% of negatives",
            "% of sample"
          ),
          digits = 1,
          align = "lccccccccc",
          booktabs = TRUE,
          linesep = ""
        ) %>%
        kable_styling(font_size = 9) %>%
        add_header_above(c(" " = 2, "Anitbody test" = 2, "Negatives" = 3, "Vaccination" = 3)) %>%
        kableExtra::footnote(
          general_title = "",
          general = notes,
          threeparttable = TRUE,
          escape = FALSE
        )
    },
    ttest = function(
      outcome_intention = TRUE,
      outcome_test = TRUE,
      label_y_pos = 60,
      y_lim_max = label_y_pos + 10,
      breaks_by = 10
    ) {
      data <- private$choose_wave(outcome_intention)
      outcome_label <- if (outcome_test) "outcome_test" else "outcome_vacc"
      use <- data[, c(outcome_label, "nudge", "coupon2019")]
      names(use) <- c("outcome", "nudge", "coupon2019")
      use$outcome <- use$outcome * 100

      stats <- use %>%
        group_by(coupon2019, nudge) %>%
        summarize(
          mu = mean(outcome),
          se = se(outcome)
        )

      x <- use$nudge
      y <- use$outcome
      default <- use$coupon2019

      t_optin <- LETTERS[1:7] %>%
        sapply(function(i) t.test(y[x == "A" & default == 0], y[x == i & default == 0])$p.value)

      t_default <- LETTERS[1:7] %>%
        sapply(function(i) t.test(y[x == "A" & default == 1], y[x == i & default == 1])$p.value)

      t_res <- tibble(
        nudge = c(names(t_optin), names(t_default)),
        coupon2019 = c(rep(0, length(t_optin)), rep(1, length(t_default))),
        p = c(t_optin, t_default)
      )

      coupon2019_labels <- c(
        sprintf("A. Default incentive group (N = %1d)", sum(default == 1)),
        sprintf("B. Opt-in incentive group (N = %1d)", sum(default == 0))
      )

      plot_data <- stats %>%
        left_join(t_res, by = c("nudge", "coupon2019")) %>%
        mutate(
          nudge = factor(nudge, labels = private$treat_labels),
          coupon2019 = factor(coupon2019, levels = c(1, 0), labels = coupon2019_labels),
          label = case_when(
            p <= 0.01 ~ sprintf("%1.1f%%***", mu),
            p <= 0.05 ~ sprintf("%1.1f%%**", mu),
            p <= 0.1 ~ sprintf("%1.1f%%*", mu),
            TRUE ~ sprintf("%1.1f%%", mu)
          )
        )
      
      if (outcome_intention == FALSE & outcome_test == TRUE) {
        private$ttest_for_value <- subset(plot_data, coupon2019 == levels(plot_data$coupon2019)[1])
      }
      
      plot_data %>%
        ggplot(aes(x = fct_rev(nudge), y = mu, ymin = mu - se, ymax = mu + se)) +
        geom_hline(aes(yintercept = 0)) +
        geom_bar(stat = "identity", fill = "grey80", color = "black") +
        geom_errorbar(width = 0.5) +
        geom_text(aes(y = label_y_pos, label = label), size = 5) +
        labs(x = "Treatments", y = "Proportion (%)") +
        facet_wrap(~coupon2019, ncol = 1, scales = "free") +
        scale_y_continuous(limits = c(0, y_lim_max), breaks = seq(0, 100, by = breaks_by)) +
        coord_flip() +
        theme_classic(base_size = 15) +
        theme(
          strip.background = element_blank(),
          strip.text = element_text(size = 16, hjust = 0)
        )
    },
    lm = function(outcome_intention = TRUE, exclude_A = FALSE) {
      dta <- private$choose_wave(outcome_intention)
      dta <- if (!exclude_A) dta else subset(dta, nudge != "A")
      covariate <- private$noNA_control(private$covs, dta)
      Regression$new(dta, covariate, private$treat_labels)
    },
    monetary_value = function() {
      if (is.null(private$ttest_for_value)) stop("Run ttest(outcome_intention = FALSE)")
      MonetaryValue$new(self$wave2, private$ttest_for_value)
    }
  ),
  private = list(
    covs = c(),
    treat_labels = c(),
    ttest_for_value = NULL,
    choose_wave = function(intention = TRUE) if (intention) self$wave1 else self$wave2,
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
