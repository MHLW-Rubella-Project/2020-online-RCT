#' ---
#' title: |
#'   Effect for Those Who Have Received Coupon in FY2019 (Sample Selection 2)
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
wave22 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 2)

wtp_setup <- create_RCTtoolbox(
  abw1_test ~ nudge,
  data = subset(wave22, coupon2019 == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#'
#+ demand2-vaccine, fig.cap = "Demand Curve of Rubella Vaccination among Men for whom Coupons are Automatically Distributed in FY 2019. Data source: new wave 2 selection data. Note: Black triangles indicate the sum of the percentage of vaccination when vaccination costs are free and the percentage of antibody test uptake in the MHLW message combined, and the corresponding WTP.", out.extra=""
act1 <- wtp_setup$
  ttest()$
  result

wtp <- wtp_setup$data %>%
  dplyr::filter(exp_antibody == 0) %>%
  group_by(wtp_vaccine) %>%
  summarize(N = n()) %>%
  arrange(desc(wtp_vaccine)) %>%
  mutate(cum_prop = cumsum(N) / sum(N))

cumprop0 <- unlist(wtp[wtp$wtp_vaccine == 0, "cum_prop"])
baseprop <- cumprop0 +
  subset(act1, arms == "MHLW (Control)")$mean1

demand <- with(wtp, approxfun(cum_prop, wtp_vaccine))
basewtp <- demand(baseprop)

plot_wtp <- wtp %>%
  ggplot(aes(x = cum_prop, y = wtp_vaccine)) +
  geom_point(
      aes(x = baseprop, y = basewtp),
      size = 3, shape = 17
  ) +
  geom_point(size = 3) +
  geom_line() +
  geom_segment(
      aes(x = -Inf, xend = baseprop, y = basewtp, yend = basewtp),
      linetype = 3
  ) +
  geom_segment(
      aes(x = baseprop, xend = baseprop, y = -Inf, yend = basewtp),
      linetype = 3
  ) +
  scale_y_continuous(breaks = seq(-6000, 5000, by = 1000)) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(
    y = "WTP (Price of vaccination=5,000JPY)",
    x = "Cumulative fraction"
  ) +
  simplegg(axis_text_size = 12)

ggsave(
  here("figures", "demand2-vaccine.pdf"),
  plot = plot_wtp,
  width = 10,
  height = 6
)

#'
#+ economic-value2
rawvalue <- function(x) x

econval <- act1 %>%
  dplyr::filter(arms != "MHLW (Control)") %>%
  mutate(cumfraq = diff + baseprop) %>%
  mutate_at(
    vars(cumfraq),
    list(wtp = ~ abs(demand(.)) - abs(basewtp))
  ) %>%
  mutate(totalval = (646 - 117) * wtp / 100000) %>%
  mutate(
    wtp_dollar = wtp / 110,
    totalval_dollar = (totalval / 110) * 1000
  ) %>%
  select(
    arms,
    diff,
    cumfraq,
    wtp,
    totalval,
    wtp_dollar,
    totalval_dollar
  ) %>%
  mutate(arms = droplevels(arms, exclude = "MHLW (Control)"))

out.file <- file(here("tables", "economic-value2.tex"), open = "w")

tab <- econval %>%
  modelsummary::datasummary(
    (`Text-based nudge` = arms) ~ rawvalue * (
      (`Size of effect` = diff) +
      (`Baseline + size of effect` = cumfraq) +
      (`pp` = wtp) +
      (`total` = totalval) +
      (`pp` = wtp_dollar) +
      (`total` = totalval_dollar)
    ),
    data = .,
    title = paste(
      "Estimated Monetary Value of Text-Based Nudges",
      "\\label{tab:economic-value2}"
    ),
    fmt = 3, align = "lcccccc",
    output = "latex"
  ) %>%
  kableExtra::kable_styling(font_size = 9, latex_options = "scale_down") %>%
  kableExtra::add_header_above(
    c(" " = 3, "Monetary value (JPY)" = 2, "Monetary value (USD)" = 2)
  ) %>%
  kableExtra::footnote(
    general_title = "",
    general = paste(
      "Note:",
      "Effect is the size of effect of each text-based nudge",
      "on antibody test.",
      "Baseline is the sum of the rate of antibody test in the control",
      "and the vaccination rate when the vaccine is free",
      "The monetary value is the amount per person (pp) and the total amount",
      "(total) multiplied by the number of people who received the coupon",
      "in 2019 but did not use it until January, 2020.",
      "We valued the monetary value in Japanese Yen (JPY)",
      "and US Dollars (USD) (1USD = 110JPY).",
      "The unit of monetary value per person is 1 JPY and 1 USD,",
      "respectively.",
      "The unit of total monetary value is 1 billion JPY and 1 million USD,",
      "respectively."
    ),
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(tab, out.file)
close(out.file)
