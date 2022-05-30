#' ---
#' title: |
#'   Effect for Those Who Have Received Coupon in FY2019 (Sample Selection 2)
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

wtp %>%
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

#'
#' 2019年度にクーポン券を自動的に受け取る人へのナッジ・メッセージの効果を
#' 金銭的な価値で評価することを試みる。
#' 本論で示した方法を用いて、図\@ref(fig:demand2-vaccine)に
#' 2019年度に自動的にクーポン券を受け取る人に限定した、風しんワクチン接種の需要曲線を示した。
#' ワクチン接種が0円で供給されているとき、均衡接種割合は0.664である。
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
    title = "Estimated Monetary Value of Text-Based Nudges",
    fmt = 3, align = "lcccccc",
    output = out
  )

if (out == "kableExtra") {
  tab %>%
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
} else {
  tab %>%
    add_header_row(
      values = c("", "Monetary value (JPY)", "Monetary value (USD)"),
      colwidths = c(3, 2, 2)
    ) %>%
    add_footer_lines(values = paste(
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
    )) %>%
    fontsize(size = 9, part = "all")
}

#'
#' 抗体検査の受検確率をナッジ・メッセージの効果量として用いて、
#' 表\@ref(tab:economic-value2)にメッセージの金銭的価値を示した。
#' 第2列は図\@ref(fig:act2-coupon1-ttest)のパネルAで示した比率を示している。
#' 第3列はベースラインの均衡接種割合からメッセージの効果分だけ増やしたときの接種割合を示している。
#' 第4列はそのその接種割合と対応する自治体の追加的な補助金額であり、
#' これがメッセージの一人当たりの金銭的価値である。
#' アメリカドルに換算した価値は第6列に示した。
#' 利他強調メッセージの一人当たりの金銭的価値は約3900円（約35ドル）である。
#' 第5列はメッセージの一人当たりの金銭的価値を
#' 2019年度にクーポン券が発行されたにもかかわらず、
#' 1月時点で抗体検査のクーポン券を利用していない人口で掛けた
#' メッセージの金銭的価値の総額を示している。
#' アメリカドルに換算した価値は第7列に示している。
#' 利他強調メッセージの金銭的価値の総額はそれぞれ200億円である。
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/8-selection2_money.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */