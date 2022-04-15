#' ---
#' title: Monetary Value of Text-Based Nudges on Behavior
#' subtitle: Preview
#' output:
#'   bookdown::html_document2:
#'     toc: yes
#'     toc_float: yes
#'     number_sections: false
#' params:
#'   preview: yes
#'   appendix: yes
#' ---
#'
#+ include = FALSE
library(here)
source(here("code/_common.r"), encoding = "utf8")

#+ include = FALSE
wave1 <- rct_data_wave1(here(rct_path, "shape_survey.csv"))
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

#+ include = FALSE
covmod <- ~ age + married + education +
  exercise_w1 + health_check + flushot +
  prob_social + handicap + severity +
  handwash + temp_check + avoid_out + avoid_crowd + wear_mask

act_coupon1 <- create_RCTtoolbox(
  aw1_test + aw1_testvaccine ~ nudge,
  covmod,
  data = subset(wave2, coupon2019 == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#+ demand-vaccine, eval = FALSE
act1 <- act_coupon1$ttest()$result

wtp <- wave2 %>%
  dplyr::filter(coupon2019 == 1 & exp_antibody == 0) %>%
  group_by(wtp_vaccine) %>%
  summarize(N = n()) %>%
  arrange(desc(wtp_vaccine)) %>%
  mutate(cum_prop = cumsum(N) / sum(N))

cumprop0 <- unlist(wtp[wtp$wtp_vaccine == 0, "cum_prop"])
baseprop <- cumprop0 +
  subset(act1, arms == "MHLW" & outcome == "aw1_test")$mean1

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
  labs(y = "WTP (ワクチン接種価格＝5,000円)", x = "累積比率") +
  simplegg(font_family = "YuGothic", axis_text_size = 12)

#+ economic-value, eval = FALSE
rawvalue <- function(x) x

econval <- act1 %>%
  dplyr::filter(outcome == "aw1_test" & arms != "厚労省") %>%
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
    arms, diff, cumfraq, wtp, totalval, wtp_dollar, totalval_dollar
  ) %>%
  mutate(arms = droplevels(arms, exclude = "MHLW"))


tab <- econval %>%
  modelsummary::datasummary(
    (`ナッジ・メッセージ` = arms) ~ rawvalue * (
      (`効果の規模` = diff) +
      (`ベースライン＋効果の規模` = cumfraq) +
      (`一人当たり` = wtp) +
      (`総額` = totalval) +
      (`一人当たり` = wtp_dollar) +
      (`総額` = totalval_dollar)
    ),
    data = .,
    title = "ナッジ・メッセージの金銭的価値の推定",
    fmt = 3, align = "lcccccc",
    output = out
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(font_size = 9, latex_options = "scale_down") %>%
    kableExtra::add_header_above(
      c(" " = 3, "金銭的価値（日本円）" = 2, "金銭的価値（米ドル）" = 2)
    ) %>%
    kableExtra::footnote(
      general_title = "",
      general = paste(
        "注）",
        "抗体検査の受検に対するナッジ・メッセージの効果を効果の規模として用いた。",
        "ベースラインはワクチン接種費用が無料であるときの接種割合と",
        "厚労省メッセージの抗体検査受検率を合計した割合である。",
        "金銭的価値は一人当たりの価値と",
        "それに2020年1月時点でワクチンクーポン券を利用していない人数（529万人）をかけた",
        "総額を示している。",
        "また、金銭的価値は日本円と米ドルで示した（1ドル＝110円）。",
        "一人当たりの金銭的価値の単位はそれぞれ1円と1ドルである。",
        "総額で示した金銭的価値の単位はそれぞれ10億円と100万ドルである。"
        # "Note:",
        # "Effect is the size of effect of each text-based nudge",
        # "on antibody test.",
        # "Baseline is the sum of the rate of antibody test in the control",
        # "and the vaccination rate when the vaccine is free",
        # "The monetary value is the amount per person (pp) and the total amount",
        # "(total) multiplied by the number of people who received the coupon",
        # "in 2019 but did not use it until January, 2020.",
        # "We valued the monetary value in Japanese Yen (JPY)",
        # "and US Dollars (USD) (1USD = 110JPY).",
        # "The unit of monetary value per person is 1 JPY and 1 USD,",
        # "respectively.",
        # "The unit of total monetary value is 1 billion JPY and 1 million USD,",
        # "respectively."
      ),
      threeparttable = TRUE,
      escape = FALSE
    )
} else {
  tab %>%
    add_header_row(
      values = c("", "金銭的価値（日本円）", "金銭的価値（米ドル）"),
      colwidths = c(3, 2, 2)
    ) %>%
    add_footer_lines(values = paste(
      "注）",
      "抗体検査の受検に対するナッジ・メッセージの効果を効果の規模として用いた。",
      "ベースラインはワクチン接種費用が無料であるときの接種割合と",
      "厚労省メッセージの抗体検査受検率を合計した割合である。",
      "金銭的価値は一人当たりの価値と",
      "それに2020年1月時点でワクチンクーポン券を利用していない人数（529万人）をかけた",
      "総額を示している。",
      "また、金銭的価値は日本円と米ドルで示した（1ドル＝110円）。",
      "一人当たりの金銭的価値の単位はそれぞれ1円と1ドルである。",
      "総額で示した金銭的価値の単位はそれぞれ10億円と100万ドルである。"
      # "Note:",
      # "Effect is the size of effect of each text-based nudge",
      # "on antibody test.",
      # "Baseline is the sum of the rate of antibody test in the control",
      # "and the vaccination rate when the vaccine is free",
      # "The monetary value is the amount per person (pp) and the total amount",
      # "(total) multiplied by the number of people who received the coupon",
      # "in 2019 but did not use it until January, 2020.",
      # "We valued the monetary value in Japanese Yen (JPY)",
      # "and US Dollars (USD) (1USD = 110JPY).",
      # "The unit of monetary value per person is 1 JPY and 1 USD,",
      # "respectively.",
      # "The unit of total monetary value is 1 billion JPY and 1 million USD,",
      # "respectively."
    )) %>%
    fontsize(size = 9, part = "all")
}
