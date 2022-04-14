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
#+ include = FALSE, eval = params$preview
source("script/_html_header.r")
source("script/webRCT/_library.r")
web <- rct_data()
wave2 <- rct_data_wave2(2)

set_optRCTtool(
  basicmod = abw1_test + abw1_testvaccine ~ nudge,
  data = web,
  ctrl = "厚労省",
)

#'
#+ c1demandVaccine, fig.cap = "2019年度クーポン券配布対象者の風しんワクチンの需要曲線。データソース：Wave 2データ。注）黒の三角はワクチン接種費用が無料であるときの接種割合と厚労省メッセージの抗体検査受検率を合計した割合と、それに対応するWTPを示している。", out.extra=""
act1 <- subset(wave2, coupon2019 == 1) %>%
  mean_diff_test(data = .)

wtp <- wave2 %>%
  dplyr::filter(coupon2019 == 1) %>%
  group_by(wtp_vaccine) %>%
  summarize(N = n()) %>%
  arrange(desc(wtp_vaccine)) %>%
  mutate(cum_prop = cumsum(N) / sum(N))

cumprop0 <- unlist(wtp[wtp$wtp_vaccine == 0, "cum_prop"])
baseprop <- cumprop0 +
  subset(act1, treat == "厚労省" & outcome == "abw1_test")$mean

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
  labs(y = "WTP（ワクチン接種価格＝5,000円）", x = "累積比率") +
  simplegg(font_family = "YuGothic")

#'
#' 2019年度にクーポン券を自動的に受け取る人へのナッジ・メッセージの効果を
#' 金銭的な価値で評価することを試みる。
#' 本論で示した方法を用いて、図\@ref(fig:c1demandVaccine)に
#' 2019年度に自動的にクーポン券を受け取る人に限定した、風しんワクチン接種の需要曲線を示した。
#' ワクチン接種が0円で供給されているとき、均衡接種割合は0.664である。
#'
#+ c1econvalue
rawvalue <- function(x) x

econval <- act1 %>%
  dplyr::filter(outcome == "abw1_test" & treat != "厚労省") %>%
  mutate(cumfraq = effect + baseprop) %>%
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
      treat, effect, cumfraq, wtp, totalval, wtp_dollar, totalval_dollar
  ) %>%
  mutate(treat = droplevels(treat, exclude = "厚労省"))

tab <- econval %>%
  modelsummary::datasummary(
    (`ナッジ・メッセージ` = treat) ~ rawvalue * (
      (`効果の規模` = effect) +
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

#'
#' 抗体検査の受検確率をナッジ・メッセージの効果量として用いて、
#' 表\@ref(tab:c1econvalue)にメッセージの金銭的価値を示した。
#' 第2列は図\@ref(fig:c1BehaviorCoupon1)のパネルBで示した比率を示している。
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