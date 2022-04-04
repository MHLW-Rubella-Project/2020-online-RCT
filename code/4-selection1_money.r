#' ---
#' title: Monetary Value of Text-Based Nudge
#' subtitle: Preview
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' params:
#'   preview: true
#' ---
#'
#'
#+ include = FALSE, eval = params$preview
source("script/_html_header.r")
source("script/webRCT/_library.r")
web <- rct_data()
wave1 <- rct_data_wave1()
wave2 <- rct_data_wave2(1)

set_optRCTtool(
  basicmod = test_int + vaccine_int + aw1_test + aw1_testvaccine ~ nudge,
  xmod = ~ age + married + education +
    exercise_w1 + health_check + flushot +
    prob_social + handicap + severity +
    handwash + temp_check + avoid_out + avoid_crowd + wear_mask,
  data = web,
  ctrl = "厚労省"
)

#'
#' ## ナッジ・メッセージの金銭的価値 {#econvalue}
#'
#+ demandVaccine, fig.cap = "2019年度クーポン券配布対象者の風しんワクチンの需要曲線。データソース：Wave 1セレクションデータ。注）黒の三角はワクチン接種費用が無料であるときの接種割合と厚労省メッセージの抗体検査受検率を合計した割合と、それに対応するWTPを示している。", out.extra=""
act1 <- subset(wave2, coupon2019 == 1) %>%
  mean_diff_test(data = .) %>%
  filter(outcome %in% c("aw1_test", "aw1_testvaccine"))

wtp <- wave2 %>%
  dplyr::filter(coupon2019 == 1 & exp_antibody == 0) %>%
  group_by(wtp_vaccine) %>%
  summarize(N = n()) %>%
  arrange(desc(wtp_vaccine)) %>%
  mutate(cum_prop = cumsum(N) / sum(N))

cumprop0 <- unlist(wtp[wtp$wtp_vaccine == 0, "cum_prop"])
baseprop <- cumprop0 +
  subset(act1, treat == "厚労省" & outcome == "aw1_test")$mean

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

#'
#' 2019年度のクーポン券送付者におけるナッジ・メッセージの効果を金銭的な価値で評価することを試みる。
#' そのために、第1回調査のナッジ・メッセージを示す前の質問票Aで調査したワクチン接種の支払意思額を用いる。
#' ワクチンの価格は5000円と仮定して、我々は、自治体の補助金額が$s_j$のとき、ワクチン接種をするかどうかを調査した。
#' 補助金額は$s_j \in \{0, 1000, 2000, \ldots, 10000\}$とした。
#' 回答者$i$が接種すると回答した最低の補助金額を$s_i^{\text{min}}$とする。
#' 回答者$i$が接種しないと回答した最高の補助金額を$s_i^{\text{max}}$とする。
#' このとき、回答者$i$の支払意思額は
#' $[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲内で識別される[^extreme]。
#' したがって、追加の仮定を置かない限り、ワクチン接種の需要曲線はステップワイズな曲線となり、
#' メッセージの金銭的価値は範囲で得られる。
#'
#' メッセージの金銭的価値を点推定するために、
#' 我々は支払意思額が$[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲で識別されるとき、
#' 真の支払意思額はその範囲内で一様に分布することを仮定する。
#' このとき、ステップワイズなワクチン接種の需要曲線は線型補間で表される。
#' 図\@ref(fig:demandVaccine)はこの仮定のもとで、2019年度に自動的にクーポン券を受け取る人に限定した
#' 風しんワクチン接種の需要曲線である。
#' 我々はこの需要曲線を用いて、メッセージの金銭的価値を算出する。
#'
#' [^extreme]: 回答者がすべての補助金額$s_j$のときの接種しないと回答したならば、$s_i^{\text{max}} = 10000$である。しかしながら、$s_i^{\text{min}}$はデータで定義できない。そこで、$s_i^{\text{min}} = 11000$と仮定した。ただし、後に示すが、この仮定はナッジ・メッセージの金銭的価値に影響を与えない。
#'
#+ econvalue
rawvalue <- function(x) x

econval <- act1 %>%
  dplyr::filter(outcome == "aw1_test" & treat != "厚労省") %>%
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
#+ econvalue2, eval = params$preview
rawvalue <- function(x) x

baseprop2 <- cumprop0 +
  subset(act1, treat == "厚労省" & outcome == "aw1_test")$mean * 0.2
basewtp2 <- demand(baseprop2)

econval2 <- act1 %>%
  dplyr::filter(outcome == "aw1_test" & treat != "厚労省") %>%
  mutate(cumfraq = effect * 0.2 + baseprop2) %>%
  mutate_at(
    vars(cumfraq),
    list(wtp = ~ abs(demand(.)) - abs(basewtp2))
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


tab <- econval2 %>%
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
    title = "ナッジ・メッセージの金銭的価値の推定（抗体検査受検×0.2のバージョン）",
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
#' ナッジ・メッセージの金銭的な価値を次のように計算する。
#' はじめに、ベースラインの接種割合を決める。
#' 図\@ref(fig:demandVaccine)の需要曲線は無料クーポンが発行される人に限定しているので、
#' ワクチンの供給曲線はゼロで水平である。
#' このときの接種割合は約66.5%である。
#' ベースラインの接種割合はこの割合に厚労省メッセージの抗体検査受検率を足したものとする[^assumption]。
#' ベースラインの接種割合は約70%であり、対応する支払意思額は約-394円である。
#'
#' 次に、接種割合をベースラインの均衡点からナッジ・メッセージの効果分だけ増やすとき、
#' 需要曲線上で対応する支払意思額を見つける。
#' その支払意思額はナッジ・メッセージの効果量だけ増やすのに必要な自治体の追加的な補助金額であり、
#' それがナッジ・メッセージの一人当たりの金銭的価値である。
#' たとえば、ベースラインの均衡点の接種割合とナッジ・メッセージの効果の和が0.8であるとき、
#' 需要曲線上で対応する支払意思額は約-4280円である。
#' すなわち、ナッジ・メッセージの効果量分だけ接種割合を増やすために、
#' 自治体は一人当たり約3886（$=4280-394$）円の追加的な補助金を支払う必要がある。
#'
#' [^assumption]: 抗体検査の結果が陰性である人のほとんどはワクチンを接種しているので、
#' 抗体検査の受検率をワクチン接種率として用いる。
#'
#' 我々は抗体検査受検に対するナッジ・メッセージの効果を用いる。
#' 表\@ref(tab:CtabTesterCoupon1)で示したように、
#' 抗体検査の結果が陰性である人のほとんどはワクチンを接種している。
#' したがって、抗体検査受検に対するナッジ・メッセージの効果をワクチン接種に対する効果とみなせる。
#'
#' 表\@ref(tab:econvalue)はメッセージの金銭的価値の試算結果である。
#' 第2列は図\@ref(fig:BehaviorCoupon1)のパネルAで示したメッセージの効果を示している。
#' 第3列はベースラインの均衡点の接種割合からメッセージの効果量分だけ増やしたときの接種割合を示している。
#' 第4列はメッセージの一人当たりの金銭的価値である。
#' この金銭的価値をアメリカドルに換算した結果を第6列に示している。
#' 抗体検査の受検を促進した利他強調メッセージの一人当たりの金銭的価値は約2000円（約18ドル）である。
#'
#' <!--- 参考：https://www.mhlw.go.jp/content/10906000/000645181.pdf --->
#' また、メッセージ自体の金銭的価値の総額は一人当たりの金銭的価値と
#' 2019年度に発行されたクーポン券をまだ利用していない人数の積で得られる。
#' 厚生労働省より、2019年度にクーポン券が発行されたにもかかわらず、
#' 1月時点で抗体検査のクーポン券を利用していない人は約529万人である。
#' 表\@ref(tab:econvalue)の第5列はメッセージの金銭的価値の総額を示している。
#' 第7列はそれをアメリカドルに換算した結果を示している。
#' 利他強調メッセージの金銭的価値の総額は約100億円である。
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/4-selection1_money.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */