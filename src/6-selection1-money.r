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
source(here("src/_common.r"), encoding = "utf8")

#+ include = FALSE, eval = params$preview
source(here("src/_html_header.r"), encoding = "utf8")

#+ include = FALSE
wave2 <- rct_data_wave2(here(rct_path, "shape_survey.csv"), 1)

wtp_setup <- create_RCTtoolbox(
  aw1_test ~ nudge,
  data = subset(wave2, coupon2019 == 1),
  treat_levels = LETTERS[1:7],
  treat_labels = treat_labels
)

#+ wtp-question, eval = params$preview | params$appendix, fig.cap = "Elicitaiton of Willingess-to-Pay for Rubella Vaccination.", out.width = "100%", out.extra = ""
knitr::include_graphics(here("assets", "wtp-question.png"))

#+ demand-vaccine, eval = params$preview | params$appendix, fig.cap = "Demand Curve of Rubella Vaccination among Men for whom Coupons are Automatically Distributed in FY 2019. Data source: wave 2 selection data. Note: Black triangles indicate the sum of the percentage of vaccination when vaccination costs are free and the percentage of antibody test uptake in the MHLW message combined, and the corresponding WTP.", out.extra = ""
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
  subset(act1, arms == "MHLW")$mean1

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

#' ```{asis, echo = params$preview | !params$appendix}
#' 厚生労働省の追加施策は風しんワクチンの価格をゼロにする金銭的インセンティブとみなせる。
#' しかしながら、クーポン券を利用した抗体検査の受検率は18%である。
#' 抗体検査の結果が陰性であれば、ワクチン接種を受ける必要がないので、
#' クーポン券を利用したワクチン接種率は18%より低くなる。
#' では、仮に、厚生労働省がナッジ・メッセージを用いずに金銭的インセンティブだけを用いて、
#' ワクチン接種率を高めようとしているならば、
#' 厚生労働省はあといくらの追加的な金銭的インセンティブを個人に与えればよいのだろうか。
#'
#' そこで、我々はオンライン調査で得られた風しんワクチン接種の支払意思額を用いて、
#' ナッジ・メッセージの効果を金銭的な価値を評価する。
#' ナッジ・メッセージの効果の金銭的な価値は
#' 推定されたメッセージの効果と同等となる追加的な金銭的補助である。
#' 似たようなアプローチでナッジ・メッセージの金銭的な価値を推定した研究に
#' @Bursztyn2019 や @Moriwaki2020 がある。
#'
#' 我々は、第1回調査のナッジ・メッセージを示す前に、ワクチン接種の支払意思額を調査した。
#' ワクチンの価格は5000円と仮定して、
#' 我々は、自治体の補助金額が$s_j$のとき、ワクチン接種をするかどうかを調査した。
#' 補助金額は$s_j \in \{0, 1000, 2000, \ldots, 10000\}$とした。
#' 回答者$i$が接種すると回答した最低の補助金額を$s_i^{\text{min}}$とする。
#' 回答者$i$が接種しないと回答した最高の補助金額を$s_i^{\text{max}}$とする。
#' このとき、回答者$i$の支払意思額は
#' $[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲内で識別される[^extreme]。
#' したがって、
#' 追加の仮定を置かない限り、ワクチン接種の需要曲線はステップワイズな曲線となり、
#' メッセージの金銭的価値は範囲で得られる。
#'
#' メッセージの金銭的価値を点推定するために、
#' 我々は支払意思額が
#' $[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲で
#' 識別されるとき、
#' 真の支払意思額はその範囲内で一様に分布することを仮定する。
#' このとき、ステップワイズなワクチン接種の需要曲線は線型補間で表される。
#' 我々は補論に支払意思額に関する調査と需要曲線を示した。
#'
#' [^extreme]: 回答者がすべての補助金額$s_j$のときの接種しないと回答したならば、$s_i^{\text{max}} = 10000$である。
#' しかしながら、$s_i^{\text{min}}$はデータで定義できない。そこで、$s_i^{\text{min}} = 11000$と仮定した。
#' ただし、後に示すが、この仮定はナッジ・メッセージの金銭的価値に影響を与えない。
#' ```
#'
#' ```{asis, echo = params$appendix}
#' ナッジ・メッセージの効果を金銭的に評価するために、
#' 我々は風しんワクチンの支払意思額を用いた。
#' 我々は、第1回調査のナッジ・メッセージを示す前に、ワクチン接種の支払意思額を調査した。
#' 支払意思額はMultiple price list法に基づくものである
#' （調査画面を表\@ref(fig:wtp-question)に示した）。
#'
#' ワクチンの価格は5000円と仮定して、
#' 我々は、自治体の補助金額が$s_j$のとき、ワクチン接種をするかどうかを調査した。
#' 補助金額は$s_j \in \{0, 1000, 2000, \ldots, 10000\}$とした。
#' 回答者$i$が接種すると回答した最低の補助金額を$s_i^{\text{min}}$とする。
#' 回答者$i$が接種しないと回答した最高の補助金額を$s_i^{\text{max}}$とする。
#' 回答者がすべての補助金額$s_j$のときの接種しないと回答したならば、$s_i^{\text{max}} = 10000$である。
#' しかしながら、$s_i^{\text{min}}$はデータで定義できない。
#' そこで、$s_i^{\text{min}} = 11000$と仮定した。
#'
#' このとき、回答者$i$の支払意思額は
#' $[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲内で識別される。
#' したがって、
#' 追加の仮定を置かない限り、ワクチン接種の需要曲線はステップワイズな曲線となり、
#' メッセージの金銭的価値は範囲で得られる。
#'
#' メッセージの金銭的価値を点推定するために、
#' 我々は支払意思額が
#' $[5000 - s_i^{\text{min}}, 5000 - s_i^{\text{max}})$の範囲で
#' 識別されるとき、
#' 真の支払意思額はその範囲内で一様に分布することを仮定する。
#' このとき、ステップワイズなワクチン接種の需要曲線は線型補間で表される。
#' 2019年度にクーポン券が自動的に送付され、
#' 第1回調査時点で抗体検査もしくはワクチン接種をしていない男性に限定して、
#' 線型補間された需要曲線を図\@ref(fig:demand-vaccine)に示した。
#' ```
#'
#+ economic-value, eval = params$preview | !params$appendix
rawvalue <- function(x) x

econval <- act1 %>%
  dplyr::filter(arms != "MHLW") %>%
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
  mutate(arms = droplevels(arms, exclude = "MHLW"))


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

#' ```{asis, echo = params$preview | !params$appendix}
#' ナッジ・メッセージの金銭的な価値を次のように計算する。
#' はじめに、ベースラインの接種割合を決める。
#' クーポン券が自動的に配布され、抗体検査やワクチン接種を受検していない人に限定して、
#' 需要曲線を推定した。
#' したがって、ワクチンの供給曲線はゼロで水平である。
#' このときの接種割合は約66.5%である。
#' ベースラインの接種割合はこの割合に
#' 厚労省メッセージの抗体検査受検率を足したものである[^assumption]。
#' ベースラインの接種割合は約70%であり、対応する支払意思額は約-394円である。
#'
#' 次に、
#' 接種割合をベースラインの均衡点からナッジ・メッセージの効果分だけ増やすとき、
#' 需要曲線上で対応する支払意思額を見つける。
#' その支払意思額は
#' ナッジ・メッセージの効果だけ増やすのに必要な自治体の追加的な補助金額であり、
#' それがナッジ・メッセージの一人当たりの金銭的価値である。
#' たとえば、
#' ベースラインの均衡点の接種割合とナッジ・メッセージの効果の和が0.8であるとき、
#' 需要曲線上で対応する支払意思額は約-4280円である。
#' すなわち、ナッジ・メッセージの効果量分だけ接種割合を増やすために、
#' 自治体は一人当たり約3886（$=4280-394$）円の追加的な補助金を支払う必要がある。
#'
#' 我々は抗体検査の受検率に対するナッジ・メッセージの効果を用いる。
#' 表\@ref(tab:tester-move)で示したように、
#' 抗体検査の結果が陰性である人のほとんどはワクチンを接種している。
#' この事実は、抗体検査を受検した人は同時にワクチンを接種したいと考えている
#' ことを示唆している。
#' したがって、抗体検査受検に対するナッジ・メッセージの効果を
#' 行動から推測されるワクチン接種の（真の）意向に対する効果とみなせる。
#'
#' [^assumption]: ベースラインの接種割合を求めるときに、
#' 我々は厚労省メッセージの抗体検査受検率を足した。
#' これは調査によってメッセージを受け取ることの自体の影響を考慮したためである。
#'
#' 表\@ref(tab:economic-value)はメッセージの金銭的価値の試算結果である。
#' 第2列は図\@ref(fig:act-coupon1-ttest)の
#' パネルAで示したメッセージの効果を示している。
#' 第3列はベースラインの均衡点の接種割合から
#' メッセージの効果だけ増やしたときの接種割合を示している。
#' 第4列はメッセージの一人当たりの金銭的価値である。
#' この金銭的価値をアメリカドルに換算した結果を第6列に示している。
#' 抗体検査の受検を促進した利他強調メッセージの一人当たりの金銭的価値は約2000円（約18ドル）である。
#'
#' <!-- 参考：https://www.mhlw.go.jp/content/10906000/000645181.pdf -->
#' また、メッセージ自体の金銭的価値の総額は一人当たりの金銭的価値と
#' 2019年度に発行されたクーポン券をまだ利用していない人数の積で得られる。
#' 厚生労働省より、2019年度にクーポン券が発行されたにもかかわらず、
#' 1月時点で抗体検査のクーポン券を利用していない人は約529万人である。
#' 表\@ref(tab:economic-value)の第5列はメッセージの金銭的価値の総額を示している。
#' 第7列はそれをアメリカドルに換算した結果を示している。
#' 利他強調メッセージの金銭的価値の総額は約100億円である。
#' ```
# /*
#+
rmarkdown::render(
  here("src/6-selection1-money.r"),
  output_dir = here("report/view")
)
# */