#' ---
#' title: Sample Selection 2 and Balance Test
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
#+ c1balancetestw2coupon1
subset(wave2, coupon2019 == 1) %>%
  balance_test(data = .) %>%
  rct_table(
    title = paste0(
      "新しいサンプルセレクション定義に従ったWave 2データのバランステストの結果",
      "（2019年度クーポン券配布対象）"
    ),
    output = out
  )

#'
#+ c1balancetestw2coupon0
subset(wave2, coupon2019 == 1) %>%
  balance_test(data = .) %>%
  rct_table(
    title = paste0(
      "新しいサンプルセレクション定義に従ったWave 2データのバランステストの結果",
      "（2019年度クーポン券配布対象外）"
    ),
    output = out
  )

#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ c1AprioriPowerAnalysis, eval = params$preview
datalist <- list(
  subset(wave2, coupon2019 == 1),
  subset(wave2, coupon2019 == 0)
)

rawvalue <- function(x) x

datalist %>%
  purrr::map(~ power_analysis(
    data = ., std_dev = 0.2, alpha = 0.05, power = 0.8
  )) %>%
  purrr::map(function(x) {
    tab <- rct_table(x, output = "data.frame")
    colnames(tab) <- c("treat", "N", "diff")
    tab
  }) %>%
  reduce(left_join, by = "treat") %>%
  mutate(treat = factor(treat, levels = levels(web$nudge))) %>%
  mutate_at(vars(-treat), list(~ as.numeric(.))) %>%
  datasummary(
    (`介入群` <- treat) ~ rawvalue * (
      (`N` <- N.x) * Format(digits = 0) +
        (`二群の差` <- diff.x) * Format(digits = 3) +
        (`N` <- N.y) * Format(digits = 0) +
        (`二群の差` <- diff.y) * Format(digits = 3)
    ),
    title = "検出力80%・有意水準5%を保つために必要な二群の平均の差",
    data = .,
    align = "lcccc",
    output = out
  ) %>%
  {
    if (out == "kableExtra") {
      kableExtra::kable_styling(.) %>%
        kableExtra::add_header_above(
          c(
            " ",
            "クーポン配布対象" = 2, "クーポン配布対象外" = 2
          )
        ) %>%
        kableExtra::footnote(
          general_title = "",
          general = paste0(
            "注）効果量から二群の平均の差の絶対値を計算するときに、",
            "標準偏差が0.2であることを仮定している。"
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    } else {
      flextable::add_header_row(
        .,
        values = c(
          " ",
          "クーポン配布対象", "クーポン配布対象外"
        ),
        colwidths = c(1, 2, 2)
      ) %>%
      flextable::add_footer_lines(., values = paste0(
        "注）効果量から二群の平均の差の絶対値を計算するときに、",
        "標準偏差が0.2であることを仮定している。"
      )) %>%
      flextable::fontsize(size = 9, part = "all")
    }
  }

#'
#' ここでは、第2回調査の抗体検査の受検行動やワクチン接種行動の回答に
#' 想起バイアスが伴うことを考慮した分析を行う。
#' 第2回調査はそれぞれの行動を第1回調査以前に行ったかどうかを調査している。
#' この時期の回答に想起バイアスが伴うならば、
#' 本論の分析のように第2回調査で第1回調査以前に行動したと回答した人を除くべきではない。
#' そこで、我々は第2回調査の行動の回答に想起バイアスが伴うことを仮定して、
#' 第1回調査の調査ですでに抗体検査もしくはワクチン接種を受けた男性だけを除いて、
#' ナッジ・メッセージの行動に対する効果を推定する。
#' なお、第2回調査で第1回調査以前に抗体検査もしくはワクチン接種を受けたと回答した人はサンプルに含まれている。
#' <!---
#' 表\@ref(tab:ctabact)の"Test (W1): No"かつ"Vaccination (W1): No"列に当てはまる人を分析で使用する。
#' -->
#'
#' 本論と同様に、
#' 我々は2019年度にクーポン券を自動的に受け取っているかどうかでサンプルを分割して、
#' サブサンプルを用いてナッジ・メッセージの効果を推定する。
#' 表\@ref(tab:c1balancetestw2coupon1)と表\@ref(tab:c1balancetestw2coupon0)は共変量のバランステストの結果であり、
#' 回答者の観察可能な特徴は群間でシステマティックに異ならないことを示している。
#'
#' また、アウトカム変数の定義も本論のものから変更する。
#' 本論では、第1回調査以降に抗体検査を受検したら1を取るアウトカム変数と
#' 第1回調査以降に抗体検査を受検し、ワクチンによって抗体を新たに獲得したら1を取るダミー変数でを用いた。
#' 対して、この補論では、
#' 第2回調査で時期に関わらず抗体検査を受検したと回答したら1を取るダミー変数と
#' 第2回調査で時期に関わらず抗体検査を受検し、
#' 時期に関わらずワクチンによって抗体を獲得したら1を取るダミー変数である。
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/6-selection2_balance.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */