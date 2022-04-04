#' ---
#' title: Sample Selection and Balance Test
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
#' ## サンプルセレクションの定義 {#selection}
#'
#' 我々の関心はナッジ・メッセージが抗体検査やワクチン接種を受けていない男性の行動を促進できるかどうかである。
#' そのために、第1回調査時点で抗体検査とワクチン接種を受けていない男性にサンプルを限定して、
#' ナッジ・メッセージの効果を推定する。
#' 分析に用いるサンプルの基準は2つある。
#' 第一の基準は第1回調査で過去に抗体検査を受検したもしくは過去にワクチン接種をしていないと回答したかどうかである。
#' 抗体検査の受検やワクチン接種の意向に対する効果を推定するとき、
#' この基準を満たした男性にサンプルを限定する（以降、Wave 1セレクションデータと呼ぶ）。
#' <!--
#' すなわち、補論Aの表\@ref(tab:ctabact)の"Test (W1): No"かつ"Vaccination (W1): No"列に当てはまる人
#' を分析で使用する。
#' -->
#' 第二の基準は第2回調査で第1回調査以前に抗体検査を受検したもしくはワクチンを接種したと回答したかどうかである[^reason]。
#' 第1回調査以降の抗体検査の受検やワクチン接種に対する効果を推定するとき、
#' 第一の基準と第二の基準を満たした男性にサンプルを限定する（以降、Wave 2セレクションデータと呼ぶ）。
#' <!---
#' すなわち、補論Aの表\@ref(tab:ctabact)の"Test (W1): No"かつ"Vaccination (W1): No"列に当てはまる人
#' のうち、"Test (W2)"列の"Before W1"もしくは"Vaccination (W2)"列の"Before W1"に当てはまる人を除き、
#' 残ったサンプルを分析に使用する。
#' --->
#'
#' [^reason]: 第1回調査以降に自身の接種歴を調べ直すなどによって、第1回調査と第2回調査の回答に違いが生じる可能性がある。
#' そのため、どちらかの調査で第1回調査以前に抗体検査を受検したもしくはワクチンを接種したと回答した人を除いた。
#'
#' 我々は上記の基準で構築したサブサンプルを2019年度にクーポン券の送付対象年齢か否かで分割して、
#' 各グループにおけるナッジ・メッセージの効果を推定する。
#' 2019年度にクーポン券の送付対象年齢か否かは2019年4月時点の年齢で識別した。
#' 各市区町村は2019年度に40歳以上46歳以下の男性のクーポン券を送付し、
#' 2020年度以降に47歳以上56歳以下の男性のクーポン券を送付する。
#' ただし、市区町村の判断や本人の希望に応じて、47歳以上56歳以下の男性もクーポン券を受け取ることはできる。
#'
#' <!---
#' //NOTE: バランステスト
#' --->
#'
#+ BalanceWave1Coupon1, results = if(params$preview) "markup" else "hide"
subset(wave1, coupon2019 == 1) %>%
  balance_test(data = .) %>%
  rct_table(
    title = "Wave 1セレクションデータの共変量のバランステスト（2019年度クーポン券配布対象）",
    output = out
  )

#'
#+ BalanceWave2Coupon1, results = if(params$preview) "markup" else "hide"
subset(wave2, coupon2019 == 1) %>%
  balance_test(data = .) %>%
  rct_table(
    title = "Wave 2セレクションデータの共変量のバランステスト（2019年度クーポン券配布対象）",
    output = out
  )

#'
#+ BalanceWave1Coupon0, results = if(params$preview) "markup" else "hide"
subset(wave1, coupon2019 == 0) %>%
  balance_test(data = .) %>%
  rct_table(
    title = "Wave 1セレクションデータの共変量のバランステスト（2019年度クーポン券配布対象外）",
    output = out
  )

#'
#+ BalanceWave2Coupon0, results = if(params$preview) "markup" else "hide"
subset(wave2, coupon2019 == 0) %>%
  balance_test(data = .) %>%
  rct_table(
    title = "Wave 1セレクションデータの共変量のバランステスト（2019年度クーポン券配布対象外）",
    output = out
  )

#'
#' 回答者の観察可能な特徴の観点から、ナッジ・メッセージのランダム割り当ては成功している。
#' 2019年度クーポン券配布対象に限定したバランステストの結果を
#' 補論\@ref(addtab)の
#' 表\@ref(tab:BalanceWave1Coupon1)（Wave 1セレクションデータ）と
#' 表\@ref(tab:BalanceWave2Coupon1)（Wave 2セレクションデータ）に
#' 示した。
#' また、2019年クーポン券配布対象外に限定したバランステストの結果を
#' 補論\@ref(addtab)の
#' 表\@ref(tab:BalanceWave1Coupon0)（Wave 1セレクションデータ）と
#' 表\@ref(tab:BalanceWave2Coupon0)（Wave 2セレクションデータ）に
#' 示した。
#' ナッジ・メッセージは個人の観察可能な特徴に対してランダムなので、
#' 共変量をコントロールしたかどうかに関わらず、メッセージの効果は大きく変化しないはずである。
#' 事実、線形確率モデルの推定において、介入効果の規模は共変量を説明変数に加えるか否かで大きく変化しない。
#' したがって、本節では、厚労省メッセージと各ナッジ・メッセージ間の平均値の差の検定（t検定）の結果のみを示し、
#' 回帰分析の結果は補論\@ref(addtab)の表\@ref(tab:RegCoupon1)（2019年度クーポン券配布対象）
#' と表\@ref(tab:RegCoupon0)（2019年度クーポン券配布対象外）に示す。
#'
#' <!---
#' //NOTE: 検定力分析
#' --->
#'
#+ AprioriPowerAnalysis, eval = params$preview
datalist <- list(
  subset(wave1, coupon2019 == 1),
  subset(wave1, coupon2019 == 0),
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
    (`介入群` = treat) ~ rawvalue * (
      (`N` = N.x) * Format(digits = 0) +
      (`二群の差` = diff.x) * Format(digits = 3) +
      (`N` = N.y) * Format(digits = 0) +
      (`二群の差` = diff.y) * Format(digits = 3) +
      (`N` = N.x.x) * Format(digits = 0) +
      (`二群の差` = diff.x.x) * Format(digits = 3) +
      (`N` = N.y.y) * Format(digits = 0) +
      (`二群の差` = diff.y.y) * Format(digits = 3)
    ),
    title = "検出力80\\%・有意水準5\\%を保つために必要な二群の平均の差",
    data = .,
    align = "lcccccccc",
    output = out
  ) %>% {
    if (out == "kableExtra") {
      kableExtra::kable_styling(., font_size = 9) %>%
        kableExtra::add_header_above(
          c(
            " ",
            "クーポン配布対象" = 2, "クーポン配布対象外" = 2,
            "クーポン配布対象" = 2, "クーポン配布対象外" = 2
          )
        ) %>%
        kableExtra::add_header_above(
          c(" ", "Wave 1セレクションデータ" = 4, "Wave 2セレクションデータ" = 4)
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
        values = c(" ",
          "クーポン配布対象", "クーポン配布対象外",
          "クーポン配布対象", "クーポン配布対象外"
        ),
        colwidths = c(1, 2, 2, 2, 2)
      ) %>%
      flextable::add_header_row(
        values = c("", "Wave 1セレクションデータ", "Wave 2セレクションデータ"),
        colwidths = c(1, 4, 4)
      ) %>%
      flextable::add_footer_lines(., values = paste0(
        "注）効果量から二群の平均の差の絶対値を計算するときに、",
        "標準偏差が0.2であることを仮定している。"
      )) %>%
      flextable::fontsize(size = 9, part = "all")
    }
  }

#'
#' また、補論\@ref(addtab)の表\@ref(tab:AprioriPowerAnalysis)に検出力分析の結果を示した。
#' この表は検出力が80%で有意水準が5%となるために必要最低限な二群の平均値の差の絶対値を示している。
#' 抗体検査の受検行動やワクチン接種の受検行動をアウトカムとして、
#' 2019年度クーポン券配布対象のサンプルに限定したWave 2セレクションデータを用いるとき、
#' 検出力80%・有意水準5%を保つために必要な効果量は少なくとも7%ポイントの差がないとならない。
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/2-selection1_balance.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */