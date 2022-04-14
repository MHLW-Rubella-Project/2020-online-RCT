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
#' ## 2019年度クーポン券配布対象者に限定した分析
#'
#' 始めに、2019年度ににクーポン券が送付された40歳以上46歳以下の男性グループにおける、
#' ナッジ・メッセージの行動に対する効果を推定する。
#'
#' <!---
#' //NOTE: 行動に対する効果（クーポンあり）
#' --->
#'
#+ c1BehaviorCoupon1, fig.cap = "2019年度クーポン券配布対象者に限定した行動に対するナッジ・メッセージの効果。注）図中の数値は各群の比率を示し、角括弧内の数値はナッジ・メッセージの効果の規模（厚労省メッセージ群との差）を示している。効果の統計的な有意性は次の規則に従う：* p < 0.1、** p < 0.05、*** p < 0.01。", out.extra=""
act1 <- subset(wave2, coupon2019 == 1) %>%
  mean_diff_test(data = .)

lapply(unique(act1$outcome), function(x) {
  rct_plot(
    subset(act1, outcome == x),
    label = "{{mean}} [{{effect}}{{star}}]",
    text_adjust = 0.05,
    xlab = "介入群", ylab = "比率（+/- 標準誤差）",
    title = ifelse(
      x == "abw1_test",
      "A. 抗体検査の受検",
      "B. 抗体検査の受検×ワクチン接種"
    ),
    flip = TRUE, ylim = c(0, 0.3)
  )
}) %>% wrap_plots(ncol = 1)

#'
#' 図\@ref(fig:c1BehaviorCoupon1)は各介入群のWave 2で抗体検査を受検したと回答した比率と
#' Wave 2で抗体検査とワクチン接種を両方受けたと回答した比率を示している。
#' 結果として、利他強調メッセージは厚労省メッセージよりも抗体検査の受検比率を高めているが、
#' 抗体検査とワクチン接種の両方を促進していない。
#' 厚労省メッセージを読んだ人の約6.6%は抗体検査を受検したと回答している。
#' その一方で、利他強調メッセージを読んだ人の約14.1%は抗体検査を受検したと回答している。
#' したがって、利他強調メッセージはコントロールよりも7.5%ポイント抗体検査の受検率を引き上げており、
#' これはt検定より統計的に5%水準で有意である。
#' また、厚労省メッセージを読んだ人の約2.5%は抗体検査とワクチン接種の両方を受けたと回答している。
#' その一方で、利他強調メッセージを読んだ人の約4.9%が
#' 抗体検査とワクチン接種の両方を受けたと回答している。
#' したがって、利他強調メッセージはコントロールよりもワクチン接種率を2.4%ポイント高めているが、
#' これはt検定より統計的に有意でない。
#'
#' <!---
#' //NOTE: 回帰分析（クーポンあり）
#' --->
#'
#+ c1RegCoupon1
subset(wave2, coupon2019 == 1) %>%
  rct_lm(data = .) %>%
  rct_table(
    title = paste(
      "2019年度クーポン券配布対象者に限定した",
      "抗体検査とワクチン接種の線形確率モデルの推定結果"
    ),
    outcome_map = c(
      "abw1_test" = "抗体検査",
      "abw1_testvaccine" = "抗体検査×ワクチン接種"
    ),
    coef_map = c(
      "nudge年齢表現" = "年齢表現",
      "nudge利他強調" = "利他強調",
      "nudge利己強調" = "利己強調",
      "nudge社会比較" = "社会比較",
      "nudge有効期限" = "有効期限",
      "nudge低コスト" = "低コスト"
    ),
    footnote = paste(
      "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
      "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
    ),
    not_show_x = list(
      "共変量" = getOption("RCTtool.xlist")
    ),
    output = out
  )

#'
#' 利他強調メッセージの効果は
#' 補論\@ref(addtab)の表\@ref(tab:covlist)で示した個人の観察可能な特徴をコントロールしても変化しない。
#' 表\@ref(tab:c1RegCoupon1)はナッジ・メッセージの線形確率モデルの推定結果である。
#' 奇数列はナッジ・メッセージのダミー変数のみを説明変数に加えているので、
#' これらの結果は二群間のt検定の結果（図\@ref(fig:c1BehaviorCoupon1)）に対応している。
#' 偶数列はナッジ・メッセージのダミー変数に加えて、個人の観察可能な特徴を説明変数に加えている。
#' 共変量をコントロールすると、利他強調メッセージに加えて、
#' 利己強調メッセージが厚労省メッセージより抗体検査の受検を促進していることが明らかになった。
#' これは統計的に10%水準で有意である。
#'
#' <!--
#' //NOTE: 抗体検査受検者の動き（クーポンあり）
#' -->
#'
#+ c1CtabTesterCoupon1
fisher <- c("abw1_test", "abw1_testnega", "abw1_testvaccine") %>%
  purrr::map(function(x) {
    as.formula(paste0(x, "~ nudge")) %>%
      fisher_multi_treat(
        data = if (x == "abw1_test") {
          subset(wave2, coupon2019 == 1)
        } else if (x == "abw1_testnega") {
          subset(wave2, coupon2019 == 1 & abw1_test == 1)
        } else {
          subset(wave2, coupon2019 == 1 & abw1_testnega == 1)
        },
        base = "厚労省"
      )
  }) %>%
  reduce(bind_cols) %>%
  rbind(rep(NA_real_, 3), .) %>%
  mutate_all(list(~ if_else(!is.na(.), sprintf("%1.3f", .), "－")))

colnames(fisher) <- c("二群比較のp値\r", "二群比較のp値\r\r", "二群比較のp値\r\r\r")
attr(fisher, "position") <- c(4, 6)

wave2 %>%
  subset(coupon2019 == 1) %>%
  datasummary(
    (`ナッジ・メッセージ` = nudge) ~ (`サンプルサイズ` = N) +
      sum * (
        (`人数` = abw1_test) +
          (`人数` = abw1_testnega) +
          (`人数` = abw1_testvaccine)
      ),
    add_columns = fisher,
    title = "抗体検査受検者の動き",
    fmt = 0,
    output = out,
    data = .,
    align = "lccccccc",
  ) %>%
  {
    if (out == "kableExtra") {
      kableExtra::kable_styling(
        ., font_size = 9, latex_options = "scale_down"
      ) %>%
        kableExtra::add_header_above(
          c(
            " " = 2,
            "抗体検査の受検" = 2,
            "抗体検査の結果が陰性" = 2,
            "陰性かつワクチンを接種" = 2
          )
        ) %>%
        kableExtra::footnote(
          general_title = "",
          general = paste0(
            "注）二群比較は、厚労省メッセージ群とあるナッジ・メッセージの二群をFisherの正確検定で分析している。",
            "抗体検査の受検をアウトカムとするとき、抗体検査の受検者数に群間で差がないという帰無仮説を検定している。",
            "抗体検査の陰性者をアウトカムとするとき、抗体検査の受検者の中で陰性者の比率に群間で差がないという帰無仮説を検定している。",
            "陰性者のワクチン接種をアウトカムとするとき、陰性者の中でワクチン接種の比率に群間で差がないという帰無仮説を検定している。"
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    } else {
      add_header_row(
        .,
        values = c(
          "",
          "抗体検査の受検",
          "抗体検査の結果が陰性",
          "陰性かつワクチンを接種"
        ),
        colwidths = c(2, 2, 2, 2)
      ) %>%
      add_footer_lines(values = paste0(
        "注）二群比較は、厚労省メッセージ群とあるナッジ・メッセージの二群をFisherの正確検定で分析している。",
        "抗体検査の受検をアウトカムとするとき、抗体検査の受検者数に群間で差がないという帰無仮説を検定している。",
        "抗体検査の陰性者をアウトカムとするとき、抗体検査の受検者の中で陰性者の比率に群間で差がないという帰無仮説を検定している。",
        "陰性者のワクチン接種をアウトカムとするとき、陰性者の中でワクチン接種の比率に群間で差がないという帰無仮説を検定している。"
      )) %>%
      flextable::fontsize(size = 9, part = "all")
    }
  }

#'
#'
#' <!---
#' //NOTE: 全体のワクチン接種率の推定（クーポンあり）
#' --->
#'
#+ c1FullVaccineCoupon1
full <- with(
  subset(wave2, coupon2019 == 1 & abw1_testnega == 1),
  meanci_boot(abw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

#'
#' <!---
#' //NOTE: 陰性者の抗体検査受検率（クーポンあり）
#' --->
#'
#+ c1NegativeCoupon1
res <- subset(wave2, coupon2019 == 1) %>% {
  list(
    full = .,
    control = subset(., nudge == "厚労省"),
    age = subset(., nudge == "年齢表現"),
    altruistic = subset(., nudge == "利他強調"),
    selfish = subset(., nudge == "利己強調"),
    social = subset(., nudge == "社会比較"),
    expire = subset(., nudge == "有効期限"),
    lowcost = subset(., nudge == "低コスト")
  )} %>%
  purrr::map(~ cal_testcondnega_selection2(.))

res1 <- quantile(
  res$full$boot$testcondnega, prob = c(0.025, 0.975), na.rm = TRUE
)
res2 <- quantile(
  res$full$boot$diff, prob = c(0.025, 0.975), na.rm = TRUE
)

#'
#' 本論と同様に、
#' 介入に関わらず抗体検査の結果が陰性である人のほとんどがワクチンを接種している。
#' 全体の陰性者のワクチン接種比率は
#' `r sprintf("%1.1f%%", 100 * full$mean)`
#' である（この95%ブートストラップ信頼区間は
#' `r sprintf("[%1.1f%%, %1.1f%%]", full$ci[1] * 100, full$ci[2] * 100)`
#' ）。
#' また、ベイズ定理を用いた間接的な推定より、
#' 陰性者の抗体検査受検率は約26%で、全体の抗体検査受検率10%より高い。
#' 陰性者の抗体検査受検率と全体の抗体検査受検率の差の95%ブートストラップ信頼区間は
#' `r sprintf("[%1.3f, %1.3f]", res2[1], res2[2])`
#' である。
#' これらの結果は、抗体を保有していない人が抗体検査を受検している傾向にあることを示唆しており、
#' 本論の結果と一致する。
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/7-selection2_main.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */