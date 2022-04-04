#' ---
#' title: Effect for Those Who Have Received Vaccination Coupon in FY2019
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
wave1 <- rct_data_wave1()
wave2 <- rct_data_wave2(1)
set_optRCTtool(
  basicmod = test_int + vaccine_int + aw1_test + aw1_testvaccine ~ nudge,
  xmod = ~ age + married + education +
    exercise_w1 + health_check + flushot +
    prob_social + handicap + severity +
    handwash + temp_check + avoid_out + avoid_crowd + wear_mask,
  data = wave1,
  ctrl = "厚労省"
)

#'
#' ## 2019年度クーポン券配布対象者に限定したナッジ・メッセージの効果 {#coupon1}
#'
#' 始めに、2019年度にクーポン券が送付された40歳以上46歳以下の男性グループにおける、
#' ナッジ・メッセージの意向と行動に対する効果を推定する。
#'
#' <!---
#' //NOTE: 意向に対する効果のt検定（クーポンあり）
#' --->
#'
#+ IntentionCoupon1, fig.cap = "2019年度クーポン券配布対象者に限定した意向に対するナッジ・メッセージの効果。データソース：Wave 1セレクションデータ。注）図中の数値は各群の比率を示し、角括弧内の数値はナッジ・メッセージの効果の規模（厚労省メッセージ群との差）を示している。効果の統計的な有意性は次の規則に従う：* p < 0.1、** p < 0.05、*** p < 0.01。", out.extra=""
int1 <- subset(wave1, coupon2019 == 1) %>%
  mean_diff_test(data = .) %>%  
  filter(outcome %in% c("test_int", "vaccine_int"))

lapply(unique(int1$outcome), function(x) {
  rct_plot(
    subset(int1, outcome == x),
    label = "{{mean}} [{{effect}}{{star}}]",
    xlab = "介入群", ylab = "比率（+/- 標準誤差）",
    title = ifelse(x == "test_int", "A. 抗体検査（意向）", "B. ワクチン接種（意向）"),
    flip = TRUE, ylim = c(0, 1)
  )
}) %>% wrap_plots(ncol = 1)

#'
#' 図\@ref(fig:IntentionCoupon1)は各介入群の抗体検査受検とワクチン接種の意向を示している。
#' 結果として、利他強調メッセージは厚労省メッセージよりも抗体検査受検の意向を高めている。
#' 厚労省メッセージを読んだ人の約20.8%が抗体検査を受けたいと回答している一方で、
#' 利他強調メッセージを読んだ人の約35.1%が抗体検査を受けたいと回答している。
#' したがって、
#' 利他強調メッセージはコントロールよりも約14.3%ポイント抗体検査の受検意向を引き上げており、
#' これはt検定より統計的に1%水準で有意である。
#' その他のナッジ・メッセージについては、抗体検査を受けたいと答えた人の割合は30%を下回っていて、
#' その比率が厚労省メッセージと変わらないという帰無仮説を棄却できない。
#'
#' 図\@ref(fig:IntentionCoupon1)のパネルBはワクチン接種の意向を示している。
#' その結果、
#' すべての介入群のワクチン接種の意向の比率は40%から45%の範囲にあり、
#' その比率は介入群間で統計的に有意な差とならなかった。
#' 考えられる可能性の一つはワクチン接種の意向の質問文による刺激である。
#' 我々は抗体を持っていないという条件のもとで接種したいかどうかを質問しているので、
#' 質問文がワクチン接種の必要性を強く刺激した可能性がある。
#'
#' <!---
#' //NOTE: 行動に対する効果のt検定（クーポンあり）
#' --->
#'
#+ BehaviorCoupon1, fig.cap = "2019年度クーポン券配布対象者に限定した行動に対するナッジ・メッセージの効果。データソース：Wave 2セレクションデータ。注）図中の数値は各群の比率を示し、角括弧内の数値はナッジ・メッセージの効果の規模（厚労省メッセージ群との差）を示している。効果の統計的な有意性は次の規則に従う：* p < 0.1、** p < 0.05、*** p < 0.01。", out.extra=""
act1 <- subset(wave2, coupon2019 == 1) %>%
  mean_diff_test(data = .) %>%  
  filter(outcome %in% c("aw1_test", "aw1_testvaccine"))

lapply(unique(act1$outcome), function(x) {
  rct_plot(
    subset(act1, outcome == x),
    label = "{{mean}} [{{effect}}{{star}}]",
    text_adjust = 0.05,
    xlab = "介入群", ylab = "比率（+/- 標準誤差）",
    title = ifelse(
      x == "aw1_test",
      "A. 抗体検査の受検（第1回調査以降の行動）",
      "B. 抗体検査の受検×ワクチン接種 (第1回調査以降の行動)"
    ),
    flip = TRUE, ylim = c(0, 0.3)
  )
}) %>% wrap_plots(ncol = 1)

#'
#' 図\@ref(fig:BehaviorCoupon1)のパネルAは各介入群の第1回調査以降の抗体検査受検比率を示している。
#' 結果として、利他強調メッセージと利己強調メッセージでは、厚労省メッセージよりも
#' 第1回調査以降の抗体検査の受検比率が高い。
#' 厚労省メッセージを読んだ人の約3.5%は第1回調査以降に抗体検査を受検している。
#' その一方で、利他強調メッセージを読んだ人の約10.9%と
#' 利己強調メッセージを読んだ人の約9%は第1回調査以降に抗体検査を受検している。
#' したがって、利他強調メッセージはコントロールに比べて7.4%ポイント抗体検査の受検率を引き上げており、
#' これはt検定より統計的に5%水準で有意である。
#' また、利己強調メッセージはコントロールよりも約5.5%ポイント高く、これはt検定より統計的に10%水準で有意である。
#'
#' 図\@ref(fig:BehaviorCoupon1)のパネルBは
#' 各介入群の第1回調査以降の抗体検査とワクチン接種を両方受けた人の比率（以降、ワクチン接種率と呼ぶ）を示している。
#' この比率は今回の厚生労働省の政策によって新たに抗体を獲得した人の比率を示すので、
#' 政策効果のアウトカム指標となる。
#' その結果、利他強調メッセージと社会比較メッセージがワクチン接種を促進していることがわかる。
#' 厚労省メッセージを読んだ人の約0.9%が抗体検査を受検し、ワクチンを接種している。
#' その一方で、利他強調メッセージを読んだ人の約4.7%と
#' 社会比較メッセージを読んだ人の約4.9%が第1回調査以降に抗体検査を受検し、ワクチンを接種している。
#' よって、利他強調メッセージは3.8%ポイントコントロールよりも高く、これはt検定より統計的に10%水準で有意である。
#' 社会比較メッセージは4%ポイントコントロールよりも高く、これはt検定より統計的に10%水準で有意である。
#'
#' <!---
#' [^policy]: この効果は今回の厚生労働省の政策によって新たに抗体を獲得した人の比率を示すので、政策効果である。
#' 利他強調メッセージと社会比較メッセージは厚生労働省の追加政策の対象男性の抗体保有率を約4%ポイント高めていて、
#' これは厚生労働省の目標である10%ポイント増加の約40%に相当する。
#' --->
#'
#' <!---
#' //NOTE: 回帰分析（クーポンあり）
#' --->
#'
#+ RegCoupon1, results = if(params$preview) "markup" else "hide"
intreg <- subset(wave1, coupon2019 == 1) %>%
  rct_lm(data = ., subset_outcome = 1:2, se_type = "HC0")

actreg <- subset(wave2, coupon2019 == 1) %>%
  rct_lm(data = ., subset_outcome = 3:4, se_type = "HC0")

reg <- list(
  model = list(intreg$model, actreg$model),
  res = list(intreg$res, actreg$res)
) %>% map(~ purrr::flatten(.))
class(reg) <- class(actreg)

reg %>%
  rct_table(
    title = paste(
      "2019年度クーポン券配布対象者に限定した",
      "抗体検査とワクチン接種の線形確率モデルの推定結果"
    ),
    outcome_map = c(
      "test_int" = "抗体検査",
      "vaccine_int" = "ワクチン接種",
      "aw1_test" = "抗体検査",
      "aw1_testvaccine" = "抗体検査×ワクチン接種"
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
  ) %>% {
    if (out == "kableExtra") {
      kableExtra::add_header_above(., c(
        " " = 1,
        "意向" = 4,
        "第1回調査以降の行動" = 4
      ))
    } else {
      add_header_row(
        .,
        values = c("", "意向", "第1回調査以降の行動"),
        colwidths = c(1, 4, 4)
      )
    }
  }

#'
#' 補論\@ref(addtab)の表\@ref(tab:RegCoupon1)に厚労省メッセージ群を比較対象とした
#' ナッジ・メッセージの線形確率モデルの推定結果を示した。
#' ここまでの結果は個人の観察可能な特徴をコントロールしても変化しない。
#' それに加えて、共変量を制御したモデルを推定すると、
#' 利己強調メッセージは厚労省メッセージよりも抗体検査受検の意向を約9%ポイント強めていて、
#' これは統計的に10%水準で有意である。
#' さらに、利己強調メッセージと社会比較メッセージは厚労省メッセージと比較して抗体検査の受検行動に
#' 統計的に5%水準で正の影響を与えている。
#' 効果の規模はそれぞれ6.7%ポイントと6.5%ポイントである。
#'
#' <!--
#' //NOTE: 利他強調をベースとした効果検証
#' -->
#'
#+ altbase-reg-coupon1, eval = FALSE
intreg_alt <- subset(wave1, coupon2019 == 1) %>%
  rct_lm(data = ., subset_outcome = 1:2, ctrl = "利他強調", se_type = "HC0")

actreg_alt <- subset(wave2, coupon2019 == 1) %>%
  rct_lm(data = ., subset_outcome = 3:4, ctrl = "利他強調", se_type = "HC0")

reg_alt <- list(
  model = list(intreg_alt$model, actreg_alt$model),
  res = list(intreg_alt$res, actreg_alt$res)
) %>% map(~ purrr::flatten(.))
class(reg_alt) <- class(actreg_alt)

reg_alt %>%
  rct_table(
    title = paste(
      "利他強調メッセージと比較した介入群の効果",
      "(2019年度クーポン券配布対象者)"
    ),
    outcome_map = c(
      "test_int" = "抗体検査",
      "vaccine_int" = "ワクチン接種",
      "aw1_test" = "抗体検査",
      "aw1_testvaccine" = "抗体検査×ワクチン接種"
    ),
    coef_map = c(
      "nudge厚労省" = "厚労省",
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
  ) %>% {
    if (out == "kableExtra") {
      kableExtra::add_header_above(., c(
        " " = 1,
        "意向" = 4,
        "第1回調査以降の行動" = 4
      ))
    } else {
      add_header_row(
        .,
        values = c("", "意向", "第1回調査以降の行動"),
        colwidths = c(1, 4, 4)
      )
    }
  }

#'
#' また、効果の規模が最も大きい利他強調メッセージ群を比較対象とした
#' 線形確率モデルの推定結果を補論\@ref(addtab)の表\@ref(tab:altbase-reg-coupon1)に示した。
#' その結果、
#' 利己強調メッセージ群の抗体検査受検の意向は利他強調メッセージのそれと統計的に有意な差とならなかった。
#' さらに、利己強調メッセージと社会比較メッセージの抗体検査の受検比率は
#' 利他強調メッセージのそれと統計的に有意に異ならならなかった。
#' したがって、効果があった利他強調メッセージとの有意差がないという意味で、
#' 利己強調メッセージは抗体検査受検の意向と行動を促進した可能性があり、
#' 社会比較メッセージは抗体検査の受検を促進した可能性がある。
#' しかしながら、抗体検査の受検比率の差は検出力を十分に保つほどの大きさではないので、
#' サンプルサイズを十分に大きくして検証する必要がある。
#'
#' <!--
#' //NOTE: 抗体検査受検者の動き（クーポンあり）
#' -->
#'
#+ CtabTesterCoupon1
fisher <- c("aw1_test", "aw1_testnega", "aw1_testvaccine") %>%
  purrr::map(function(x) {
    as.formula(paste0(x, "~ nudge")) %>%
      fisher_multi_treat(
        data = if (x == "aw1_test") {
          subset(wave2, coupon2019 == 1)
        } else if (x == "aw1_testnega") {
          subset(wave2, coupon2019 == 1 & aw1_test == 1)
        } else {
          subset(wave2, coupon2019 == 1 & aw1_testnega == 1)
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
        (`人数` = aw1_test) +
        (`人数` = aw1_testnega) +
        (`人数` = aw1_testvaccine)
      ),
    add_columns = fisher,
    title = "2019年度クーポン券配布対象の抗体検査受検者の動き",
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
#' 利他強調メッセージと社会比較メッセージがワクチン接種を促進した理由は、
#' 厚労省メッセージ群よりも抗体検査の陰性比率が高いことにある。
#' この点を明らかにするために、
#' 表\@ref(tab:CtabTesterCoupon1)に各群の抗体検査受検者の動きを示した。
#' この表から二つの発見がある。
#' 第一に、利他強調メッセージと低コストメッセージを除くすべての群で、
#' 抗体検査の結果が陰性である人が全員ワクチンを接種している。
#' たとえば、厚労省メッセージ群では、1人の陰性者がワクチン接種をしているので、
#' 陰性者のワクチン接種比率は100%である。
#' ワクチン接種を促進した利他強調メッセージと社会比較メッセージの
#' 陰性者のワクチン接種比率はそれぞれ
#' 86%（$=6/7$）と100%（$=5/5$）である。
#'
#' 第二に、各群の抗体検査の陰性比率は20%から62.5%の範囲にある。
#' たとえば、厚労省メッセージ群では、4人の抗体検査受検者のうち、
#' 1人が陰性であったので、陰性比率は約25%である。
#' ワクチン接種を促進した利他強調メッセージと社会比較メッセージの陰性比率はそれぞれ
#' 50%（$=7/14$）と56%（$=5/9$）である。
#' したがって、厚労省メッセージと比較して、
#' 利他強調メッセージと社会比較メッセージはワクチンを接種するべき人が多くいたので、
#' これらのメッセージがワクチン接種に対して正の効果があった。
#' また、利他強調メッセージ群の陰性者のワクチン接種率が100%を下回っているので、
#' ワクチン接種に対する社会比較メッセージの効果の規模（4%ポイント）が
#' 利他強調メッセージ（3.8%ポイント）より若干大きくなった。
#'
#' ただし、利他強調メッセージと社会比較メッセージの抗体検査の陰性比率が
#' 厚労省メッセージより高いという発見は偶然である可能性が高い。
#' 我々はあるナッジ・メッセージ群と厚労省メッセージ群の抗体検査受検者に限定して、
#' 陰性者の数が群間で異ならないという帰無仮説を
#' フィッシャーの正確検定で検証し、
#' そのp値を表\@ref(tab:CtabTesterCoupon1)の第6列に示した。
#' すべてのナッジ・メッセージ群において、
#' 陰性者の数が厚労省のそれと異ならないという帰無仮説を棄却できなかった。
#' よって、我々のデータでは陰性比率が各群で異なっているが、
#' 母集団では陰性比率は群間で差がない[^FisherVaccine]。
#' 
#' [^FisherVaccine]: 陰性者のワクチン接種比率についても同様の結果が得られた。
#' すなわち、利他強調メッセージと厚労省メッセージで、
#' 陰性者のワクチン接種の数に差がない
#' （表\@ref(tab:CtabTesterCoupon1)の第8列より、p値は1）。
#'
#' <!---
#' //NOTE: 全体のワクチン接種率の推定（クーポンあり）
#' --->
#'
#+ FullVaccineCoupon1
full <- with(
  subset(wave2, coupon2019 == 1 & aw1_testnega == 1),
  meanci_boot(aw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

#'
#' また、介入に関わらず抗体検査の結果が陰性である人のほとんどがワクチンを接種しているという事実は、
#' 抗体検査の受検率を高めることが政策的に重要であることを示唆している。
#' 抗体保有率を高めるための政策介入は二種類が考えられる。
#' 第一に、単純に抗体検査の受検者を増やす政策である。
#' 第二に、抗体検査の結果が陰性である人がワクチンを接種することを促進する政策である。
#' 今回のランダム化比較試験は後者の介入をしていないにも関わらず、
#' ほとんどの陰性者がワクチンを接種している[^fullsample]。
#' これは陰性者のワクチン接種を促進する政策よりも抗体検査の受検を促進する政策の方が
#' 効率的に抗体保有率を高められることを示唆している。
#' このとき、抗体を保有していない人に抗体検査を受検させるような政策を用いることで、
#' より政策目標を達成できる[^Bayesian]。
#'
#' [^fullsample]: 介入群ごとにデータを分割しない場合の陰性者のワクチン接種比率は
#' `r sprintf("%1.1f%%", 100 * full$mean)`
#' であった。また、1000個のブートストラップ標本で構築した95%信頼区間は100%を含んでいる
#' （95%信頼区間は、
#' `r sprintf("[%1.1f%%, %1.1f%%]", full$ci[1] * 100, full$ci[2] * 100)`
#' ）。
#'
#'  <!---
#' //NOTE: 陰性者の抗体検査受検率（クーポンあり）
#' --->
#'
#+ NegativeCoupon1
res <- subset(wave2, coupon2019 == 1) %>%
  {
    list(
      full = .,
      control = subset(., nudge == "厚労省"),
      age = subset(., nudge == "年齢表現"),
      altruistic = subset(., nudge == "利他強調"),
      selfish = subset(., nudge == "利己強調"),
      social = subset(., nudge == "社会比較"),
      expire = subset(., nudge == "有効期限"),
      lowcost = subset(., nudge == "低コスト")
    )
  } %>%
  purrr::map(~ cal_testcondnega_selection1(.))

res1 <- quantile(
  res$full$boot$testcondnega,
  prob = c(0.025, 0.975), na.rm = TRUE
)
res2 <- quantile(
  res$full$boot$diff,
  prob = c(0.025, 0.975), na.rm = TRUE
)

res_ctrl <- res$control$obs$testcondnega - res$control$obs$test
res_ctrl_boot <- quantile(
  res$control$boot$diff, prob = c(.025, .975), na.rm = TRUE
)

res_alt <- res$altruistic$obs$testcondnega - res$altruistic$obs$test
res_alt_boot <- quantile(
  res$altruistic$boot$diff, prob = c(.025, .975), na.rm = TRUE
)

#'
#' [^Bayesian]: 陰性であるにも関わらず抗体検査を受検していない人がいるはずなので、
#' 陰性者の抗体検査受検率をデータから直接復元することはできない。
#' しかしながら、ベイズ定理を用いて、間接的に推定することができる。
#' それを示すために、陰性という事象$A$と抗体検査の受検という事象$B$の二つの事象を考える。
#' このとき、抗体検査の受検比率は$P(B)$、抗体検査受検者の陰性比率は$P(A|B)$で表すことができ、
#' これらの値はデータから直接推定できる。
#' ベイズの定理より、抗体検査受検者の陰性比率は
#' $$ P(A|B) = \frac{P(B|A) \cdot P(A)}{P(B)} $$
#' と定義できる。
#' ここで、$P(A)$は陰性比率であり、
#' これは第\@ref(background)節で示したNIIDのデータより0.2となる。
#' 確率$P(B|A)$は陰性者で条件づけた抗体検査の受検比率であり、我々の関心のあるパラメータである。
#' よって、陰性者の抗体検査の受検比率は
#' $$ \hat{P}(B|A) = \frac{\hat{P}(A|B) \cdot \hat{P}(B)}{0.2} $$
#' で計算できる。
#' 介入群でサンプルを分割しなかった場合、
#' $\hat{P}(A|B) = 0.413$と$\hat{P}(B) = 0.072$なので、
#' $\hat{P}(B|A) = 0.149$となる（1000個のブートストラップ標本で構築した95%信頼区間は
#' `r sprintf("[%1.3f, %1.3f]", res1[1], res1[2])` ）。
#' さらに、陰性であるかどうかによって抗体検査の受検にセレクションが生じているかどうかを
#' 検証するために、陰性という事象と抗体検査の受検という事象が独立であるという帰無仮説を検定した。
#' $\hat{P}(B|A) - \hat{P}(B)$の95%信頼区間にゼロが含まれていないとき、
#' 我々は帰無仮説を5%有意水準で棄却できる。
#' $\hat{P}(B|A) - \hat{P}(B)$の95%信頼区間は
#' `r sprintf("[%1.3f, %1.3f]", res2[1], res2[2])`
#' なので、我々は帰無仮説を棄却できる。
#' 言い換えれば、抗体を保有していない人が抗体検査を受検している傾向にある。
#'
#' <!---
#' //NOTE: (x)WTPに対する効果（クーポンあり）
#' --->
#'
#+ WtpCoupon1, fig.cap = "2019年度クーポン券配布対象者に限定したワクチン接種のWTPに対するナッジ・メッセージの効果", out.extra="", eval = FALSE
wtp <- c("wtp_vaccine", "wtp_vaccine_wave2") %>%
  purrr::map(function(x) {
    ttest_loop(
      x,
      data = subset(wave2, coupon2019 == 1 & aw1_test == 0),
      base = "厚労省"
    )
  }) %>%
  purrr::reduce(bind_rows)

wtp %>%
  mutate(
    outcome = factor(
      outcome,
      levels = c("wtp_vaccine", "wtp_vaccine_wave2"),
      labels = c(
        "パネルA：ワクチン接種のWTP（Wave 1）",
        "パネルB：ワクチン接種のWTP（Wave 2）"
      )
    )
  ) %>%
  ggplot(aes(x = fct_rev(nudge), y = mean)) +
  geom_bar(stat = "identity", color = "black", fill = "lightblue") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.5) +
  geom_text(aes(label = label, y = -1500), size = 5) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "ナッジ・メッセージ", y = "平均（+/- 標準誤差）") +
  ylim(c(-2000, 500)) +
  facet_wrap(~outcome, nrow = 2) +
  coord_flip() +
  ggtemp(flip = TRUE, family = "YuGothic", size = list(text = 12)) +
  theme(strip.text = element_text(size = 15))

#'
#+ RegWtpCoupon1, results = if(params$preview) "markup" else "hide", eval = FALSE
wtpmod <- list(
  "(1)" = list(model = wtp_vaccine ~ nudge),
  "(2)" = list(model = update(wtp_vaccine ~ nudge, xeq)),
  "(3)" = list(model = wtp_vaccine_wave2 ~ nudge),
  "(4)" = list(model = update(wtp_vaccine_wave2 ~ nudge, xeq)),
  "(5)" = list(model = I(wtp_vaccine_wave2 - wtp_vaccine) ~ nudge),
  "(6)" = list(model = update(I(wtp_vaccine_wave2 - wtp_vaccine) ~ nudge, xeq))
)

tab <- wtpmod %>%
  purrr::map(
    ~ lm(
      .$model,
      data = subset(wave2, coupon2019 == 1 & aw1_test == 0)
    )
  ) %>%
  modelsummary(
    vcov = function(x) sandwich::vcovHC(x, type = "HC0"),
    coef_omit = "^(?!nudge)",
    coef_rename = c(
      "nudge年齢表現" = "年齢表現",
      "nudge利他強調" = "利他強調",
      "nudge利己強調" = "利己強調",
      "nudge社会比較" = "社会比較",
      "nudge有効期限" = "有効期限",
      "nudge低コスト" = "低コスト"
    ),
    gof_omit = "AIC|BIC|Log|F|R2 Adj.",
    stars = c("*" = .1, "**" = .05, "***" = .01),
    add_rows = tibble::tribble(
      ~term, ~"(1)", ~"(2)", ~"(3)", ~"(4)", ~"(5)", ~"(6)",
      "Covariate", "", "X", "", "X", "", "X"
    ),
    output = out,
    title = paste(
      "2019年度クーポン券配布対象者に限定した",
      "WTPの線形確率モデルの推定結果"
    )
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(font_size = 9, latex_options = "scale_down") %>%
    kableExtra::add_header_above(c(
      " " = 1,
      "Wave 1" = 2,
      "Wave 2" = 2,
      "Wave 2 - Wave 1" = 2
  )) %>%
    kableExtra::footnote(
      general_title = "",
      general = paste(
        "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
        "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
      ),
      threeparttable = TRUE,
      escape = FALSE
  )
} else {
  tab %>%
    add_header_row(
      values = c("", "Wave 1", "Wave 2", "Wave 2 - Wave 1"),
      colwidths = c(1, 2, 2, 2)
    ) %>%
    add_footer_lines(values = paste(
      "注）* p < 0.1、** p < 0.05、*** p < 0.01。頑健標準誤差を使用している。",
      "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。"
    )) %>%
    fontsize(size = 9, part = "all")
}

#'
# /*
#+
rmarkdown::render(
  "script/webRCT/3-selection1_main.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */