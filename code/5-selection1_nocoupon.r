#' ---
#' title: Effect for Those Who Have Not Received Vaccination Coupon in FY2019
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
#' ## 2019年度クーポン券送付対象外の男性に限定したナッジ・メッセージの効果 {#coupon0}
#'
#' 次に、2019年度には、クーポン券の送付対象ではないが、
#' オンデマンドでクーポン券を受け取れる人に限定して、
#' ナッジ・メッセージの効果を推定する。
#'
#' <!---
#' //NOTE: 意向に対する効果（クーポンなし）
#' --->
#'
#+ IntentionCoupon0, fig.cap = "2019年度クーポン券配布対象外の男性に限定した意向に対するナッジ・メッセージの効果。データソース：Wave 1セレクションデータ。注）図中の数値は各群の比率を示し、角括弧内の数値はナッジ・メッセージの効果の規模（厚労省メッセージ群との差）を示している。効果の統計的な有意性は次の規則に従う：* p < 0.1、** p < 0.05、*** p < 0.01。", out.extra=""
int0 <- subset(wave1, coupon2019 == 0) %>%
  mean_diff_test(data = .) %>%
  filter(outcome %in% c("test_int", "vaccine_int"))

lapply(unique(int0$outcome), function(x) {
  rct_plot(
    subset(int0, outcome == x),
    label = "{{mean}} [{{effect}}{{star}}]",
    xlab = "介入群", ylab = "比率（+/- 標準誤差）",
    title = ifelse(x == "test_int", "A. 抗体検査（意向）", "B. ワクチン接種（意向）"),
    flip = TRUE, ylim = c(0, 1)
  )
}) %>% wrap_plots(ncol = 1)

#'
#' 図\@ref(fig:IntentionCoupon0)は介入群ごとの抗体検査受検とワクチン接種の意向の比率を示している。
#' 図\@ref(fig:IntentionCoupon1)と同様に、すべての介入群について、
#' 抗体検査受検の意向の比率はワクチン接種の意向の比率より低い。
#' 2019年度にクーポン券が送付されない人に対しても、
#' ワクチン接種の意向の質問文は抗体検査が陰性だったという条件付であることに注意して解釈すべきである。
#'
#' また、社会比較メッセージは抗体検査受検の意向に対して統計的に有意な効果を持っていないが、
#' ワクチン接種の意向に対して負の効果を持っている。
#' 厚労省メッセージを読んだ人の抗体検査受検とワクチン接種の意向の比率はそれぞれ約27.6%と約52.8%である。
#' それに対して、社会比較メッセージを読んだ人の抗体検査受検とワクチン接種の意向の比率はそれぞれ
#' 約23.9%と約44.6%である。
#' したがって、社会比較メッセージの抗体検査受検の意向に対する効果は約-3.7%ポイントであり、
#' これは統計的に有意な効果ではない。
#' しかしながら、社会比較メッセージのワクチン接種の意向に対する効果は約-8.2%ポイントであり、
#' これは統計的に10%水準で有意である。
#'
#' この負の効果の原因の一つとして、ワクチン接種のただ乗りが挙げられる。
#' 社会比較メッセージは「5人に1人が抗体を持っていない」ことを強調している。
#' 裏返せば、5人に4人が抗体を持っているということである。
#' このメッセージを読んだ人は、仮に風しんの抗体を保有していないとしても、
#' 全体の80%が抗体を持っているので、自身が感染する機会は少ないと考えたのかもしれない。
#' クーポン券がない場合、
#' この信念がワクチンを接種することの価値を低め、ワクチン接種の意向の比率をコントロールよりも下げた可能性がある。
#'
#' <!---
#' //NOTE: 行動に対する効果（クーポンなし）
#' --->
#'
#+ BehaviorCoupon0, fig.cap = "2019年度クーポン券配布対象外の男性に限定した行動に対するナッジ・メッセージの効果。データソース：Wave 2セレクションデータ。注）図中の数値は各群の比率を示し、角括弧内の数値はナッジ・メッセージの効果の規模（厚労省メッセージ群との差）を示している。効果の統計的な有意性は次の規則に従う：* p < 0.1、** p < 0.05、*** p < 0.01。", out.extra=""
act0 <- subset(wave2, coupon2019 == 0) %>%
  mean_diff_test(data = .) %>%
  filter(outcome %in% c("aw1_test", "aw1_testvaccine"))

lapply(unique(act0$outcome), function(x) {
  rct_plot(
    subset(act0, outcome == x),
    label = "{{mean}} [{{effect}}{{star}}]",
    text_adjust = 0.05,
    xlab = "介入群", ylab = "比率（+/- 標準誤差）",
    title = ifelse(
      x == "aw1_test",
      "A. 抗体検査の受検（第1回調査以降の行動）",
      "B. 抗体検査の受検×ワクチン接種 (第1回調査以降の行動)"
    ),
    flip = TRUE, ylim = c(0, 0.2)
  )
}) %>% wrap_plots(ncol = 1)

#'
#' 図\@ref(fig:BehaviorCoupon0)は各介入群の第1回調査以降の抗体検査受検比率と
#' 第1回調査以降のワクチン接種比率である。
#' 社会比較メッセージが厚労省メッセージよりも第1回調査以降の抗体検査の受検を促進しているが、
#' 抗体検査とワクチン接種の両方を促進していない。
#' 厚労省メッセージを読んだ人の約0.5%が第1回調査以降に抗体検査を受検したが、
#' 誰もワクチン接種をしていない。
#' また、社会比較メッセージを読んだ人の約2.8%が第1回調査以降に抗体検査を受検したが、
#' 誰もワクチン接種をしていない。
#' よって、社会比較メッセージの抗体検査受検に対する効果は約2.3%ポイントであり、
#' これは統計的に10%水準で有意である。
#' しかしながら、抗体検査とワクチン接種の両方に対する効果はゼロである。
#'
#' <!---
#' //NOTE: 回帰分析（クーポンなし）
#' --->
#'
#+ RegCoupon0, results = if(params$preview) "markup" else "hide"
intreg <- subset(wave1, coupon2019 == 0) %>%
  rct_lm(data = ., subset_outcome = 1:2, se_type = "HC0")

actreg <- subset(wave2, coupon2019 == 0) %>%
  rct_lm(data = ., subset_outcome = 3:4, se_type = "HC0")

reg <- list(
  model = list(intreg$model, actreg$model),
  res = list(intreg$res, actreg$res)
) %>% map(~ purrr::flatten(.))
class(reg) <- class(actreg)

reg %>%
  rct_table(
    title = paste(
      "2019年度クーポン券配布対象外の男性に限定した",
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
      "共変量は補論\\@ref(addtab)の表\\@ref(tab:covlist)に示した変数をすべて使用している。",
      "社会比較メッセージ群と低コストダミー群の回答者は全員ワクチン接種をしていないので、",
      "列(7)の社会比較ダミーと低コストダミーの標準誤差は計算できない。"
    ),
    not_show_x = list(
      "共変量" = getOption("RCTtool.xlist")
    ),
    output = out
  ) %>%
  {
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
#' ここまでの結果は表\@ref(tab:covlist)で示した個人の観察可能な特徴をコントロールしても変化しない。
#' 表\@ref(tab:RegCoupon0)はナッジ・メッセージの線形確率モデルの推定結果である。
#' 奇数列はナッジ・メッセージのダミー変数のみを説明変数に加えているので、
#' これらの結果は二群間のt検定の結果
#' （図\@ref(fig:IntentionCoupon0)と図\@ref(fig:BehaviorCoupon0)）に対応している。
#' 偶数列はナッジ・メッセージのダミー変数に加えて、表\@ref(tab:covlist)の変数を説明変数に加えている。
#' 列(4)は、これまでの結果に加えて、
#' 年齢表現メッセージが抗体検査の受検に負の効果を持っていることを示しており、
#' これは統計的に5%水準で有意である。
#'
#' <!--
#' //NOTE: 抗体検査受検者の動き（クーポンなし）
#' //DISCUSS: そもそもこれを入れるべきか？
#' -->
#'
#+ CtabTesterCoupon0
fisher <- c("aw1_test", "aw1_testnega", "aw1_testvaccine") %>%
  purrr::map(function(x) {
    as.formula(paste0(x, "~ nudge")) %>%
      fisher_multi_treat(
        data = if (x == "aw1_test") {
          subset(wave2, coupon2019 == 0)
        } else if (x == "aw1_testnega") {
          subset(wave2, coupon2019 == 0 & aw1_test == 1)
        } else {
          subset(wave2, coupon2019 == 0 & aw1_testnega == 1)
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
  subset(coupon2019 == 0) %>%
  datasummary(
    (`ナッジ・メッセージ` = nudge) ~ (`サンプルサイズ` = N) +
      sum * (
        (`人数` = aw1_test) +
        (`人数` = aw1_testnega) +
        (`人数` = aw1_testvaccine)
      ),
    add_columns = fisher,
    title = "2019年度クーポン券配布対象外の抗体検査受検者の動き",
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
#' 第\@ref(coupon1)節で示したように、
#' ワクチン接種比率が抗体検査の受検比率より低い原因は、
#' 抗体検査の陰性比率が低いことにある。
#' 表\@ref(tab:CtabTesterCoupon0)に抗体検査受検者の動きを示した。
#' 各群の抗体検査の陰性比率は0%から100%の範囲にある。
#' とくに、厚労省メッセージの抗体検査の陰性比率は0%（$=0/1$）である。
#' 抗体検査を促進した社会比較メッセージの陰性比率は17%（$=1/6$）である。
#' しかしながら、フィッシャーの正確検定より、
#' これらの二群の陰性比率の差は統計的に有意でない
#' （第6列より、p値は1）。
#'
#' また、第\@ref(coupon1)節と同様に、
#' 抗体検査の結果が陰性である人のほとんどがワクチンを接種していることも明らかになった。
#' 厚労省メッセージ群と低コストメッセージ群では、陰性者がいない。
#' 残った介入群のうち、社会比較メッセージ群を除くすべての群において、
#' 抗体検査の結果が陰性である人は全員ワクチンを接種していた。
#'
#' <!---
#' //NOTE: (x)全体のワクチン接種率の推定（クーポンなし）
#' //DISCUSS: そもそもこれを入れるべきか？ 一旦はずします。
#' --->
#'
#+ FullVaccineCoupon0, eval = FALSE
full <- with(
  subset(wave2, coupon2019 == 0 & aw1_testnega == 1),
  meanci_boot(aw1_testvaccine, bias.correct = FALSE, boot = 1000)
)

#'
#' <!---
#' //NOTE: (x)陰性者の抗体検査受検率（クーポンなし）
#' //DISCUSS: そもそもこれを入れるべきか？一旦外します
#' --->
#'
#+ NegativeCoupon0, eval = FALSE
res <- subset(wave2, coupon2019 == 0) %>%
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

#'
#' <!---
#' //NOTE: (x)WTPに対する効果（クーポンなし）
#' --->
#'
#+ eval = FALSE
wtp <- c("wtp_vaccine", "wtp_vaccine_wave2") %>%
  purrr::map(function(x) {
    ttest_loop(
      x,
      data = subset(wave2, coupon2019 == 0 & aw1_test == 0),
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
#+ eval = FALSE
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
      data = subset(wave2, coupon2019 == 0 & aw1_test == 0)
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
      "2019年度クーポン券配布対象外の男性に限定した",
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


# /*
#+
rmarkdown::render(
  "script/webRCT/5-selection1_nocoupon.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */