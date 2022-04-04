#' ---
#' title: Data
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

#'
#' 我々はインターネット調査会社であるマイボイスコム株式会社に委託して、合計2回のオンライン調査を実施した。
#' 補論\@ref(addtab)の図\@ref(fig:flowchart)に調査の流れを示した。
#' 第1回調査は2020年2月15日から2020年2月17日に実施した。
#' 第1回調査の対象は調査会社のモニターのうち、日本全国に居住する40歳から59歳の男性の4,200名である。
#' 第1回調査の目的はナッジ・メッセージをランダムに割り当て、
#' それが風しんの予防行動の意思にどのような影響を与えるかを検証することである。
#' 第2回調査は2020年3月17日から2020年3月25日に実施した。
#' 第2回調査は第1回調査の回答者全員を対象として、3,963名から回答を得た（脱落率=5.64%）[^attrition]。
#' 第2回調査の目的は第1回調査でランダムに割り当てたナッジ・メッセージが
#' 実際の予防行動にどのような影響を与えるかを検証することである。
#'
#+ flowchart, fig.show = if(!params$preview) "hide" else "asis", fig.width = 25, fig.height=15, out.width="150%", fig.cap = "オンライン調査の概要"
textdt <- tibble::tribble(
  ~x, ~y, ~xmin, ~xmax, ~ymin, ~ymax, ~label,
  105, 225, 75, 135, 210, 240,
  "4,200名が第1回調査に参加",
  105, 175, 75, 135, 160, 190,
  paste(
    "4,200名が質問票Aに回答",
    sep = "\n"
  ),
  15, 125, 2, 28, 110, 140,
  paste(
    "600名を", "厚労省メッセージ", "に割り当てた",
    sep = "\n"
  ),
  45, 125, 32, 58, 110, 140,
  paste(
    "600名を", "年齢表現メッセージ", "に割り当てた",
    sep = "\n"
  ),
  75, 125, 62, 88, 110, 140,
  paste(
    "600名を", "利他強調メッセージ", "に割り当てた",
    sep = "\n"
  ),
  105, 125, 92, 118, 110, 140,
  paste(
    "600名を", "利己強調メッセージ", "に割り当てた",
    sep = "\n"
  ),
  135, 125, 122, 148, 110, 140,
  paste(
    "600名を", "社会比較メッセージ", "に割り当てた",
    sep = "\n"
  ),
  165, 125, 152, 178, 110, 140,
  paste(
    "600名を", "有効期限メッセージ", "に割り当てた",
    sep = "\n"
  ),
  195, 125, 182, 208, 110, 140,
  paste(
    "600名を", "低コストメッセージ", "に割り当てた",
    sep = "\n"
  ),
  15, 25, 2, 28, 10, 40,
  paste(
    "571名が", "第2回調査に参加",
    sep = "\n"
  ),
  45, 25, 32, 58, 10, 40,
  paste(
    "578名が", "第2回調査に参加",
    sep = "\n"
  ),
  75, 25, 62, 88, 10, 40,
  paste(
    "557名が", "第2回調査に参加",
    sep = "\n"
  ),
  105, 25, 92, 118, 10, 40,
  paste(
    "563名が", "第2回調査に参加",
    sep = "\n"
  ),
  135, 25, 122, 148, 10, 40,
  paste(
    "562名が", "第2回調査に参加",
    sep = "\n"
  ),
  165, 25, 152, 178, 10, 40,
  paste(
    "566名が", "第2回調査に参加",
    sep = "\n"
  ),
  195, 25, 182, 208, 10, 40,
  paste(
    "566名が", "第2回調査に参加",
    sep = "\n"
  )
)

textdt <- textdt %>%
  bind_rows(tibble::tibble(
    x = seq(15, 195, by = 30),
    y = 75,
    xmin = seq(2, 182, by = 30),
    xmax = seq(28, 208, by = 30),
    ymin = 60, ymax = 90,
    label = paste(
      "600名が", "質問票B",
      "に回答",
      sep = "\n"
    )
  ))

arrowdt <- tibble::tribble(
  ~x, ~xend, ~y, ~yend,
  105, 105, 210, 190,
  15, 15, 150, 140,
  15, 15, 110, 90,
  15, 15, 60, 40,
  45, 45, 150, 140,
  45, 45, 110, 90,
  45, 45, 60, 40,
  75, 75, 150, 140,
  75, 75, 110, 90,
  75, 75, 60, 40,
  105, 105, 160, 140,
  105, 105, 110, 90,
  105, 105, 60, 40,
  135, 135, 150, 140,
  135, 135, 110, 90,
  135, 135, 60, 40,
  165, 165, 150, 140,
  165, 165, 110, 90,
  165, 165, 60, 40,
  195, 195, 150, 140,
  195, 195, 110, 90,
  195, 195, 60, 40
)

textdt %>%
  ggplot(aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = "black", fill = "white"
  ) +
  geom_text(aes(label = label), size = 6, family = "YuGothic") +
  geom_segment(
    data = arrowdt,
    aes(x = x, xend = xend, y = y, yend = yend),
    linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(5, "mm"), type = "closed")
  ) +
  geom_segment(
    x = 15, xend = 195, y = 150, yend = 150,
    linejoin = "mitre", lineend = "butt"
  ) +
  scale_x_continuous(breaks = seq(0, 210, by = 30), limits = c(0, 210)) +
  ylim(c(0, 250)) +
  labs(caption = paste(
    "質問票Aは日常の健康行動や風しんの知識・感染歴・ワクチン接種歴などを調査した。",
    "質問票Bは風しんの抗体検査やワクチン接種の意向や個人の社会経済属性を調査した。",
    "第2回調査は第1回調査以降の風しんの抗体検査やワクチン接種の行動を調査した。",
    sep = "\n"
  )) +
  simplegg(caption_size = 20, font_family = "YuGothic") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank()
  )

#'
#'
#+
attr_f <- anova(lm(I(1 - follow) ~ nudge, data = web))[["F value"]][1]
attr_p <- anova(lm(I(1 - follow) ~ nudge, data = web))[["Pr(>F)"]][1]
attr_label <- sprintf("F-value = %1.3f (p-value = %1.3f)", attr_f, attr_p)

#'
#' [^attrition]: 脱落率はナッジ・メッセージの群間で統計的に有意な差はない。
#' 第2回調査に参加しなかったら1を取るダミー変数を被説明変数にし、
#' 介入群ダミーを説明変数とした線形回帰分析を行った。
#' その結果、`r attr_label`となった。
#'
#' ## 第1回調査 {#wave1}
#'
#+ covlist, results = if(params$preview) "markup" else "hide"
cov <- c(
  "age", "coupon2019", "married", "education",
  "exercise_w1", "health_check", "flushot",
  "prob_social", "handicap", "severity",
  "handwash", "temp_check", "avoid_out", "avoid_crowd", "wear_mask"
)

descript <- readr::read_csv(
  "data/description/vars_descript.csv",
  locale = locale(encoding = "cp932")
) %>%
  dplyr::filter(vars %in% cov) %>%
  dplyr::select("変数の説明" = Description_ja)

attr(descript, "position") <- 2

tab <- paste(cov, collapse = "+") %>%
  paste("~ Mean + (`Std.Dev.` = SD)") %>%
  as.formula() %>%
  modelsummary::datasummary(
    data = web,
    add_columns = descript,
    align = "llcc",
    output = out,
    title = "共変量の一覧"
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(
      font_size = 9, latex_options = "hold_position"
    ) %>%
    kableExtra::column_spec(2, width = "30em")
} else {
  tab %>%
    fontsize(size = 9, part = "all") %>%
    width(2, width = 5)
}

#'
#' 第1回調査の調査は二つのパートに分けられている（便宜上、質問票Aと質問票Bとする）。
#' ナッジ・メッセージを示される前に、回答者は質問票Aの質問に回答する。
#' 質問票Aは普段の健康行動などを調査した。
#' これらの回答の一部を共変量として用いる。
#' 補論\@ref(addtab)の表\@ref(tab:covlist)に共変量の一覧を示す。
#' また、質問票Aは第1回調査時点で風しんの抗体検査やワクチン接種を受けたかどうかを調査した。
#' これらの回答はナッジ・メッセージの効果を抗体検査とワクチン接種を受けていない男性に
#' サンプルを限定して推定するときに使用する。
#'
#+ nudgelist
message_list <- readr::read_csv(
  "data/description/nudge_descript.csv",
  local = locale(encoding = "cp932")
) %>% dplyr::select("メッセージ文" = Contents_ja)

attr(message_list, "position") <- 2

tab <- web %>%
  mutate(age_group = case_when(
    age == 39 ~ "39",
    age <= 46 ~ "40-46",
    age <= 56 ~ "47-56",
    age <= 59 ~ "57-59"
  )) %>%
  rename(`ナッジ` = nudge) %>%
  modelsummary::datasummary_crosstab(
    `ナッジ` ~ age_group,
    statistic = ~ 1 + N,
    add_columns = message_list,
    align = "llcccccc",
    data = .,
    output = out,
    title = "ナッジ・メッセージの一覧"
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(font_size = 9) %>%
    kableExtra::column_spec(2, width = "20em") %>%
    kableExtra::add_header_above(
      c(" " = 3, "年齢（2019年4月時点）" = 4, " " = 1)
    )
} else {
  tab %>%
    add_header_row(
      values = c("", "年齢（2019年4月時点）", ""),
      colwidths = c(3, 4, 1)
    ) %>%
    width(2, width = 2.5) %>%
    fontsize(size = 9, part = "all")
}

#'
#' 質問票Aの回答終了後、回答者は7つのメッセージのうち一つをランダムに受け取る。
#' サンプルサイズが均等になるように、メッセージを年齢層別にランダムに割り当てた[^svyrandom]。
#' 各メッセージのサンプルサイズは600人である。
#' 表\@ref(tab:nudgelist)はメッセージの一覧とサンプルサイズを示している。
#' 表\@ref(tab:nudgelist)に示した年齢は調査によって得られた誕生年と誕生月を用いて、2019年4月時点の年齢を計算した[^svyage]。
#' 2019年4月時点で40歳から56歳の男性が厚労省の追加的対策の対象であり、
#' 40歳から46歳の男性は1年目にクーポン券を自動的に受け取る。
#'
#' [^svyrandom]: 調査会社が保有する年齢情報を用いて、40～44歳・45～49歳・50～54歳・55～59歳の層別にランダムに割り当てた。各層は1,050名で構成されていて、我々はナッジ・メッセージを均等に割り当てた（150名）。
#'
#' [^svyage]: 4月生まれの人はまだ誕生日を迎えていないことを仮定している。また、2019年4月時点での年齢であるため、調査時点で40歳の男性の一部が39歳である。
#'
#' ナッジ・メッセージは厚生労働省のホームページにあるメッセージ
#' 「昭和３７年度～昭和５３年度生まれの男性の皆様へ　
#' あなたと、これから生まれてくる世代の子どもを守るために風しんの抗体検査と予防接種を受けましょう！」
#' に基づいており、厚労省メッセージと呼ぶ。
#'
#' 各ナッジ・メッセージには、厚労省メッセージを(1)簡易な年齢表現と(2)行動経済学に基づいたメッセージ内容に変更したものを用意した。
#' 年齢表現メッセージは風しんの追加対策の正確な対象年齢に加えて、「40代・50代の男性」という平易な表現を追加した。
#' これは自分が接種対象者であるかどうかが容易に理解できるようにして、メッセージ自体の注意を引くことを目的としている。
#' 年齢表現メッセージは年齢表現を変えただけで、メッセージの内容は原文と同じであるが、
#' それ以外のナッジ・メッセージは年齢表現だけでなく、行動経済学の知見に基づいたメッセージ内容に変更した。
#'
#' 利他強調メッセージは自身が感染することで他人（特に、妊婦）にどのような損害を与えるかを具体的に記述している。
#' これは負の外部性を容易に想起させ、外部性を考慮する利他的な人の行動変容を促すことを目的としている。
#'
#' 利己強調メッセージ・社会比較メッセージは風しんの抗体を持つことの価値を高めることで行動変容を促すという目的で作成された。
#' 利己強調メッセージは自身が感染することで自分がどのような損害を受けるかを具体的に記述し、
#' 自身が感染することで生じる自身の損害を容易に想像できるようにした。
#' 社会比較メッセージは抗体保有率が低いことを明記して、自身が感染しやすいことを強調している。
#' これは風しんの感染確率を過小に見積もることを通じてワクチン接種の価値を過小に評価することを防ぐことができる。
#'
#' 有効期限メッセージと低コストメッセージは風しんのクーポン券制度に関する内容に変更した。
#' 有効期限メッセージはクーポンの有効期限を強調する内容である。
#' 2019年度に配布されるクーポン券には、2020年3月31日が有効期限であることを明記していた。
#' このメッセージは再度その内容を明記した。
#' これは現在バイアスによって抗体検査の受験やワクチン接種が妨げられていることを防ぐ目的で作成した。
#' 低コストメッセージは健康診断のついでに抗体検査を受診できることを明記して、簡単に受検できることを強調する内容である。
#' このメッセージは抗体検査の主観的なコストを抑える目的で作成した。
#'
#+ statint, eval = FALSE
c("test", "vaccine") %>%
  purrr::map(function(x) {
    web %>%
      dplyr::group_by(nudge, get(x)) %>%
      dplyr::summarize(n = n()) %>%
      dplyr::mutate(outcome = x) %>%
      dplyr::rename(val = "get(x)")
  }) %>%
  purrr::reduce(bind_rows) %>%
  mutate(outcome = factor(outcome, labels = c("Antibody Test", "Vaccine"))) %>%
  ggplot2::ggplot(aes(x = fct_rev(nudge), y = n, fill = factor(val))) +
  ggplot2::geom_bar(stat = "identity", position = "fill", color = "black") +
  ggplot2::scale_fill_brewer(
    palette = "Blues", direction = -1,
    labels = c(
      "Defenitely Yes", "Yes", "I don't know",
      "No", "Defenitely No", "Already done"
    )
  ) +
  ggplot2::facet_wrap(~outcome, ncol = 2) +
  ggplot2::labs(
    x = "Text-Based Nudge", y = "Relative Frequency",
    fill = "Intention"
  ) +
  ggplot2::coord_flip() +
  simplegg(flip = TRUE)

#'
#' ランダムに割り当てられたナッジ・メッセージを閲覧した後、回答者は質問票Bに移る。
#' 質問票Bは抗体検査の受検やワクチン接種に関する意思を調査した[^socioecon]。
#' ナッジ・メッセージの意向に対する効果を推定するとき、この回答をアウトカム変数として用いる。
#' 抗体検査受検の意向は「今、あなたは、風しんの抗体検査を受けようとどのくらい思っていますか」という質問である。
#' ワクチン接種の意向は「抗体検査を受けて、あなたに抗体がないと分かった場合、
#' あなたは、ワクチンを接種しようとどのくらい思いますか」という質問である。
#' それぞれの質問に対して、
#' 回答者は「絶対に受ける」「受ける」「どちらともいえない」「受けない」「絶対に受けない」「すでに受けた」で回答する。
#' 我々は「絶対に受ける」もしくは「受ける」と回答したら1を取るダミー変数をアウトカム変数として用いる。
#'
#' [^socioecon]: 質問票Bでは、教育年数や婚姻状態などの個人の社会経済変数についても調査している。
#' これらの変数は共変量として用いる（補論\@ref(addtab)の表 \@ref(tab:covlist)）。
#'
#' ## 第2回調査 {#wave2}
#'
#+ ctabact, eval = FALSE
tab <- web %>%
  dplyr::filter(follow == 1) %>%
  mutate(
    exp_antibody = factor(
      exp_antibody,
      levels = 0:1,
      labels = c("Test (W1): No", "Test (W1): Before W1")
    ),
    exp_vaccine = factor(
      exp_vaccine,
      levels = 0:1,
      labels = c("Vaccination (W1): No", "Vaccination (W1): Before W1")
    ),
    antibody_test = factor(
      act_test,
      levels = 1:3,
      labels = c("After W1", "Before W1", "No")
    ),
    vaccination = factor(
      act_vaccine,
      levels = 1:4,
      labels = c("After W1", "Before W1", "No", "Positive Revealed")
    )
  ) %>%
  datasummary(
    (`Test (W2)` = antibody_test) * (`Vaccination (W2)` = vaccination) ~
    N * (exp_antibody * exp_vaccine),
    align = "llcccc",
    title = "Cross Tabulation of Antibody Test and Vaccination between 第1回調査 and 2",
    data = .,
    output = out
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(font_size = 9, latex_options = "scale_down") %>%
    kableExtra::footnote(
      general_title = "",
      general = paste(
        "Note: (W1) is variables extracted from 第1回調査.",
        "(W2) denotes variables extracted from 第2回調査.",
        "Positive Reveled of Vaccination (W2) means that",
        "respondents confirm that they have positive antibody",
        "through antibody tests or past infection experience."
      ),
      threeparttable = TRUE,
      escape = FALSE
    )
} else {
  tab %>%
    add_footer_lines(values = paste(
      "Note: (W1) is variables extracted from 第1回調査.",
      "(W2) denotes variables extracted from 第2回調査.",
      "Positive Reveled of Vaccination (W2) means that",
      "respondents confirm that they have positive antibody",
      "through antibody tests or past infection experience."
    )) %>%
    fontsize(size = 9, part = "all")
}

#'
#' 第2回調査は第1回調査以降に抗体検査の受検やワクチン接種したかどうかを調査した[^covid]。
#' 抗体検査の受検行動は
#' 「前回のアンケートの回答終了時から今日までの期間に、あなたは風しんの抗体検査を受診しましたか」
#' という質問で得られる。
#' 回答者は「受診した」・「受診していない」・「前回アンケートより以前に、受診済みである」から一つ選ぶ。
#' ワクチン接種行動は
#' 「前回のアンケートの回答終了時から今日までの期間に、あなたは風しんワクチンを接種しましたか」
#' という質問で得られる。
#' 回答者は「接種した」・
#' 「すでに抗体検査で十分に抗体があることを確認した・すでに風しんに感染したので、接種する必要がなかった」・
#' 「すでに抗体検査で十分に抗体がないことを確認したが、まだ接種していない」・
#' 「抗体検査の受診もワクチンの接種もしていない」・
#' 「前回アンケートより以前に、接種済みである」
#' から一つ選ぶ。
#' <!--
#' 読者の理解を促すために、表\@ref(tab:ctabact)に第1回調査の予防行動と第2回調査の予防行動の回答のクロス集計表を示した。
#'
#' [^note]: 読者の理解を促すために選択肢のカッコ内の文章を追記しているが、回答者にカッコ内の文章を示していない。
#' -->
#'
#' これらの回答を用いて、二つのアウトカム変数を作成する。
#' 第一のアウトカム変数は回答者が「受検した」と回答すると1を取るダミー変数である。
#' <!-- 表\@ref(tab:ctabact)の"Test (W2)"列の"After W1"に当てはまるならば1を取るダミー変数である。-->
#' このアウトカム変数を用いて、我々は第1回調査以降の抗体検査の受検に対するナッジ・メッセージの効果を推定する。
#' 第二のアウトカム変数は回答者が抗体検査を「受検した」と回答し、
#' ワクチンを「接種した」と回答すると1を取るダミー変数である。
#' <!--
#' 表\@ref(tab:ctabact)の"Test (W2)"列の"After W1"に当てはまり、かつ"Vaccination (W2)"列の
#' "After W1"もしくは"Positive Revealed"に当てはまるならば1を取るダミー変数である。
#' --->
#' 厚生労働省の政策目標は抗体を持っていない人がワクチン接種を受けることで抗体保有率を引き上げることである。
#' したがって、このアウトカム変数は政策目標に直結している。
#'
#' [^covid]: 第2回調査の調査時期は新型コロナウイルスの流行が始まった時期と重なるので、
#' それが健康行動などに大きな変化を与えた可能性がある。
#' この可能性をコントロールするために、日常の詳細な健康行動についても調査した。
#' その回答は共変量として用いる（補論\@ref(addtab)の表 \@ref(tab:covlist)）。
#'
#'
# /*
#+
rmarkdown::render(
  "script/webRCT/1-summary.r",
  output_dir = "report/view/webRCT",
  knit_root_dir = here::here()
)
# */