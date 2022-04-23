#'---
#' title: Online Survey Experiment
#' subtitle: Preview
#' author: Hiroki Kato
#' bibliography: ../MHLW Rubella Nudge.bib
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
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
web <- read_csv(here(rct_path, "shape_survey.csv"))

#+ flowchart, eval = params$preview | params$appendix, fig.width = 25, fig.height = 15, out.width = "150%", fig.cap = "Overview of Online Survey Experiment", out.extra = "angle=90"
textdt <- tibble::tribble(
  ~x, ~y, ~xmin, ~xmax, ~ymin, ~ymax, ~label,
  105, 225, 75, 135, 210, 240,
  paste(nrow(web), "males participated in Wave 1"),
  105, 175, 75, 135, 160, 190,
  paste(nrow(web), "males answered Questionnaire A"),
  15, 125, 2, 28, 110, 140,
  paste("600 males read", "MHLW message", sep = "\n"),
  45, 125, 32, 58, 110, 140,
  paste("600 males read", "Age expression message", sep = "\n"),
  75, 125, 62, 88, 110, 140,
  paste("600 males read", "Altruistic message", sep = "\n"),
  105, 125, 92, 118, 110, 140,
  paste("600 males read", "Selfish message", sep = "\n"),
  135, 125, 122, 148, 110, 140,
  paste("600 males read", "Social comparison message", sep = "\n"),
  165, 125, 152, 178, 110, 140,
  paste("600 males read", "Valid date message", sep = "\n"),
  195, 125, 182, 208, 110, 140,
  paste("600 males read", "Low-cost message", sep = "\n"),
  105, 75, 75, 135, 60, 90,
  paste(nrow(web), "males answered Questionnaire B"),
  105, 25, 75, 135, 10, 40,
  paste(sum(web$follow), "males participated in Wave 2"),
)

arrowdt <- tibble::tribble(
  ~x, ~xend, ~y, ~yend,
  105, 105, 210, 190,
  15, 15, 150, 140,
  45, 45, 150, 140,
  75, 75, 150, 140,
  105, 105, 160, 140,
  135, 135, 150, 140,
  165, 165, 150, 140,
  195, 195, 150, 140,
  105, 105, 100, 90,
  105, 105, 60, 40
)

segmentdt <- tibble::tribble(
  ~x, ~xend, ~y, ~yend,
  15, 195, 150, 150,
  15, 195, 100, 100,
  15, 15, 110, 100,
  45, 45, 110, 100,
  75, 75, 110, 100,
  105, 105, 110, 100,
  135, 135, 110, 100,
  165, 165, 110, 100,
  195, 195, 110, 100
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
    data = segmentdt,
    aes(x = x, xend = xend, y = y, yend = yend),
    linejoin = "mitre", lineend = "butt"
  ) +
  scale_x_continuous(breaks = seq(0, 210, by = 30), limits = c(0, 210)) +
  ylim(c(0, 250)) +
  labs(caption = paste(
    "Questionnaire A investigated daily health behaviors,",
    "knowledge of rubella, infection history, and vaccination history.",
    "Questionnaire B investigated the intention to be tested for antibody to",
    "rubella and to be vaccinated, as well as socioeconomic attributes.",
    "Wave 2 surveyed the behavior of antibody testing",
    "and vaccination against rubella since Wave 1.",
    sep = "\n"
  )) +
  simplegg(caption_size = 20) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank()
  )

#' ```{asis, echo = params$preview | !params$appendix}
#' 本研究は、厚生労働省とコラボレーションをして、2回のオンライン調査を実施した[^IRB]。
#' 我々はオンライン調査の実施を
#' インターネット調査会社であるマイボイスコム株式会社（MyVociceCome Co. Ltd.）に委託した。
#' 補論Bに調査の流れを示した。
#' 第1回調査は2020年2月15日から2020年2月17日に実施した。
#' 第1回調査の対象は調査会社のモニターのうち、日本全国に居住する40歳から59歳の男性の4,200名である。
#' 第1回調査の目的はナッジ・メッセージをランダムに割り当て、
#' それが風しんの予防行動の意思にどのような影響を与えるかを検証することである。
#' 第2回調査は2020年3月17日から2020年3月25日に実施した。
#' 第2回調査は第1回調査の回答者全員を対象として、3,963名から回答を得た（脱落率=5.64%）[^attrition]。
#' 第2回調査の目的は第1回調査でランダムに割り当てたナッジ・メッセージが
#' 実際の予防行動にどのような影響を与えるかを検証することである。
#'
#' [^IRB]: オンライン調査上のランダム化比較試験を実施する際、
#' 我々は大阪大学経済学研究科の倫理審査委員会の認可を受けている（承認番号：R020114）。
#'
#' ```
#'
#+ reg-attrition, eval = params$preview | !params$appendix, results = "asis"
attr_f <- anova(lm(I(1 - follow) ~ nudge, data = web))[["F value"]][1]
attr_p <- anova(lm(I(1 - follow) ~ nudge, data = web))[["Pr(>F)"]][1]
attr_label <- sprintf("F-value = %1.3f (p-value = %1.3f)", attr_f, attr_p)

cat(c(
  "[^attrition]: 脱落率はナッジ・メッセージの群間で統計的に有意な差はない。",
  "第2回調査に参加しなかったら1を取るダミー変数を被説明変数にし、",
  "介入群ダミーを説明変数とした線形回帰分析を行った。",
  "その結果、",
  attr_label,
  "となった。"
), sep = "\n")

#+ covlist, eval = params$preview | params$appendix
cov <- c(
  "age", "coupon2019", "married", "education",
  "exercise_w1", "health_check", "flushot",
  "prob_social", "handicap", "severity",
  "handwash", "temp_check", "avoid_out", "avoid_crowd", "wear_mask"
)

descript <- readr::read_csv(
  here("assets/vars_descript.csv"),
  locale = locale(encoding = "cp932")
) %>%
  dplyr::filter(vars %in% cov) %>%
  dplyr::select(Description)

attr(descript, "position") <- 2

tab <- paste(cov, collapse = "+") %>%
  paste("~ Mean + (`Std.Dev.` = SD)") %>%
  as.formula() %>%
  modelsummary::datasummary(
    data = web,
    add_columns = descript,
    align = "llcc",
    output = out,
    title = "List of Covariates"
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

#' ```{asis, echo = params$preview | !params$appendix}
#' ## Wave 1: Interventions and Outcome Variables on Intention {#wave1}
#' ```
#'
#' ```{asis, echo = FALSE}
#' 第1回調査の調査は二つのパートに分けられている（便宜上、質問票Aと質問票Bとする）。
#' ナッジ・メッセージを示される前に、回答者は質問票Aの質問に回答する。
#' 質問票Aは普段の健康行動などを調査した。
#' これらの回答の一部を共変量として用いる。
#' 補論Aに共変量の一覧を示す。
#' また、質問票Aは第1回調査時点で風しんの抗体検査やワクチン接種を受けたかどうかを調査した。
#' ナッジ・メッセージの効果を抗体検査とワクチン接種を受けていない男性に限定して推定するときにこの回答を使用する。
#' ```
#'
#+ nudgelist, eval = params$preview | !params$appendix
message_list <- readr::read_csv(
  here("assets/nudge_descript.csv"),
  local = locale(encoding = "cp932")
) %>% dplyr::select(Contents)

attr(message_list, "position") <- 2

tab <- web %>%
  mutate(age_group = case_when(
    age == 39 ~ "39",
    age <= 46 ~ "40-46",
    age <= 56 ~ "47-56",
    age <= 59 ~ "57-59"
  )) %>%
  mutate(nudge = factor(nudge, labels = treat_labels)) %>%
  rename(Message = nudge) %>%
  modelsummary::datasummary_crosstab(
    Message ~ age_group,
    statistic = ~ 1 + N,
    add_columns = message_list,
    align = "llcccccc",
    data = .,
    output = out,
    title = "List of Text-Based Nudges"
  )

if (out == "kableExtra") {
  tab %>%
    kableExtra::kable_styling(font_size = 9) %>%
    kableExtra::column_spec(2, width = "20em") %>%
    kableExtra::add_header_above(
      c(" " = 3, "Age (as of Apr 2019)" = 4, " " = 1)
    )
} else {
  tab %>%
    add_header_row(
      values = c("", "Age (as of Apr 2019)", ""),
      colwidths = c(3, 4, 1)
    ) %>%
    width(2, width = 2.5) %>%
    fontsize(size = 9, part = "all")
}

#' ```{asis, echo = params$preview | !params$appendix}
#' 表\@ref(tab:nudgelist)はコントロールメッセージとナッジ・メッセージをまとめている[^svyage]。
#' 厚労省メッセージ（*MHLW* message）は
#' 厚生労働省が風しんの抗体検査とワクチン接種を勧奨するホームページ上で用いているものであり、
#' 本研究のコントロールメッセージ（*business-as-usual control*）である。
#' 我々はどのような要素を強調したナッジ・メッセージが有効であるかを探るために、
#' 厚労省メッセージに基づいた6つの異なるナッジ・メッセージを作成した。
#' 本研究のように、ナッジ・メッセージの探索的研究は、
#' 候補となるメッセージを複数用意し、
#' 一回の実験でその有効性を検証しているものが多い[e.g. @Dai2021; @Milkman2021]。
#'
#' [^svyage]: 表\@ref(tab:nudgelist)に示した年齢は調査によって得られた誕生年と誕生月を用いて、
#' 2019年4月時点の年齢を計算した。
#' 2019年4月時点で40歳から56歳の男性が厚労省の追加的対策の対象であり、
#' 40歳から46歳の男性は1年目にクーポン券を自動的に受け取る。
#' 4月生まれの人はまだ誕生日を迎えていないことを仮定している。
#' また、2019年4月時点での年齢であるため、調査時点で40歳の男性の一部が39歳である。
#'
#' ナッジ・メッセージは、厚労省メッセージを(1)簡易な年齢表現と(2)行動経済学に基づいたメッセージ内容に変更している。
#' 年齢表現メッセージ（*Age expression* message）は風しんの追加対策の正確な対象年齢に加えて、
#' 「40代・50代の男性」という平易な表現を追加した。
#' これは自分が接種対象者であるかどうかが容易に理解できるようにして、メッセージ自体の注意を引くことを目的としている。
#' 年齢表現メッセージは年齢表現を変えただけで、メッセージの内容は原文と同じであるが、
#' それ以外のナッジ・メッセージは年齢表現だけでなく、行動経済学の知見に基づいたメッセージ内容に変更した。
#'
#' 利他強調メッセージ（*Altruistic* message）は
#' 自身が感染することで他人（特に、妊婦）にどのような損害を与えるかを具体的に記述している。
#' これは負の外部性を容易に想起させ、外部性を考慮する利他的な人の行動変容を促すことを目的としている。
#'
#' 利己強調メッセージ（*Selfish* message）・社会比較メッセージ（*Social comparison* message）は
#' 風しんの抗体を持つことの価値を高めることで行動変容を促すという目的で作成された。
#' 利己強調メッセージは自身が感染することで自分がどのような損害を受けるかを具体的に記述し、
#' 自身が感染することで生じる自身の損害を容易に想像できるようにした。
#' 社会比較メッセージは抗体保有率が低いことを明記して、自身が感染しやすいことを強調している。
#' これは風しんの感染確率を過小に見積もることを通じてワクチン接種の価値を過小に評価することを防ぐことができる。
#'
#' 有効期限メッセージ（*Valid data* message）・低コストメッセージ（*Low-cost* message）は
#' 風しんのクーポン券制度に関する内容に変更した。
#' 有効期限メッセージはクーポンの有効期限を強調する内容である。
#' 2019年度に配布されるクーポン券には、2020年3月31日が有効期限であることを明記していた。
#' このメッセージは再度その内容を明記した。
#' これは現在バイアスによって抗体検査の受験やワクチン接種が妨げられていることを防ぐ目的で作成した。
#' 低コストメッセージは健康診断のついでに抗体検査を受診できることを明記して、簡単に受検できることを強調する内容である。
#' このメッセージは抗体検査の主観的なコストを抑える目的で作成した。
#'
#' サンプルサイズが均等になるように、我々は7つのメッセージを年齢層別にランダムに割り当てた[^svyrandom]。
#' したがって、各群のサンプルサイズは600人である。
#'
#' [^svyrandom]: 調査会社が保有する年齢情報を用いて、40-44歳・45-49歳・50-54歳・55-59歳の層別にランダムに割り当てた。
#' 各層は1,050名で構成されていて、我々はナッジ・メッセージを均等に割り当てた（150名）。
#'
#' ランダムに割り当てられたメッセージを閲覧した直後、
#' 回答者は抗体検査受検とワクチン接種の意向について5段階で評価する。
#' 抗体検査受検の意向は「今、あなたは、風しんの抗体検査を受けようとどのくらい思っていますか」という質問である。
#' ワクチン接種の意向は「抗体検査を受けて、あなたに抗体がないと分かった場合、
#' あなたは、ワクチンを接種しようとどのくらい思いますか」という質問である。
#' それぞれの質問に対して、
#' 回答者は「絶対に受ける」「受ける」「どちらともいえない」「受けない」「絶対に受けない」「すでに受けた」で回答する。
#' 我々は、それぞれの意向について、「絶対に受ける」もしくは「受ける」と回答したら1を取るダミー変数を作成し、
#' それらを意向に関するアウトカム変数として用いる。
#'
#' ## Wave 2: Outcome Variables on Behavior {#wave2}
#'
#' 第2回調査は第1回調査以降の抗体検査の受検有無・ワクチン接種の有無を調査した。
#' 抗体検査の受検行動は
#' 「前回のアンケートの回答終了時から今日までの期間に、あなたは風しんの抗体検査を受診しましたか」
#' という質問である。
#' 回答者は「受診した」・「受診していない」・「前回アンケートより以前に、受診済みである」から一つ選ぶ。
#' 我々は「受検した」と回答したならば1を取るダミー変数を作成し、
#' それを抗体検査の受検率に関するアウトカム変数として用いる。
#'
#' ワクチン接種は
#' 「前回のアンケートの回答終了時から今日までの期間に、あなたは風しんワクチンを接種しましたか」
#' という質問である。
#' 回答者は「接種した」・
#' 「すでに抗体検査で十分に抗体があることを確認した・すでに風しんに感染したので、接種する必要がなかった」・
#' 「すでに抗体検査で十分に抗体がないことを確認したが、まだ接種していない」・
#' 「抗体検査の受診もワクチンの接種もしていない」・
#' 「前回アンケートより以前に、接種済みである」
#' から一つ選ぶ。
#' 厚生労働省の追加対策でワクチンを接種するためには、
#' 対象者は必ず抗体検査を受検しなければならない。
#' したがって、我々は抗体検査の受検行動の質問に対して「受検した」と回答し、
#' ワクチン接種の質問に対して「接種した」と回答すると1を取るダミー変数を作成し、
#' それをワクチン接種率に関するアウトカム変数として用いる。
#' 厚生労働省の政策目標は抗体を持っていない人がワクチン接種を受けることで抗体保有率を引き上げることである。
#' したがって、このアウトカム変数は政策目標に直結している。
#' ```
#'
# /*
#+
rmarkdown::render(
  here("src/2-summary-experiment.r"),
  output_dir = here("report/view")
)
# */