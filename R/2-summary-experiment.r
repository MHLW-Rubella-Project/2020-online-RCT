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
  paste("600 males read", "MHLW (Control) message", sep = "\n"),
  45, 125, 32, 58, 110, 140,
  paste("600 males read", "MHLW (Age) message", sep = "\n"),
  75, 125, 62, 88, 110, 140,
  paste("600 males read", "Altruistic message", sep = "\n"),
  105, 125, 92, 118, 110, 140,
  paste("600 males read", "Selfish message", sep = "\n"),
  135, 125, 122, 148, 110, 140,
  paste("600 males read", "Social comparison message", sep = "\n"),
  165, 125, 152, 178, 110, 140,
  paste("600 males read", "Deadline message", sep = "\n"),
  195, 125, 182, 208, 110, 140,
  paste("600 males read", "Convenient message", sep = "\n"),
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
#'
#' 本研究は、厚生労働省とのコラボレーションの下で、
#' インターネット調査会社であるマイボイスコム株式会社に委託して、
#' 全国規模のオンライン調査を2回実施した。
#' 我々は第１回調査を、2020年2月15日-17日に、
#' 日本全国に居住する40-59歳の男性4,200名を対象にして実施した。
#' 彼らは追加対策の対象で、
#' 2019年にクーポン券が自動送付された人々とされなかった人々の両方が含まれている。
#' 第１回調査の目的は、ナッジ・メッセージをランダムに割当て、
#' それらの介入が風しんの予防意向にどのような影響を与えるかを検証することである。
#' 続いて、我々は第２回調査を、2020年3月17日-25日に、
#' 第１回調査の回答者を対象にして行い、3,963名から回答を得た（脱落率＝5.64%）。
#' 第２回調査の目的は、第１回目調査でランダムに割り当てたナッジ・メッセージが
#' 実際の予防行動にどのような影響を与えるかを検証することである。
#'
#' 調査の詳細は補論Bに示した。
#' また、我々はオンライン調査上のランダム化比較試験を実施するときに、
#' 大阪大学経済学研究科の倫理委員会の承認を事前に取得している（承認番号：R020114）。
#' ```
#'
#' ```{asis, echo = params$appendix}
#' 本研究が実施したオンライン調査の概要を説明する。
#' 我々はインターネット調査会社であるマイボイスコム株式会社（MyVociceCome Co. Ltd.）に委託し、
#' 2回のオンライン調査を実施した。
#' 図\@ref(fig:flowchart)に調査の流れを示した。
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
  "income", "noinfo_income",
  "exercise_w1", "health_check", "flushot",
  # "prob_social", "handicap", "severity",
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

#'
#' ```{asis, echo = params$preview | !params$appendix}
#' ## Wave 1: Treatments and Outcome Variables on Intention {#wave1}
#' ```
#'
#' ```{asis, echo = params$appendix}
#' 第1回調査は2020年2月15日から2020年2月17日に実施した。
#' 第1回調査の対象は調査会社のモニターのうち、日本全国に居住する40歳から59歳の男性の4,200名である。
#' 第1回調査の目的はナッジ・メッセージをランダムに割り当て、
#' それが風しんの予防行動の意思にどのような影響を与えるかを検証することである。
#' ナッジ・メッセージを見せる前に、
#' 第1回調査の参加者は日常的な健康行動・風しんの知識・風しん感染歴・ワクチン接種歴などの
#' 質問に回答する（Questionnaire A）。
#' 表\@ref(tab:covlist)にバランステストや回帰分析に使用する共変量を示した。
#'
#' 次に、7つのメッセージをランダムに1つ調査画面上に表示した。
#' ランダムに表示されたメッセージを確認した後、
#' 回答者はQuestionnaire Bに移動する。
#' この質問票では、抗体検査やワクチン接種の意向・
#' 生年月・婚姻歴・教育年数などを含んでいる。
#' 生年月から計算された年齢や婚姻歴などの個人属性の記述統計を表\@ref(tab:covlist)に示した。
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
    title = "List of Text-Based Nudges",
    linesep = "\\addlinespace"
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
#' 我々は、表\@ref(tab:nudgelist)のコントロール・メッセージと
#' ナッジ・メッセージの中からいずれか一つをランダムに提供した[^svyage]。
#' 厚労省メッセージ（*MHLW (Control)* message）がコントロールメッセージで、
#' 風しんの抗体検査とワクチン接種を勧奨するホームページ上で用いているものである
#' （*business-as-usual control*）。
#' 我々はどのような要素をどのような表現で強調することが効果的なのかを探るために、
#' 厚労省メッセージに基づいた6種類の異なるナッジ・メッセージを作成した。
#' 近年の行動科学研究には、候補となるナッジ・メッセージを複数個用意して、
#' どれが効果的なのかを探索するもの[e.g. @Dai2021; @Milkman2021]が増えており、
#' 本研究もその流れに属する。
#'
#' [^svyage]: 表\@ref(tab:nudgelist)に示した年齢は調査によって得られた誕生年と誕生月を用いて、
#' 2019年4月時点の年齢を計算した。
#' 2019年4月時点で40歳から56歳の男性が厚労省の追加的対策の対象であり、
#' 40歳から46歳の男性は1年目にクーポン券を自動的に受け取る。
#' 4月生まれの人はまだ誕生日を迎えていないことを仮定している。
#' また、2019年4月時点での年齢であるため、調査時点で40歳の男性の一部が39歳である。
#'
#' ナッジ・メッセージは、厚労省メッセージを(1)簡易な年齢表現と(2)行動経済学に基づいたメッセージ内容に変更している。
#' 年齢表現メッセージ（*MHLW (Age)* message）は風しんの追加対策の正確な対象年齢に加えて、
#' 「40代・50代の男性」という平易な表現を追加した。
#' これは自分が接種対象者であるかどうかが容易に理解できるようにして、メッセージ自体の注意を引くことを目的としている。
#' 年齢表現メッセージは年齢表現を変えただけで、メッセージの内容は原文と同じである。
#'
#' それ以外の5つのナッジ・メッセージは年齢表現だけでなく、
#' 行動経済学の知見に基づいたメッセージ内容に変更した。
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
#' 有効期限メッセージ（*Deadline* message）・低コストメッセージ（*Convenient* message）は
#' 風しんのクーポン券制度に関する内容に変更した。
#' 有効期限メッセージはクーポンの有効期限を強調する内容である。
#' 2019年度に配布されるクーポン券には、2020年3月31日が有効期限であることを明記していた。
#' このメッセージは再度その内容を明記した。
#' これは行動経済学的特性の一つである現在バイアスによって
#' 抗体検査の受験やワクチン接種の実行が先延ばしされるのを防ぐ目的で作成した。
#' 低コストメッセージは健康診断のついでに抗体検査を受診できることを明記して、簡単に受検できることを強調する内容である。
#' このメッセージは抗体検査の主観的なコストを抑える目的で作成した。
#'
#' サンプルサイズが均等になるように、我々は7つのメッセージを年齢層別にランダムに割り当てた[^svyrandom]。
#' したがって、各群のサンプルサイズは600人である。
#'
#' [^svyrandom]: 調査会社が保有する年齢情報を用いて、40-44歳・45-49歳・50-54歳・55-59歳の層別にランダムに割り当てた。
#' 各層は1,050名で構成されていて、我々はナッジ・メッセージを均等に割り当てた（150名）。
#'
#' 第1回調査では、ランダムに割り当てられたメッセージを閲覧した直後、
#' 回答者は抗体検査受検とワクチン接種の意向について5段階で評価する。
#' 抗体検査受検の意向は「今、あなたは、風しんの抗体検査を受けようとどのくらい思っていますか」という質問である。
#' ワクチン接種の意向は「抗体検査を受けて、あなたに抗体がないと分かった場合、
#' あなたは、ワクチンを接種しようとどのくらい思いますか」という質問である。
#' それぞれの質問に対して、
#' 回答者は「絶対に受ける」「受ける」「どちらともいえない」「受けない」「絶対に受けない」「すでに受けた」で回答する。
#' 我々は、それぞれの意向について、「絶対に受ける」もしくは「受ける」と回答したら1を取るダミー変数を作成し、
#' それらを意向に関するアウトカム変数として用いる。
#' ```
#'
#' ```{asis, echo = params$preview | !params$appendix}
#' ## Wave 2: Outcome Variables on Behavior {#wave2}
#'
#' 第2回調査は第1回調査以降の実際の抗体検査の受検行動・ワクチン接種の行動を調査した。
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
#' 厚生労働省の追加対策でワクチンを接種するためには、対象者は必ず抗体検査を受検しなければならない。
#' 同時に、抗体検査を受けずに、自費で風しんワクチンを接種している可能性もある。
#' この可能性を排除するために、
#' 我々は抗体検査の受検行動の質問に対して「受検した」と回答し、
#' ワクチン接種の質問に対して「接種した」と回答すると1を取るダミー変数を作成し、
#' それをワクチン接種率に関するアウトカム変数として用いる。
#' 厚生労働省の政策目標は抗体を持っていない人がワクチン接種を受けることで抗体保有率を引き上げることなので、
#' このアウトカム変数は政策目標に直結している。
#' ```
#'
#' ```{asis, echo = params$appendix}
#' 第2回調査は第1回調査の追跡調査であり、2020年3月17日から2020年3月25日に実施した。
#' 第2回調査は第1回調査の回答者全員を対象として、3,963名から回答を得た（脱落率=5.64%）。
#' 第2回調査の目的は第1回調査でランダムに割り当てたナッジ・メッセージが
#' 実際の予防行動にどのような影響を与えるかを検証することである。
#'
#' 第2回調査では、第1回調査以降に抗体検査やワクチン接種を受けたかどうかを調査した。
#' 加えて、新型コロナウイルスの流行に伴い、
#' 手洗いや人混みを避けるなどの日常的な感染予防行動に関する調査をした。
#' 日常的な感染予防行動の変数はバランステストや回帰分析で用いる。
#' 表\@ref(tab:covlist)にそれらの変数の記述統計を示した。
#' ```
#'
# /*
#+
rmarkdown::render(
  here("src/2-summary-experiment.r"),
  output_dir = here("report/view")
)
# */