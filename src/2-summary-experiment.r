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
  "4,200 males participated in wave 1",
  105, 175, 75, 135, 160, 190,
  paste(
    "4,200 males answered Questionnaire A",
    sep = "\n"
  ),
  15, 125, 2, 28, 110, 140,
  paste(
    "600 males read", "MHLW message",
    sep = "\n"
  ),
  45, 125, 32, 58, 110, 140,
  paste(
    "600 males read", "Age expression message",
    sep = "\n"
  ),
  75, 125, 62, 88, 110, 140,
  paste(
    "600 males read", "Altruistic message",
    sep = "\n"
  ),
  105, 125, 92, 118, 110, 140,
  paste(
    "600 males read", "Selfish message",
    sep = "\n"
  ),
  135, 125, 122, 148, 110, 140,
  paste(
    "600 males read", "Social comparison message",
    sep = "\n"
  ),
  165, 125, 152, 178, 110, 140,
  paste(
    "600 males read", "Valid date message",
    sep = "\n"
  ),
  195, 125, 182, 208, 110, 140,
  paste(
    "600 males read", "Low-cost message",
    sep = "\n"
  ),
  15, 25, 2, 28, 10, 40,
  paste(
    "571 males", "participated in wave 2",
    sep = "\n"
  ),
  45, 25, 32, 58, 10, 40,
  paste(
    "578 males", "participated in wave 2",
    sep = "\n"
  ),
  75, 25, 62, 88, 10, 40,
  paste(
    "557 males", "participated in wave 2",
    sep = "\n"
  ),
  105, 25, 92, 118, 10, 40,
  paste(
    "563 males", "participated in wave 2",
    sep = "\n"
  ),
  135, 25, 122, 148, 10, 40,
  paste(
    "562 males", "participated in wave 2",
    sep = "\n"
  ),
  165, 25, 152, 178, 10, 40,
  paste(
    "566 males", "participated in wave 2",
    sep = "\n"
  ),
  195, 25, 182, 208, 10, 40,
  paste(
    "566 males", "participated in wave 2",
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
      "600 males answered", "Questionnaire B",
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
    "Questionnaire A investigated daily health behaviors,",
    "knowledge of rubella, infection history, and vaccination history.",
    "Questionnaire B investigated the intention to be tested for antibody to",
    "rubella and to be vaccinated, as well as socioeconomic attributes.",
    "Wave 2 surveyed the behavior of antibody testing",
    "and vaccination against rubella since Wave 1.",
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

#' ```{asis, echo = params$preview | !params$appendix}
#' 我々はインターネット調査会社であるマイボイスコム株式会社（MyVociceCome Co. Ltd.）に委託して、合計2回のオンライン調査を実施した。
#' 補論Aに調査の流れを示した。
#' 第1回調査は2020年2月15日から2020年2月17日に実施した。
#' 第1回調査の対象は調査会社のモニターのうち、日本全国に居住する40歳から59歳の男性の4,200名である。
#' 第2回調査は2020年3月17日から2020年3月25日に実施した。
#' 第2回調査は第1回調査の回答者全員を対象として、3,963名から回答を得た（脱落率=5.64%）[^attrition]。
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
#' ## 第1回調査 {#wave1}
#'
#' 第1回調査の目的はナッジ・メッセージをランダムに割り当て、
#' それが風しんの予防行動の意思にどのような影響を与えるかを検証することである。
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
#' 質問票Aの回答終了後、回答者は7つのメッセージのうち一つをランダムに受け取る。
#' サンプルサイズが均等になるように、メッセージを年齢層別にランダムに割り当てた[^svyrandom]。
#' 各メッセージのサンプルサイズは600人である。
#' 表\@ref(tab:nudgelist)はメッセージの一覧とサンプルサイズを示している。
#' 表\@ref(tab:nudgelist)に示した年齢は調査によって得られた誕生年と誕生月を用いて、2019年4月時点の年齢を計算した[^svyage]。
#' 2019年4月時点で40歳から56歳の男性が厚労省の追加的対策の対象であり、
#' 40歳から46歳の男性は1年目にクーポン券を自動的に受け取る。
#'
#' [^svyrandom]: 調査会社が保有する年齢情報を用いて、40～44歳・45～49歳・50～54歳・55～59歳の層別にランダムに割り当てた。
#' 各層は1,050名で構成されていて、我々はナッジ・メッセージを均等に割り当てた（150名）。
#'
#' [^svyage]: 4月生まれの人はまだ誕生日を迎えていないことを仮定している。
#' また、2019年4月時点での年齢であるため、調査時点で40歳の男性の一部が39歳である。
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
#' これらの変数は共変量として用いる（補論A）。
#'
#' ## 第2回調査 {#wave2}
#'
#' 第2回調査の目的は第1回調査でランダムに割り当てたナッジ・メッセージが
#' 実際の予防行動にどのような影響を与えるかを検証することである。
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
#'
#' これらの回答を用いて、二つのアウトカム変数を作成する。
#' 第一のアウトカム変数は抗体検査の受検である。
#' 回答者が抗体検査を「受検した」と回答すると1を取るダミー変数を作成した。
#' この変数を用いて、我々は第1回調査以降の抗体検査の受検率に対するナッジ・メッセージの効果を推定する。
#' 第二のアウトカム変数は抗体検査を通じたワクチン接種である。
#' 回答者が抗体検査を「受検した」と回答し、
#' ワクチンを「接種した」と回答すると1を取るダミー変数を作成した。
#' 厚生労働省の政策目標は抗体を持っていない人がワクチン接種を受けることで抗体保有率を引き上げることである。
#' したがって、このアウトカム変数は政策目標に直結している。
#'
#' [^covid]: 第2回調査の調査時期は新型コロナウイルスの流行が始まった時期と重なるので、
#' それが健康行動などに大きな変化を与えた可能性がある。
#' この可能性をコントロールするために、日常の詳細な健康行動についても調査した。
#' その回答は共変量として用いる（補論A）。
#' ```
#'
# /*
#+
rmarkdown::render(
  here("src/2-summary-experiment.r"),
  output_dir = here("report/view")
)
# */