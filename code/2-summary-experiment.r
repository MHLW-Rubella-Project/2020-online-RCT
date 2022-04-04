#+ include = FALSE
library(here)
source(here("code/_library.r"))

#+ include = FALSE
web <- read_csv(here(rct_path, "shape_survey.csv"))

#+ flowchart, eval = FALSE
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

#+ reg-attrition, include = FALSE
attr_f <- anova(lm(I(1 - follow) ~ nudge, data = web))[["F value"]][1]
attr_p <- anova(lm(I(1 - follow) ~ nudge, data = web))[["Pr(>F)"]][1]
attr_label <- sprintf("F-value = %1.3f (p-value = %1.3f)", attr_f, attr_p)


#+ covlist, eval = FALSE
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

#+ nudgelist, eval = FALSE
message_list <- readr::read_csv(
  here("assets/nudge_descript.csv"),
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
