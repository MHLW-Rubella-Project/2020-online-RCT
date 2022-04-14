#+ include = FALSE
library(here)
source(here("code/_common.r"), encoding = "utf8")

#+ include = FALSE
web <- read_csv(here(rct_path, "shape_survey.csv"))

#+ flowchart, eval = FALSE
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

#+ reg-attrition, eval = FALSE
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

#+ nudgelist, eval = FALSE
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
