#'---
#' title: Online Survey Experiment
#' subtitle: Preview
#' author: Hiroki Kato
#' output:
#'   bookdown::html_document2:
#'     toc: true
#'     toc_float: true
#'     number_sections: false
#' ---
#'
#+ include = FALSE
library(here)
source(here("R/_common.r"), encoding = "utf8")

#+ include = FALSE
source(here("R/_html_header.r"), encoding = "utf8")

#+ include = FALSE
web <- read_csv(here(rct_path, "shape_survey.csv"))

#+ flowchart, fig.width = 25, fig.height = 15, out.width = "150%", fig.cap = "Overview of Online Survey Experiment", out.extra = "angle=90"
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

plot_flowchart <- textdt %>%
  ggplot(aes(x = x, y = y)) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = "black", fill = "white"
  ) +
  geom_text(aes(label = label), size = 6) +
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

plot_flowchart

ggsave(
  here("figures", "flowchart.pdf"),
  plot = plot_flowchart,
  width = 25,
  height = 15
)

#'
#+ reg-attrition
attr_f <- anova(lm(I(1 - follow) ~ nudge, data = web))[["F value"]][1]
attr_p <- anova(lm(I(1 - follow) ~ nudge, data = web))[["Pr(>F)"]][1]
attr_label <- sprintf("F-value = %1.3f (p-value = %1.3f)", attr_f, attr_p)
attr_label

#+ covariate-list
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

paste(cov, collapse = "+") %>%
  paste("~ Mean + (`Std.Dev.` = SD)") %>%
  as.formula() %>%
  modelsummary::datasummary(
    data = web,
    add_columns = descript,
    align = "llcc",
    title = "List of Covariates \\label{tab:covariate-list}",
    label = "tab:cov-list",
    output = here("tables", "covariate-list.tex")
  )

#'
#+ nudge-list
message_list <- readr::read_csv(
  here("assets/nudge_descript.csv"),
  local = locale(encoding = "cp932")
) %>% dplyr::select(Contents)

attr(message_list, "position") <- 2

web %>%
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
    title = "List of Text-Based Nudges \\label{tab:nudge-list}",
    linesep = "\\addlinespace",
    output = here("tables", "nudge-list.tex")
  )

# /*
#+
rmarkdown::render(
  here("R/2-summary-experiment.r"),
  output_dir = here("docs/html-preview")
)
# */