#+ include = FALSE
# library and relative paths
library(here)
source(here("code/_common.r"), encoding = "utf8")

#+ include = FALSE
vaccine <- read_csv(here(niid_path, "shape_vaccine.csv"))

#+ include = FALSE
antibody <- read_csv(here(niid_path, "shape_prevalence.csv")) %>%
  dplyr::filter(gender != "all") %>%
  mutate(
    antibody_rate_8 =
      (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total
  )

#+ niid-vaccine, eval = FALSE
vaccine %>%
  mutate(
    age = factor(age, levels = unique(vaccinedt$age)[c(31, 1:30)]),
    clear_total = total - others - unknown,
    rate_shot0 = no / clear_total,
    rate_shot1 = time1 / clear_total,
    rate_shot2 = time2 / clear_total
  ) %>%
  dplyr::select(age, gender, rate_shot0, rate_shot1, rate_shot2) %>%
  tidyr::pivot_longer(
    -(age:gender),
    names_prefix = "rate_", names_to = "shot"
  ) %>%
  mutate(shot = factor(shot, levels = c("shot2", "shot1", "shot0"))) %>%
  dplyr::filter(gender != "all") %>%
  ggplot(aes(x = age, y = value * 100, group = shot)) +
  geom_area(aes(fill = shot), color = "black") +
  scale_fill_brewer(
    palette = "Blues", direction = -1,
    name = "Number of inoculations",
    labels = c("2", "1", "0")
  ) +
  facet_wrap(~gender) +
  labs(x = "Age", y = "Fraction (%)") +
  simplegg() +
  theme(axis.text.x = element_text(hjust = 1, angle = 45))

#+ niid-antibody, eval = FALSE
antibody %>%
  mutate(gender = factor(
    gender,
    levels = c("female", "male"), labels = c("Females", "Males")
  )) %>%
  ggplot(aes(x = age, y = antibody_rate_8 * 100, group = gender)) +
  geom_point(aes(shape = gender), size = 3) +
  geom_line() +
  geom_polygon(
    data = tibble(x = c(39, 55, 55, 39), y = c(-Inf, -Inf, Inf, Inf)),
    aes(x = x, y = y),
    fill = "black", alpha = 0.1, inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = c(0, 10, 20, 28, 30, 39, 40, 50, 55, 60, 70)
  ) +
  labs(
    x = "Age",
    y = "Percentage of Rubella Antibody Carries (%)",
    shape = "Gender"
  ) +
  simplegg(axis_text_size = 11, font_family = "YuGothic")

#+ reg-niid-antibody, eval = FALSE
cell <- antibody %>%
  dplyr::filter(39 <= age) %>%
  mutate(g = case_when(
    age <= 55 ~ "<55",
    TRUE ~ ">55"
  )) %>%
  group_by(gender, g) %>%
  summarize(mu = mean(antibody_rate_8) * 100) %>%
  mutate(mu = sprintf("%1.1f%%", mu)) %>%
  rename(sex = gender)

reg <- antibody %>%
  mutate(
    age_group = case_when(
      age < 39 ~ "under39",
      age <= 55 ~ "b/w 39 and 55",
      TRUE ~ "over55"
    ),
    age_group = factor(
      age_group,
      levels = c("b/w 39 and 55", "under39", "over55")
    ),
    gender = factor(
      gender,
      levels = c("male", "female")
    )
  ) %>%
  lm(antibody_rate_8 ~ gender * age_group, data = .) %>%
  tidy() %>%
  mutate(
    label = sprintf(
      "%1.3f (std.error = %1.3f; p = %1.3f)",
      estimate, std.error, p.value
    )
  )


cat(c(
  "風しんの抗体はワクチン接種だけでなく、自然感染でも得られる。",
  "高齢者を中心に、風しんが流行していた期間に育った人ほど、風しんに自然感染した比率が高くなるので、",
  "風しんワクチンを接種していなくても抗体を保有している可能性が高くなる。",
  "図\\@ref(fig:show-niid-antibody)は国立感染症研究所（NIID）の",
  "2018年度感染症流行予測調査の男女別・年齢別の風しん抗体保有率をプロットしたものである。",
  "56歳以上の各年齢の抗体保有率の平均は、男女とも約90%である",
  paste0("（男性：", subset(cell, g == ">55" & sex == "male")$mu, "、"),
  paste0("女性：", subset(cell, g == ">55" & sex == "female")$mu, "）。"),
  "一方、39歳以上55歳以下の（1962年4月2日から1979年4月1日生まれ）の抗体保有率の平均は、",
  paste0("男性では、", subset(cell, g == "<55" & sex == "male")$mu, "、"),
  paste0("女性では、", subset(cell, g == "<55" & sex == "female")$mu, "である。"),
  "つまり、この年齢層の男性の抗体保有率は同世代の女性の抗体保有率より低い。",
  "これは39歳以上55歳以下の男性は風しんワクチンの定期接種の対象外である一方、",
  "39歳以上55歳以下の女性は中学生のときに風しんワクチンを接種していることを反映している。",
  "また、39歳以上55歳以下の男性の抗体保有率は56歳以上の男性のそれよりも低い。",
  "これは56歳以上の男性は、風しんの流行時期に育ったために風しんに自然感染する確率が高かったことを反映している[^stat_analysis]。",
  "\n",
  "[^stat_analysis]: このデータを用いて、3つの年齢層（38歳以下・39歳以上55歳以下・56歳以上）と",
  "女性ダミーの飽和モデルによって抗体保有率を予測した。",
  "その結果、39歳以上55歳以下の抗体保有率の男女差は",
  subset(reg, term == "genderfemale")$label,
  "である。また、39歳以上55歳以下の男性と56歳以上の男性の抗体保有率の差は",
  subset(reg, term == "age_groupover55")$label,
  "である。"
), sep = "\n")
