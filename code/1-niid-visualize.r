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
    levels = c("female", "male"), labels = c("女性", "男性")
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
  labs(x = "年齢", y = "風しん抗体保有率（%）", shape = "性別") +
  simplegg(axis_text_size = 11, font_family = "YuGothic")

#+ reg-niid-antibody, include = FALSE
over55 <- antibody %>%
  dplyr::filter(age > 55) %>%
  group_by(gender) %>%
  summarize(mu = mean(antibody_rate_8) * 100)

age39_55 <- antibody %>%
  dplyr::filter(39 <= age & age <= 55) %>%
  group_by(gender) %>%
  summarize(mu = mean(antibody_rate_8) * 100)

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