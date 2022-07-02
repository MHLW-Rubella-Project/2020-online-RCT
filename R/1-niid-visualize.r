#' ---
#' title: Background of Rubella Vaccination in Japan
#' subtitle: Preview
#' author: Hiroki Kato
#' ---
#'
#+ include = FALSE
# library and relative paths
library(here)
source(here("R/_common.r"), encoding = "utf8")

#'
#+ include = FALSE
antibody <- read_csv(here(niid_path, "shape_prevalence.csv")) %>%
  dplyr::filter(gender != "all") %>%
  mutate(
    antibody_rate_8 =
      (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total
  )

cell <- antibody %>%
  mutate(g = case_when(
    age < 39 ~ "39 < age",
    age <= 55 ~ "39 <= age <= 55",
    TRUE ~ "55 < age"
  )) %>%
  group_by(gender, g) %>%
  summarize(mu = mean(antibody_rate_8) * 100) %>%
  mutate(mu = sprintf("%1.1f%%", mu)) %>%
  rename(sex = gender)

#+ niid-antibody, fig.cap = 'Percentage of Rubella Antibody Carriers at Each Age by Gender. Data: NIID "2018 National Epidemiological Surveillance of Vaccine-Preventable Diseases (NESVPD)."', out.extra=""
plot_antibody <- antibody %>%
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
  simplegg(axis_text_size = 11)

plot_antibody

ggsave(
  here("figures", "niid-antibody.pdf"),
  plot = plot_antibody,
  width = 10,
  height = 6
)

#'
#+ reg-niid-antibody
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

reg
