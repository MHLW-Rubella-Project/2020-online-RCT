library(tidyverse)
library(lubridate)
library(here)

pass <- "data/2018-NIID-survey"

# Antibody prevalence data
niid1 <- read_csv(here(pass, "raw", "prevalence.csv"))

age0_niid1 <- niid1 %>%
  dplyr::filter(str_detect(age, "M")) %>%
  group_by(gender) %>%
  dplyr::select(-age) %>%
  summarize_all(list(~ sum(.))) %>%
  mutate(age = 0)

niid1 <- niid1 %>%
  dplyr::filter(!str_detect(age, "M")) %>%
  mutate(age = dplyr::recode(age, "70-" = 70, .default = as.numeric(age))) %>%
  bind_rows(age0_niid1) %>%
  mutate(prevalence = 100 * (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total)

write_csv(niid1, file = here(pass, "shape_prevalence.csv"))

# Vaccination data
niid2 <- read_csv(here(pass, "raw", "vaccine.csv"))

niid2 <- niid2 %>%
  mutate(
    time1 = R + MR + MMR,
    time2 = R_MR + MR_MR + R_R
  )

age0_niid2 <- niid2 %>%
  dplyr::filter(str_detect(age, "M")) %>%
  group_by(gender) %>%
  dplyr::select(-age) %>%
  summarize_all(list(~ sum(.))) %>%
  mutate(age = "0")

niid2 <- niid2 %>%
  dplyr::filter(!str_detect(age, "M")) %>%
  bind_rows(age0_niid2)

write_csv(niid2, file = here(pass, "shape_vaccine.csv"))
