library(tidyverse)
library(lubridate)
library(here)

pass <- "data/2017-NIID-survey"

# Antibody prevalence data
raw <- read_csv(
  here(pass, "raw", "NESVPD-antibody-stack.csv"),
  locale = locale(encoding = "cp932")
)

col_rename <- raw %>%
  rename(
    age = "年齢",
    total = "合計",
    HI_less8 = "<8",
    HI8 = "8",
    HI16 = "16",
    HI32 = "32",
    HI64 = "64",
    HI128 = "128",
    HI256 = "256",
    HI512 = "512",
    HI_more1024 = "1024-",
    antibody_rate_8 = "≧8抗体保有率（%）"
  ) %>%
  select(-antibody_rate_8)

age0 <- col_rename %>%
  dplyr::filter(str_detect(age, "M")) %>%
  group_by(gender) %>%
  dplyr::select(-age) %>%
  summarize_all(list(~ sum(.))) %>%
  mutate(age = 0)

stacked <- col_rename %>%
  dplyr::filter(!str_detect(age, "M")) %>%
  mutate(age = dplyr::recode(age, "70-" = 70, .default = as.numeric(age))) %>%
  bind_rows(age0) %>%
  mutate(prevalence = 100 * (HI8 + HI16 + HI32 + HI64 + HI128 + HI256 + HI512 + HI_more1024) / total)

write_csv(stacked, file = here(pass, "shape_NESVPD.csv"))
