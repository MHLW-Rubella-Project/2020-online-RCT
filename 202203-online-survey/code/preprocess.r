library(tidyverse)
library(foreign)
library(readstata13)
library(lubridate)
library(here)

pass <- "202203-online-survey"

# read raw data
raw <- read_csv(here(pass, "raw/raw.csv"))

# assignment information
assign <- raw %>%
  mutate(
    treated = if_else(bq1 == 3 | bq1 == 4, 1, 0), #リーフレット介入ならば1
    order = if_else(bq1 == 1 | bq1 == 3, 1, 0) #風疹関連の質問が先ならば1
  ) %>%
  dplyr::select(id = names(raw)[1], treated, order)

# past experience
past <- raw %>%
  mutate(
    past_rubella_vaccine = if_else(q1 == 1, 1, 0),
    past_rubella_infect = if_else(q2 == 1, 1, 0),
    past_varicella_vaccine = if_else(q3_1 == 1, 1, 0),
    past_measles_vaccine = if_else(q3_2 == 1, 1, 0),
    past_varicella_infect = if_else(q4_1 == 1, 1, 0),
    past_measles_infect = if_else(q4_2 == 1, 1, 0)
  ) %>%
  dplyr::select(id = names(raw)[1], starts_with("past"))

# coupon awareness and why not take antibody test
present <- raw %>%
  mutate(
    coupon_aware = if_else(q5 == 1, 1, 0),
    reason_notest = dplyr::recode(
      q6,
      "1" = "そもそも厚生労働省のクーポン券制度を知らなかったから",
      "2" = "過去に風しんに感染したことがあるから",
      "3" = "その他",
      "4" = "特に理由はない"
    )
  ) %>%
  dplyr::select(id = names(raw)[1], coupon_aware, reason_notest)

# intention
int <- raw %>%
  mutate(
    before_int_test = if_else(q0 == 1 | q0 == 2, 1, 0),
    after_int_test = if_else(is.na(q7f1), q8f1, q7f1),
    after_int_test = if_else(after_int_test %in% c(1, 2), 1, 0),
    after_int_vaccine = if_else(is.na(q7f2), q8f2, q7f2),
    after_int_vaccine = if_else(after_int_vaccine %in% c(1, 2), 1, 0)
  ) %>%
  dplyr::select(
    id = names(raw)[1],
    before_int_test,
    after_int_test,
    after_int_vaccine
  )

# emotional effect
emo <- raw %>%
  rename(
    positive_act = q9_1,
    force_act = q9_2,
    negative_emo = q9_3,
    improve = q9_4
  ) %>%
  mutate_at(
    vars(positive_act, force_act, negative_emo, improve),
    list(~ ifelse(. %in% c(4, 5), 1, 0))
  ) %>%
  dplyr::select(
    id = names(raw)[1],
    positive_act,
    force_act,
    negative_emo,
    improve
  )

# individual characteristics
personal <- raw %>%
  dplyr::mutate(
    birth_year = sq2_1t + 25 + 1900,
    birth_month = sq2_2t,
    birth_date = as.Date(sprintf("%4d/%02d/01", birth_year, birth_month)),
    age = trunc(
      time_length(birth_date %--% as.Date("2022/04/01"), unit = "year")
    ),
    educ = dplyr::case_when(
      q10 %in% c(1, 2) ~ 9,
      q10 %in% c(3, 4, 6) ~ 12,
      q10 == 5 ~ 14,
      q10 %in% c(7, 8) ~ 16,
      q10 %in% c(9, 10) ~ 18,
      q10 == 11 ~ 21
    ),
    married = if_else(q11 == 1, 1, 0),
    num_member = q12t,
    with_older = if_else(q13_1 == 1, 1, 0),
    with_20 = if_else(q13_2 == 1, 1, 0),
    with_15 = if_else(q13_3 == 1, 1, 0),
    with_12 = if_else(q13_4 == 1, 1, 0),
    with_under6 = if_else(q13_5 == 1, 1, 0),
    occupation = q14,
    inc = dplyr::case_when(
      q15 == 1 ~ 100,
      q15 == 2 ~ 150,
      q15 == 3 ~ 300,
      q15 == 4 ~ 500,
      q15 == 5 ~ 700,
      q15 == 6 ~ 900,
      q15 == 7 ~ 1100,
      q15 == 8 ~ 1300,
      q15 == 9 ~ 1500,
      q15 == 10 ~ 1700,
      q15 == 11 ~ 1900,
      q15 == 12 ~ 2000,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(
    id = names(raw)[1],
    age, educ, married, num_member, occupation, inc,
    starts_with("with")
  )

# merge and write csv
assign %>%
  dplyr::left_join(past, by = "id") %>%
  dplyr::left_join(present, by = "id") %>%
  dplyr::left_join(int, by = "id") %>%
  dplyr::left_join(emo, by = "id") %>%
  dplyr::left_join(personal, by = "id") %>%
  readr::write_csv(here(pass, "shape.csv"))
