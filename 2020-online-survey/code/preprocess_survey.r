library(tidyverse)
library(foreign)
library(readstata13)
library(lubridate)
library(here)

pass <- "data/2020-online-survey"

#' ########################################
#' # Online survey experiment (Wave 1 & 2)
#' ########################################
#'
#' Step 1: 生データの読み込み
#'
#+
wave1 <- read_csv(
  here(pass, "raw", "survey-wave1.csv"),
  locale = locale(encoding = "cp932")
)

wave2 <- read_csv(
  here(pass, "raw", "survey-wave2.csv"),
  locale = locale(encoding = "cp932")
)

#'
#' Step 2: 調査会社項目をドロップ
#'
#+
wave1 <- wave1 %>%
  dplyr::select(
    -(pref_id:gender), -(age_step:occupation),
    -回答開始日時, -回答日時
  )

wave2 <- wave2 %>%
  dplyr::select(
    -(id:職業分類), -回答開始日時, -回答日時,
    -事前調査割付
  )

#'
#' Step 3: Wave 2のWave 1データをドロップ
#'
#+
wave2 <- wave2 %>% dplyr::select(-starts_with("p"))

#'
#' Step 4: 割り当て変数の作成
#'
#+
wave1 <- wave1 %>% dplyr::mutate(nudge = str_extract(assign, "[A-Z]"))

#'
#' Step 5: アウトカム変数の作成
#'
#' Wave 1の調査
#' - Q25: 抗体検査の意思
#' - Q26: ワクチン接種の意思（抗体がない場合）
#'
#' Wave 2の調査
#' - Q1: 前回アンケートから今日までの期間に、あなたは風しんの抗体検査を受診しましたか。
#' - Q4: 前回アンケートから今日までの期間に、あなたは風しんのワクチンを接種しましたか。
#'
#+
wave1 <- wave1 %>%
  rename(test = q25, vaccine = q26) %>%
  mutate_at(
    c("test", "vaccine"),
    list(
      scale3 = ~ case_when(. < 3 ~ 1, . < 6 ~ 2, . == 6 ~ 3),
      int = ~ ifelse(. < 3, 1, 0)
    )
  )

wave2 <- wave2 %>%
  rename(act_test = q1, act_vaccine = q4) %>%
  mutate(
    # recode raw response
    act_test = case_when(
      act_test == 1 ~ 1,
      act_test == 2 ~ 3,
      act_test == 3 ~ 2
    ),
    act_vaccine = case_when(
      act_vaccine == 1 ~ 1,
      act_vaccine == 2 ~ 4,
      act_vaccine < 5 ~ 3,
      act_vaccine == 5 ~ 2
    ),

    # dummy of before wave 1
    bw1_test = if_else(act_test == 2, 1, 0),
    bw1_vaccine = if_else(act_vaccine == 2, 1, 0),

    # dummy of after wave 1
    aw1_test = if_else(act_test == 1, 1, 0),
    aw1_vaccine = if_else(act_vaccine == 1, 1, 0),

    # dummy of before and after wave 1
    abw1_test = aw1_test + bw1_test,
    abw1_vaccine = aw1_vaccine + bw1_vaccine,

    # dummy of having and obtaining antibodies
    aw1_have_positive = aw1_vaccine + I(act_vaccine == 4),
    abw1_have_positive = abw1_vaccine + I(act_vaccine == 4),

    # test + having and obtaining antibodies
    aw1_testpositive = aw1_test * aw1_have_positive,
    abw1_testpositive = abw1_test * abw1_have_positive,

    # test + having antibodies
    aw1_testhave = aw1_test * I(act_vaccine == 4),
    abw1_testhave = abw1_test * I(act_vaccine == 4),

    # test + obtaining antibodies (vaccination)
    aw1_testvaccine = aw1_test * aw1_vaccine,
    abw1_testvaccine = abw1_test * abw1_vaccine
  )

#'
#' Step 6: 共変量の作成
#'
#' Wave 1
#' - Q9: クーポン券制度（1.知っていて、持っている、2.知っているが、持っていない、3.知らない）
#' - 教育水準 (q33)
#' - 既婚ダミー (q34)
#' - 運動習慣ダミー（週1回以上）(q1)
#' - 過去1年の健康診断（市区町村・勤務先の健診）(q2.1, q2.2)
#' - インフルエンザ予防接種（毎年）(q3)
#' - FBユーザー (q46, q47)
#' - 風しんリスク (q11)
#' - 一般的利他性（q17_2）
#' - 世帯所得（q44）
#'
#+
wave1 <- wave1 %>%
  rename(svyage = age) %>%
  mutate(
    # age and treatment of coupon in FY2019
    birth_year = q30_1t + 25 + 1900,
    birth_month = q30_2t,
    birth_date = as.Date(sprintf("%4d/%02d/02", birth_year, birth_month)),
    base_date = as.Date("2019/04/01"),
    age = trunc(time_length(birth_date %--% base_date, "year")),
    mage = trunc(time_length(birth_date %--% base_date, "month")),
    coupon2019 = if_else(40 <= age & age <= 46, 1, 0),

    # running variable of age
    run_age = 46 - age,
    run_mage = 563 - mage,

    # coupon knowledge
    coupon = q9,
    know_coupon = if_else(q9 != 3, 1, 0),
    have_coupon = if_else(q9 == 1, 1, 0),

    # past infection, antibodytest, vaccination
    exp_antibody = if_else(q7 == 1, 1, 0),
    exp_vaccine = if_else(q8 == 1, 1, 0),
    exp_infect = if_else(q6 == 1, 1, 0),

    # education and married dummy
    education = case_when(
      q33 %in% c(1, 2) ~ 9,
      q33 %in% c(3, 4, 6) ~ 12,
      q33 == 5 ~ 14,
      q33 %in% c(7, 8) ~ 16,
      q33 %in% c(9, 10) ~ 18,
      q33 == 11 ~ 21
    ),
    married = if_else(q34 == 1, 1, 0),

    # daily health behavior before wave 1
    exercise_w1 = if_else(q1 < 3, 1, 0),
    health_check = if_else(q2.1 == 1 | q2.2 == 1, 1, 0),
    flushot = if_else(q3 == 1, 1, 0),

    # facebook users
    fb = if_else(q46 == 1, 1, 0),
    fb_active = case_when(q47 <= 6 ~ 1, TRUE ~ 0),

    # knowledge of rubella
    severity = if_else(q11.1 == 1, 0, 1),
    handicap = q11.2,
    exp.again = q11.3,

    # generosity
    generosity = q17_2,

    # over-confidence
    prob_me = case_when(
      q13 == 2 ~ 0,
      q13 == 1 ~ q14 * 10
    ),
    prob_social = (q12 - 1) * 10,
    overconf_prob = prob_social - prob_me,
    overconf_noreason = case_when(
      q15 == 5 ~ 1,
      TRUE ~ 0
    ),

    # income
    income = dplyr::recode(
      q44,
      "1" = 50,
      "2" = 150,
      "3" = 300,
      "4" = 500,
      "5" = 700,
      "6" = 900,
      "7" = 1100,
      "8" = 1300,
      "9" = 1500,
      "10" = 1700,
      "11" = 1900,
      "12" = 2000,
      .default = NA_real_
    ),
    noinfo_income = if_else(q44 == 13, 1, 0)
  )

wave2 <- wave2 %>%
  dplyr::rename(
    handwash = q14_1,
    temp_check = q14_2,
    eat_habit = q14_3,
    avoid_out = q14_4,
    avoid_crowd = q14_5,
    wear_mask = q14_6,
    exercise_w2 = q14_7,
    avoid_hospital = q14_8
  )

#'
#' Step 7: 風しんワクチンのWTP・時間割引因子・リスク耐性の変数を作成
#'
#+
wave1 <- wave1 %>%
  mutate(seq_q16 = paste(
    q16_1, q16_2, q16_3, q16_4, q16_5, q16_6,
    q16_7, q16_8, q16_9, q16_10, q16_11,
    sep = ""
  )) %>%
  mutate(wtp_vaccine = case_when(
    seq_q16 == "11111111111" ~ 5000,
    seq_q16 == "21111111111" ~ 4000,
    seq_q16 == "22111111111" ~ 3000,
    seq_q16 == "22211111111" ~ 2000,
    seq_q16 == "22221111111" ~ 1000,
    seq_q16 == "22222111111" ~ 0,
    seq_q16 == "22222211111" ~ -1000,
    seq_q16 == "22222221111" ~ -2000,
    seq_q16 == "22222222111" ~ -3000,
    seq_q16 == "22222222211" ~ -4000,
    seq_q16 == "22222222221" ~ -5000,
    seq_q16 == "22222222222" ~ -6000
  )) %>%
  dplyr::select(-seq_q16)

wave2 <- wave2 %>%
  mutate(seq_q11 = paste(
    q11_1, q11_2, q11_3, q11_4, q11_5, q11_6,
    q11_7, q11_8, q11_9, q11_10, q11_11,
    sep = ""
  )) %>%
  mutate(wtp_vaccine_wave2 = case_when(
    seq_q11 == "11111111111" ~ 5000,
    seq_q11 == "21111111111" ~ 4000,
    seq_q11 == "22111111111" ~ 3000,
    seq_q11 == "22211111111" ~ 2000,
    seq_q11 == "22221111111" ~ 1000,
    seq_q11 == "22222111111" ~ 0,
    seq_q11 == "22222211111" ~ -1000,
    seq_q11 == "22222221111" ~ -2000,
    seq_q11 == "22222222111" ~ -3000,
    seq_q11 == "22222222211" ~ -4000,
    seq_q11 == "22222222221" ~ -5000,
    seq_q11 == "22222222222" ~ -6000
  )) %>%
  dplyr::select(-seq_q11)

wave1 <- wave1 %>%
  mutate(
    seq21 = paste(
      q21_1, q21_2, q21_3, q21_4, q21_5,
      q21_6, q21_7, q21_8, q21_9,
      sep = ""
    ),
    seq22 = paste(
      q22_1, q22_2, q22_3, q22_4, q22_5,
      q22_6, q22_7, q22_8, q22_9,
      sep = ""
    ),
    switch21 = str_locate(seq21, "2")[, 1],
    switch22 = str_locate(seq22, "2")[, 1]
  ) %>%
  mutate_at(c("switch21", "switch22"), list(~ ifelse(is.na(.), 10, .))) %>%
  mutate_at(
    c("switch21", "switch22"),
    list(~ case_when(
      . == 1 ~ 10000 / 9980,
      . == 2 ~ 10000 / ((9980 + 10000) / 2),
      . == 3 ~ 10000 / ((10000 + 10019) / 2),
      . == 4 ~ 10000 / ((10019 + 10076) / 2),
      . == 5 ~ 10000 / ((10076 + 10191) / 2),
      . == 6 ~ 10000 / ((10191 + 10383) / 2),
      . == 7 ~ 10000 / ((10383 + 10575) / 2),
      . == 8 ~ 10000 / ((10575 + 11917) / 2),
      . == 9 ~ 10000 / ((11917 + 19589) / 2),
      . == 10 ~ 10000 / 19589,
    ))
  ) %>%
  rename(df_present = switch21, df_future = switch22) %>%
  mutate(pbias = if_else(df_present < df_future, 1, 0)) %>%
  dplyr::select(-seq21, -seq22)

wave1 <- wave1 %>%
  mutate(
    seqg = paste(
      q23_1, q23_2, q23_3, q23_4, q23_5, q23_6,
      q23_7, q23_8, q23_9, q23_10,
      sep = ""
    ),
    seql = paste(
      q24_1, q24_2, q24_3, q24_4, q24_5, q24_6,
      q24_7, q24_8, q24_9, q24_10,
      sep = ""
    ),
    switchg = str_locate(seqg, "2")[, 1],
    switchl = str_locate(seql, "2")[, 1]
  ) %>%
  mutate_at(c("switchg", "switchl"), list(~ ifelse(is.na(.), 11, .))) %>%
  mutate(
    wtp_gain = case_when(
      switchg == 1 ~ 10,
      switchg == 2 ~ (10 + 2000) / 2,
      switchg == 3 ~ (2000 + 4000) / 2,
      switchg == 4 ~ (4000 + 8000) / 2,
      switchg == 5 ~ (8000 + 15000) / 2,
      switchg == 6 ~ (15000 + 25000) / 2,
      switchg == 7 ~ (25000 + 35000) / 2,
      switchg == 8 ~ (35000 + 45000) / 2,
      switchg == 9 ~ (45000 + 50000) / 2,
      switchg == 10 ~ (50000 + 55000) / 2,
      switchg == 11 ~ 55000
    ),
    wtp_loss = case_when(
      switchl == 1 ~ 1000,
      switchl == 2 ~ (1000 + 5000) / 2,
      switchl == 3 ~ (5000 + 10000) / 2,
      switchl == 4 ~ (10000 + 15000) / 2,
      switchl == 5 ~ (15000 + 20000) / 2,
      switchl == 6 ~ (20000 + 30000) / 2,
      switchl == 7 ~ (30000 + 40000) / 2,
      switchl == 8 ~ (40000 + 45000) / 2,
      switchl == 9 ~ (45000 + 50000) / 2,
      switchl == 10 ~ (50000 + 55000) / 2,
      switchl == 11 ~ 55000,
    )
  ) %>%
  mutate_at(c("wtp_gain", "wtp_loss"), list(~ . / 10000)) %>%
  dplyr::select(-seqg, -seql, -switchg, -switchl)

#'
#' Step 9: 未使用変数のドロップ
#'
#+
wave1 <- wave1 %>% dplyr::select(-starts_with("q"))
wave2 <- wave2 %>% dplyr::select(-starts_with("q"))

#'
#' Step 10: IDによるWave 1 & 2のマージ
#+
wave2 <- wave2 %>%
  mutate(follow = 1) %>%
  rename(id = 事前調査id)

wave1 <- wave1 %>%
  left_join(wave2, by = "id") %>%
  mutate(follow = if_else(is.na(follow), 0, follow))

#'
#' Step 11: CSVファイルへの書き出し
#'
#+
write_csv(wave1, file = here(pass, "shape_survey.csv"))
