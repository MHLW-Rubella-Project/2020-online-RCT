library(tidyverse)
library(foreign)
library(readstata13)
library(lubridate)
library(here)

pass <- "data/2020-online-survey"

#'
#' ########################################
#' # Facebookオンラインサーベイデータ
#' ########################################
#'
#' Step 1: 生データの読み込み
#'
#+
fbweb <- read_csv(
  here(pass, "raw", "fb-survey.csv"),
  locale = locale(encoding = "cp932"),
  col_types = cols(
    q31_7t = col_character()
  )
)

#'
#' Step 2: 調査会社の変数を削除
#'
#+
fbweb <- fbweb %>%
  dplyr::select(- (割付:性年代), -回答開始日時, -回答日時)

#'
#' Step 3: FBユーザーと広告をみたかどうかの変数を作成
#'
#' - Q4: アカウントの有無
#' - Q5: アクティブユーザーの有無（1日1回以上）
#' - Q16.4: FB広告を見たかどうか
#'
#+
fbweb <- fbweb %>%
  mutate(
    fb = if_else(q4 == 1, 1, 0),
    fb_active = case_when(q5 <= 6 ~ 1, TRUE ~ 0)
  ) %>%
  rename(fbad = q16.4)

#'
#' Step 4: アウトカム変数
#'
#' - Q7: 抗体検査の受診（Q8で時期、Q9で意思）
#' - Q11: ワクチン接種（Q12で時期、Q13で意思）
#'
#+
fbweb <- fbweb %>%
  mutate(
    act_test = case_when(
      q7 == 1 & q8 == 1 ~ 1,
      q7 == 1 & q8 == 2 ~ 2,
      q7 == 1 & q8 == 3 ~ 4,
      q7 != 1 ~ 3
    ),
    act_vaccine = case_when(
      q11 == 1 & q12 == 1 ~ 1,
      q11 == 1 & q12 == 2 ~ 2,
      q11 == 1 & q12 == 3 ~ 5,
      q11 == 2 ~ 4,
      q11 >= 3 ~ 3
    ),

    # dummy of before March
    b3_test = if_else(act_test == 4, 1, 0),
    b3_vaccine = if_else(act_vaccine == 5, 1, 0),

    # dummy of between March and Wave 1
    mw1_test = if_else(act_test == 2, 1, 0),
    mw1_vaccine = if_else(act_vaccine == 2, 1, 0),
    mw1_testvaccine = mw1_test * mw1_vaccine,
    mw1_testhave = mw1_test * I(act_vaccine == 4),

    # dummy of before wave 1
    bw1_test = if_else(act_test == 2 | act_test == 4, 1, 0),
    bw1_vaccine = if_else(act_vaccine == 2 | act_vaccine == 5, 1, 0),

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
#' Step 5: 共変量の作成
#'
#+
fbweb <- fbweb %>%
  mutate(
    # age and treatment of coupon in FY2019
    birth_year = q3_1t + 25 + 1900,
    birth_month = q3_2t,
    birth_date = as.Date(sprintf("%4d/%02d/02", birth_year, birth_month)),
    base_date = as.Date("2019/04/01"),
    age = trunc(time_length(birth_date %--% base_date, "year")),
    mage = trunc(time_length(birth_date %--% base_date, "month")),
    coupon2019 = if_else(40 <= age & age <= 46, 1, 0),

    # quarter of birth manth
    birth_q = case_when(
      q3_2t %in% c(1, 2, 3) ~ 1,
      q3_2t %in% c(4, 5, 6) ~ 2,
      q3_2t %in% c(7, 8, 9) ~ 3,
      q3_2t %in% c(10, 11, 12) ~ 4
    ),

    # running variable of age
    run_age = 46 - age,
    run_mage = 563 - mage,

    # coupon knowledge
    coupon = q15a,
    know_coupon = if_else(q15a != 3, 1, 0),
    have_coupon = if_else(q15a == 1, 1, 0),

    # education and married dummy
    education = case_when(
      q21 %in% c(1, 2) ~ 9,
      q21 %in% c(3, 4, 6) ~ 12,
      q21 == 5 ~ 14,
      q21 %in% c(7, 8) ~ 16,
      q21 %in% c(9, 10) ~ 18,
      q21 == 11 ~ 21
    ),
    married = if_else(q22 == 1, 1, 0)
  ) %>%
  rename(
    # daily health behavior after wave 1
    handwash = q18_1,
    temp_check = q18_2,
    eat_habit = q18_3,
    avoid_out = q18_4,
    avoid_crowd = q18_5,
    wear_mask = q18_6,
    exercise_w2 = q18_7,
    avoid_hospital = q18_8
  )

#'
#' Step 6: FBRCTのトリートメント変数
#'
#+
fbweb <- fbweb %>%
  mutate(
    nudge = case_when(
      q3_2t %in% c(4, 8, 12) ~ "A",
      q3_2t %in% c(5, 9, 1) ~ "C",
      q3_2t %in% c(6, 10, 2) ~ "D",
      q3_2t %in% c(7, 11, 3) ~ "F"
    )
  )

#'
#' Step 7: 未使用変数をドロップ
#'
#+
fbweb <- fbweb %>% dplyr::select(-starts_with("q"))

#'
#' Step 8: 前処理後データの書き出し
#'
#+
write_csv(fbweb, file = here(pass, "shape_fb-survey.csv"))
