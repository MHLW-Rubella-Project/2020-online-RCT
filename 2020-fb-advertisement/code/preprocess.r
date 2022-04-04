library(tidyverse)
library(foreign)
library(readstata13)
library(lubridate)
library(here)

pass <- "data/2020-fb-advertisement"

#'
#' Step 1: 生データの読み込み
#'
#+
fb <- read_csv(
  here(pass, "raw", "performance.csv"),
  locale = locale(encoding = "cp932")
)

#'
#' Step 2: IDと日付変数の処理
#'
#+
fb <- fb %>%
  mutate(
    id = 1:n(),
    date = as.Date(date, "%Y/%b/%e")
  )

#'
#' Step 3: トリートメント変数を作成
#'
#+
fb <- fb %>%
  mutate(
    nudge = recode(
      nudge,
      "FB：統制群" = "A",
      "FB：介入群 [ 1 ]" = "C",
      "FB：介入群 [ 2 ]" = "D",
      "FB：介入群 [ 3 ]" = "F"
    )
  )

#'
#' Step 4: アウトカム変数を作成
#'
#' - \#.Display: 広告表示回数
#' - Click Rate: FB広告のリンクをクリックした割合
#' - Conversion Rate: リンク先に遷移した人のうち、アンケートに回答した割合
#'
#+
fb <- fb %>%
  rename(display = imp) %>%
  mutate(
    review_rate = cl / display,
    conversion_rate = cv / cl
  )

#'
#' Step 5: 未使用変数をドロップ
#'
#+
fb <- fb %>%
  dplyr::select(-cl, -cost, -cv)

#'
#' Step 6: csvファイルに書き出し
#'
#+
write_csv(fb, file = here(pass, "shape_performance.csv"))
