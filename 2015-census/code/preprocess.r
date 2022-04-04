library(tidyverse)
library(foreign)
library(readstata13)
library(lubridate)
library(here)

pass <- "data/2015-census"

# Step 1: 生データの読み込み
census <- read_csv(
  here(pass, "raw", "population.csv"),
  locale = locale(encoding = "cp932")
)

# Step 2: 年齢・誕生月・人口データの取り出し
census <- census %>%
    dplyr::select("年齢_2015", "出生の月_2015", "value") %>%
    mutate(
        age = as.numeric(str_extract(年齢_2015, "[0-9]{1,2}")),
        month = as.numeric(str_extract(出生の月_2015, "[0-9]{1,2}")),
        pop = value
    ) %>%
    dplyr::select(age, month, pop)


# Step 3: csvファイルとして書き出し
write_csv(census, file = here(pass, "shape_population.csv"))
