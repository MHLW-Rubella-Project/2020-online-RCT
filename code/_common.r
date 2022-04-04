# library
# devtools::install_github("KatoPachi/RCTtoolbox")
library(tidyverse)
library(rlang)
library(rlist)
library(lubridate)
library(patchwork)
library(estimatr)
library(modelsummary)
library(kableExtra)
library(flextable)
library(officer)
library(RCTtoolbox)

lapply(
  Sys.glob(here("code/function", "*.r")),
  source, encoding = "UTF-8"
)

# relative path of data
niid_path <- "data/2018-NIID-survey"
rct_path <- "data/2020-online-survey"

# treatment labels
treat_labels <- c(
  "厚労省", "年齢表現", "利他強調", "利己強調",
  "社会比較", "有効期限", "低コスト"
)