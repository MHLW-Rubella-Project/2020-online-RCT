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
  Sys.glob(file.path("script/function", "*.r")),
  source, encoding = "UTF-8"
)
