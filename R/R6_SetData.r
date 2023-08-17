library(here)
library(R6)
library(tidyverse)

SetData <- R6::R6Class("SetData",
  public = list(
    data = NULL,
    initialize = function(path) self$data <- read_csv(path)
  )
)