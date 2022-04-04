knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE,
  echo = FALSE,
  cache = FALSE,
  include = TRUE,
  fig.width = 10
)

options(
  knitr.kable.NA = " ",
  knitr.table.format = "html",
  modelsummary_stars_note = FALSE,
  modelsummary_format_numeric_latex = "plain"
)

library(systemfonts)
data.frame(unique(system_fonts()[, "family"]))
grDevices::windowsFonts(YuGothic = grDevices::windowsFont("Yu Gothic"))

format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (is.null(format)) format <- "kableExtra"

out <- "kableExtra"