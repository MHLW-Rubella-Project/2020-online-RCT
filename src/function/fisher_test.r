fisher_multi_treat <- function(mod, base, data = "Control", ...) {
  yv <- all.vars(mod)[1]
  xv <- all.vars(mod)[2]
  control <- base
  treat <- levels(data[[xv]])
  treat <- treat[!str_detect(treat, control)]

  fisher <- treat %>%
    purrr::map(function(d) {
      usedt <- data[data[[xv]] == control | data[[xv]] == d, ]
      usedt[, yv] <- factor(as.character(usedt[[yv]]), levels = c(0, 1))
      usedt[, xv] <- droplevels(usedt[, xv])

      tab <- ftable(mod, data = usedt) %>% as.matrix()

      if (nrow(tab) > 1 & ncol(tab) > 1) {
        fisher.test(x = tab, ...) %>% .$p.value
      } else {
        NA_real_
      }
    }) %>%
    as_vector()

  names(fisher) <- treat

  fisher
}