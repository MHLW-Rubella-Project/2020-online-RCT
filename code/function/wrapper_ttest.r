ttest_loop <- function(y, data, base = "Control") {
  levels(data$nudge) %>%
    purrr::map(function(x) {
      vec1 <- subset(data, nudge == base)[y]
      vec2 <- subset(data, nudge == x)[y]
      t.test(vec2, vec1) %>%
        broom::tidy() %>%
        dplyr::select(mean = estimate1, effect = estimate, p.value) %>%
        mutate(se = se(unlist(vec2)), nudge = x, outcome = y) %>%
        mutate(label = case_when(
          nudge == base ~ sprintf("%1.3f", mean),
          p.value < .01 ~ sprintf("%1.3f (%1.3f***)", mean, effect),
          p.value < .05 ~ sprintf("%1.3f (%1.3f**)", mean, effect),
          p.value < .1 ~ sprintf("%1.3f (%1.3f*)", mean, effect),
          TRUE ~ sprintf("%1.3f (%1.3f)", mean, effect)
        ))
    }) %>%
    purrr::reduce(bind_rows) %>%
    mutate(nudge = factor(nudge, levels = levels(data$nudge)))
}