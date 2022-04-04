rct_data <- function() {
  read_csv("./data/survey.csv") %>%
    mutate(nudge = factor(nudge, labels = c(
      "厚労省", "年齢表現", "利他強調", "利己強調",
      "社会比較", "有効期限", "低コスト"
    )))
}

rct_data_wave1 <- function() {
  rct_data() %>%
    dplyr::filter(40 <= age & age <= 56) %>%
    dplyr::filter(exp_antibody != 1 & exp_vaccine != 1) %>%
    mutate(
      aw1_negative = if_else(act_vaccine != 4, 1, 0),
      aw1_testnega = aw1_test * aw1_negative
    )
}

rct_data_wave2 <- function(selection) {
  dt <- rct_data() %>%
    dplyr::filter(40 <= age & age <= 56) %>%
    dplyr::filter(follow == 1)

  if (selection == 1) {
    dt %>%
      dplyr::filter(
        exp_antibody != 1 &
        exp_vaccine != 1 &
        act_test != 2 &
        act_vaccine != 2
      )  %>%
      mutate(
        aw1_negative = if_else(act_vaccine != 4, 1, 0),
        aw1_testnega = aw1_test * aw1_negative
      )
  } else if (selection == 2) {
    dt %>%
      dplyr::filter(exp_antibody != 1 & exp_vaccine != 1) %>%
      mutate(
        aw1_negative = if_else(act_vaccine != 4, 1, 0),
        abw1_testnega = abw1_test * aw1_negative
      )
  }
}