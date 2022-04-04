rct_data_wave1 <- function(path) {
  rct_data(path) %>%
    dplyr::filter(40 <= age & age <= 56) %>%
    dplyr::filter(exp_antibody != 1 & exp_vaccine != 1) %>%
    mutate(
      aw1_negative = if_else(act_vaccine != 4, 1, 0),
      aw1_testnega = aw1_test * aw1_negative
    )
}

rct_data_wave2 <- function(path, selection) {
  dt <- rct_data(path) %>%
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