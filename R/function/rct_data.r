rct_data_wave1 <- function(path) {
  read_csv(path) %>%
    dplyr::filter(40 <= age & age <= 56) %>%
    dplyr::filter(exp_antibody != 1 & exp_vaccine != 1) %>%
    mutate(
      aw1_negative = if_else(act_vaccine != 4, 1, 0),
      aw1_testnega = aw1_test * aw1_negative,
      coupon_a = coupon2019 * as.numeric(nudge == "A"),
      coupon_b = coupon2019 * as.numeric(nudge == "B"),
      coupon_c = coupon2019 * as.numeric(nudge == "C"),
      coupon_d = coupon2019 * as.numeric(nudge == "D"),
      coupon_e = coupon2019 * as.numeric(nudge == "E"),
      coupon_f = coupon2019 * as.numeric(nudge == "F"),
      coupon_g = coupon2019 * as.numeric(nudge == "G")
    )
}

rct_data_wave2 <- function(path, selection) {
  dt <- read_csv(path) %>%
    dplyr::filter(40 <= age & age <= 56) %>%
    dplyr::filter(follow == 1) %>%
    mutate(
      coupon_a = coupon2019 * as.numeric(nudge == "A"),
      coupon_b = coupon2019 * as.numeric(nudge == "B"),
      coupon_c = coupon2019 * as.numeric(nudge == "C"),
      coupon_d = coupon2019 * as.numeric(nudge == "D"),
      coupon_e = coupon2019 * as.numeric(nudge == "E"),
      coupon_f = coupon2019 * as.numeric(nudge == "F"),
      coupon_g = coupon2019 * as.numeric(nudge == "G")
    )

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