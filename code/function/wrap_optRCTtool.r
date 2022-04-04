wrap_optRCTtool <- function(selection) {
  if (selection == 1) {
    set_optRCTtool(
      basicmod = test_int + vaccine_int + aw1_test + aw1_testvaccine ~ nudge,
      xmod = ~ age + coupon2019 + married + education +
        exercise_w1 + health_check + flushot +
        prob_social + handicap + severity +
        handwash + temp_check + avoid_out + avoid_crowd + wear_mask,
      data = web,
      ctrl = "厚労省",
      RCTtool.plot_family = "YuGothic",
      RCTtool.table_fontsize = 9
    )
  } else if (selection == 2) {
    set_optRCTtool(
      basicmod = abw1_test + abw1_testvaccine ~ nudge,
      data = web,
      ctrl = "厚労省",
      RCTtool.plot_family = "YuGothic",
      RCTtool.table_fontsize = 9
    )
  }
}