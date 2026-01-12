test_that("cf_make_full_table: alignement 0..N et flux equity cohérents", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  sch <- debt_built_schedule(6e6, 0.045, maturity = 5, type = "bullet")
  full <- cf_make_full_table(dcf, sch)
  expect_equal(nrow(full), 6) # 0..5
  expect_equal(full$payment[full$year == 0], 0)
  expect_gt(full$debt_draw[full$year == 0], 0)
  expect_true(all(c("cf_post_debt", "df", "equity_flow") %in% names(full)))
})
