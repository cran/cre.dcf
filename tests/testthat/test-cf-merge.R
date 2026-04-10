test_that("cf_make_full_table: alignement 0..N et flux equity cohérents", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  sch <- debt_built_schedule(6e6, 0.045, maturity = 5, type = "bullet")
  full <- cf_make_full_table(dcf, sch)
  expect_equal(nrow(full), 6) # 0..5
  expect_equal(full$payment[full$year == 0], 0)
  expect_gt(full$debt_draw[full$year == 0], 0)
  expect_true(all(c("gei", "noi", "pbtcf", "cf_post_debt", "df", "equity_flow") %in% names(full)))
  expect_equal(full$pbtcf[full$year >= 1], full$noi[full$year >= 1] - full$capex[full$year >= 1], tolerance = 1e-10)
  expect_equal(full$equity_flow, full$cf_post_debt)
})

test_that("cf_compute_levered matches compute_leveraged_metrics on equity flows", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  sch <- debt_built_schedule(6e6, 0.045, maturity = 5, type = "bullet")

  lev_ref <- compute_leveraged_metrics(dcf, sch, equity_invest = 4e6)
  lev_alt <- cf_compute_levered(
    dcf_res = dcf,
    debt_sched = sch,
    cfg = list(ltv_init = 0.6, arrangement_fee_pct = 0, capitalized_fees = TRUE)
  )

  expect_equal(lev_alt$equity_cf, lev_ref$cashflows$equity_cf)
  expect_equal(lev_alt$metrics$irr_equity, lev_ref$irr_equity, tolerance = 1e-10)
  expect_equal(lev_alt$metrics$npv_equity, lev_ref$npv_equity, tolerance = 1e-10)
})
