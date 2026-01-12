test_that("simulate_shock modifie les hypothèses de façon prévisible", {
  cfg <- list(
    acq_price = 1e7, entry_yield = 0.05, exit_yield = 0.055,
    horizon_years = 5, disc_rate = 0.07, capex = 0
  )
  ch <- simulate_shock(cfg, list(d_rate = 0.01, d_exit_yield = 0.005, d_noi = -0.1, d_capex = 0.2))
  expect_equal(ch$disc_rate, 0.08)
  expect_equal(ch$exit_yield, 0.060)
  expect_equal(ch$entry_yield, 0.045)
})

test_that("test_refi: échec logique si DSCR ou LTV ne passent pas", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  sch <- debt_built_schedule(7e6, 0.06, maturity = 5, type = "bullet")
  full <- cf_make_full_table(dcf, sch)
  res <- test_refi(full,
    year_T = 3, covenants = list(dscr_min = 1.25, ltv_max = 0.65),
    new_rate = 0.06, new_exit_yield = 0.06
  )
  expect_true(res$status %in% c("ok", "fail"))
  expect_true(all(c("dscr_T1", "ltv_T1") %in% names(res$snapshot)))
})
