test_that("debt_built_schedule: bullet solde zéro et identités", {
  sch <- debt_built_schedule(
    principal = 6e6,
    rate_annual = 0.045,
    maturity = 5,
    type = "bullet", # ← au lieu de amort_years=0
    extra_amort_pct = 0,
    arrangement_fee_pct = 0
  )
  expect_equal(min(sch$year), 0)
  expect_equal(max(sch$year), 5)
  expect_equal(sum(sch$debt_draw) - sum(sch$amortization),
    tail(sch$outstanding_debt, 1),
    tolerance = 0.01
  )
  expect_equal(tail(sch$outstanding_debt, 1), 0, tolerance = 0.01)
  expect_equal(sch$payment[sch$year == 0], 0)
})
