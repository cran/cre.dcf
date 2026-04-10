test_that("lease_effective_rent reproduces a constant annuity", {
  out <- lease_effective_rent(
    cashflows = rep(100, 5),
    discount_rate = 0.08,
    timing = "arrears",
    perspective = "landlord"
  )

  expect_equal(out$equivalent_annuity, 100, tolerance = 1e-10)
  expect_equal(out$effective_rent, 100, tolerance = 1e-10)
})

test_that("lease_effective_rent captures concessions and area normalization", {
  base <- lease_effective_rent(
    cashflows = rep(100, 5),
    discount_rate = 0.08,
    area = 10,
    timing = "arrears",
    perspective = "landlord"
  )
  concessive <- lease_effective_rent(
    cashflows = c(0, 100, 100, 100, 100),
    discount_rate = 0.08,
    area = 10,
    timing = "arrears",
    perspective = "landlord"
  )

  expect_lt(concessive$effective_rent, base$effective_rent)
  expect_equal(base$effective_rent_per_area, base$effective_rent / 10, tolerance = 1e-10)
})

test_that("lease_effective_rent stays symmetric across landlord and tenant perspectives", {
  landlord <- lease_effective_rent(
    cashflows = rep(100, 4),
    discount_rate = 0.07,
    timing = "arrears",
    perspective = "landlord"
  )
  tenant <- lease_effective_rent(
    cashflows = rep(-100, 4),
    discount_rate = 0.07,
    timing = "arrears",
    perspective = "tenant"
  )

  expect_equal(abs(landlord$effective_rent), abs(tenant$effective_rent), tolerance = 1e-10)
  expect_equal(landlord$effective_rent, -tenant$effective_rent, tolerance = 1e-10)
})

test_that("underwrite_loan identifies an LTV binding constraint", {
  uw <- underwrite_loan(
    noi = 1e6,
    value = 5e6,
    rate_annual = 0.05,
    maturity = 5,
    type = "bullet",
    dscr_min = 1.25,
    ltv_max = 0.50,
    debt_yield_min = 0.08
  )

  expect_equal(uw$binding_constraint, "ltv")
  expect_equal(uw$max_loan, uw$max_loan_ltv, tolerance = 1e-10)
  expect_equal(uw$implied_ltv, 0.5, tolerance = 1e-10)
  expect_equal(
    uw$payment_year1,
    debt_built_schedule(uw$max_loan, 0.05, 5, type = "bullet")$payment[2],
    tolerance = 1e-10
  )
})

test_that("underwrite_loan identifies a DSCR binding constraint", {
  uw <- underwrite_loan(
    noi = 5e5,
    value = 20e6,
    rate_annual = 0.06,
    maturity = 5,
    type = "amort",
    dscr_min = 1.25,
    ltv_max = 0.80,
    debt_yield_min = 0.03
  )

  expect_equal(uw$binding_constraint, "dscr")
  expect_equal(uw$max_loan, uw$max_loan_dscr, tolerance = 1e-10)
  expect_gte(uw$implied_dscr, 1.25 - 1e-6)
  expect_equal(
    uw$payment_year1,
    debt_built_schedule(uw$max_loan, 0.06, 5, type = "amort")$payment[2],
    tolerance = 1e-10
  )
})

test_that("underwrite_loan identifies a debt-yield binding constraint", {
  uw <- underwrite_loan(
    noi = 5e5,
    value = 20e6,
    rate_annual = 0.03,
    maturity = 5,
    type = "bullet",
    dscr_min = 1.25,
    ltv_max = 0.90,
    debt_yield_min = 0.08
  )

  expect_equal(uw$binding_constraint, "debt_yield")
  expect_equal(uw$max_loan, uw$max_loan_debt_yield, tolerance = 1e-10)
  expect_equal(uw$implied_debt_yield, 0.08, tolerance = 1e-8)
})

test_that("compare_financing_scenarios surfaces operations and terminal shares", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  cmp <- compare_financing_scenarios(
    dcf_res = dcf,
    acq_price = 1e7,
    ltv = 0.60,
    rate = 0.045,
    maturity = 5
  )

  expect_true(all(c("ops_share", "tv_share") %in% names(cmp$summary)))
  expect_true(all(abs(cmp$summary$ops_share + cmp$summary$tv_share - 1) < 1e-10))
})

test_that("tax_run_spv computes depreciation, exit gain, taxes, and after-tax equity flows", {
  basis <- tibble::tibble(
    year = 0:4,
    noi = c(0, 140, 150, 160, 170),
    capex = c(0, 0, 20, 0, 0),
    interest = c(0, 30, 25, 20, 0),
    sale_proceeds = c(0, 0, 0, 0, 900),
    pre_tax_equity_cf = c(-1000, 110, 105, 120, 950)
  )

  spec <- tax_spec_spv(
    corp_tax_rate = 0.25,
    depreciation_spec = depreciation_spec(
      acquisition_split = tibble::tribble(
        ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
        "land",      0.20,        NA,    "none",           FALSE,
        "building",  0.80,         4,    "straight_line",  TRUE
      ),
      capex_bucket = "building",
      start_rule = "full_year"
    )
  )

  out <- tax_run_spv(basis, spec, acquisition_price = 1000)
  tbl <- out$tax_table

  expect_equal(tbl$tax_depreciation, c(0, 200, 205, 205, 205), tolerance = 1e-10)
  expect_equal(tbl$book_value_close_pre_exit[5], 205, tolerance = 1e-10)
  expect_equal(tbl$taxable_exit_gain_loss[5], 695, tolerance = 1e-10)
  expect_equal(tbl$loss_cf_open[5], 235, tolerance = 1e-10)
  expect_equal(tbl$loss_cf_used[5], 235, tolerance = 1e-10)
  expect_equal(tbl$taxable_income_post_losses[5], 425, tolerance = 1e-10)
  expect_equal(tbl$cash_is[5], 106.25, tolerance = 1e-10)
  expect_equal(tbl$after_tax_equity_cf[5], 843.75, tolerance = 1e-10)
  expect_equal(out$summary$total_cash_is, 106.25, tolerance = 1e-10)
})

test_that("tax_run_spv supports next-year depreciation and capped use of losses", {
  basis <- tibble::tibble(
    year = 0:3,
    noi = c(0, 40, 300, 500),
    capex = c(0, 0, 0, 0),
    interest = c(0, 0, 0, 0),
    sale_proceeds = c(0, 0, 0, 0)
  )

  spec <- tax_spec_spv(
    corp_tax_rate = 0.30,
    depreciation_spec = depreciation_spec(
      acquisition_split = tibble::tribble(
        ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
        "land",      0.20,        NA,    "none",           FALSE,
        "building",  0.80,         2,    "straight_line",  TRUE
      ),
      capex_bucket = "building",
      start_rule = "next_year"
    ),
    loss_rule = loss_rule(
      carryforward = TRUE,
      carryforward_years = Inf,
      offset_cap_pct = 0.50
    )
  )

  out <- tax_run_spv(basis, spec, acquisition_price = 1000)
  tbl <- out$tax_table

  expect_equal(tbl$tax_depreciation, c(0, 0, 400, 400), tolerance = 1e-10)
  expect_equal(tbl$loss_generated[2], 0, tolerance = 1e-10)
  expect_equal(tbl$loss_generated[3], 100, tolerance = 1e-10)
  expect_equal(tbl$loss_cf_open[4], 100, tolerance = 1e-10)
  expect_equal(tbl$loss_cf_used[4], 50, tolerance = 1e-10)
  expect_equal(tbl$taxable_income_post_losses[4], 50, tolerance = 1e-10)
  expect_equal(tbl$cash_is[4], 15, tolerance = 1e-10)
})

test_that("tax_basis_spv extracts a tax basis from a pre-tax case", {
  deal <- deal_spec(
    price = 10e6,
    entry_yield = 0.055,
    horizon_years = 5,
    debt = debt_terms(ltv = 0.5, rate = 0.04, type = "bullet")
  )

  res <- analyze_deal(deal)
  basis <- tax_basis_spv(res)

  expect_true(all(c(
    "year", "noi", "capex", "interest", "sale_proceeds",
    "pre_tax_equity_cf", "acquisition_price"
  ) %in% names(basis)))
  expect_equal(length(unique(basis$acquisition_price)), 1L)
  expect_equal(unique(basis$acquisition_price), res$pricing$price_ht, tolerance = 1e-8)
})
