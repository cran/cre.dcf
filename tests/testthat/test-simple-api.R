test_that("debt_terms validates inputs and returns a typed object", {
  debt <- debt_terms()

  expect_s3_class(debt, "cre_debt_terms")
  expect_equal(debt$type, "bullet")
  expect_error(debt_terms(type = "foo"))
  expect_error(debt_terms(ltv = 1.2))
  expect_error(debt_terms(maturity = 0))
})

test_that("lease helpers build typed lease objects", {
  ev1 <- lease_event(start = 2025, end = 2026, rent = 200, vac = 0.05)
  ev2 <- lease_event(start = 2027, end = 2028, rent = 215, vac = 0, new_lease = TRUE, free_months = 3)
  unit <- lease_unit("A1", area_sqm = 1500, events = list(ev1, ev2))
  roll <- lease_roll(list(unit))

  expect_s3_class(ev1, "cre_lease_event")
  expect_s3_class(unit, "cre_lease_unit")
  expect_s3_class(roll, "cre_lease_roll")
  expect_equal(unit$unit, "A1")
  expect_length(roll$units, 1)

  expect_error(lease_event(start = 2026, end = 2025, rent = 200))
  expect_error(lease_unit("A1", area_sqm = 1500, events = list(list(start = 2025))))
  expect_error(lease_roll(list(list(unit = "raw"))))
})

test_that("analyst-oriented lease helpers and prints are informative", {
  vac <- vacancy_event(start = 2027, end = 2027, capex_sqm = 35)
  ren <- renewal_event(start = 2028, end = 2031, rent = 240, free_months = 3, capex_sqm = 20)
  unit <- lease_unit(
    "North",
    area_sqm = 1200,
    events = list(
      lease_event(start = 2025, end = 2026, rent = 220, vac = 0.05),
      vac,
      ren
    )
  )
  roll <- lease_roll(list(unit))

  expect_equal(vac$rent, 0)
  expect_equal(vac$vac, 1)
  expect_equal(vac$new_lease, 0)
  expect_equal(ren$new_lease, 1)

  snap_unit <- lease_roll_snapshot(unit)
  snap_roll <- lease_roll_snapshot(roll)
  expect_s3_class(snap_unit, "tbl_df")
  expect_s3_class(snap_roll, "tbl_df")
  expect_s3_class(summary(unit), "tbl_df")
  expect_s3_class(summary(roll), "tbl_df")
  expect_equal(snap_unit$event_count, 3)
  expect_equal(snap_unit$free_months_total, 3)
  expect_equal(snap_unit$capex_sqm_total, 55)
  expect_equal(snap_unit$reletting_count, 1)

  expect_output(print(vac), "cre_lease_event")
  expect_output(print(unit), "cre_lease_unit")
  expect_output(print(roll), "cre_lease_roll")
  expect_output(print(roll), "North")
})

test_that("deal_spec accepts exactly one income mode", {
  expect_s3_class(
    deal_spec(price = 10e6, entry_yield = 0.055),
    "cre_deal_spec"
  )
  expect_s3_class(
    deal_spec(price = 10e6, noi_y1 = 550000),
    "cre_deal_spec"
  )
  expect_s3_class(
    deal_spec(price = 10e6, rent_sqm = 220, area_sqm = 3000),
    "cre_deal_spec"
  )
  expect_s3_class(
    deal_spec(
      price = 10e6,
      lease_roll = lease_roll(list(
        lease_unit(
          "A1",
          area_sqm = 3000,
          events = list(lease_event(start = 2025, end = 2034, rent = 220, vac = 0.05))
        )
      )),
      purchase_year = 2025
    ),
    "cre_deal_spec"
  )

  expect_error(deal_spec(price = 10e6))
  expect_error(deal_spec(price = 10e6, entry_yield = 0.055, noi_y1 = 550000))
  expect_error(deal_spec(price = 10e6, rent_sqm = 220))
  expect_error(deal_spec(
    price = 10e6,
    rent_sqm = 220,
    area_sqm = 3000,
    lease_roll = lease_roll(list(
      lease_unit("A1", area_sqm = 3000, events = list(lease_event(start = 2025, end = 2034, rent = 220)))
    ))
  ))
})

test_that("deal_to_config produces a valid engine config", {
  deal_entry <- deal_spec(price = 10e6, entry_yield = 0.055, capex = c(0, 0, 1000, rep(0, 7)))
  cfg_entry <- deal_to_config(deal_entry)
  expect_no_error(cfg_validate(cfg_entry))
  expect_true(isTRUE(cfg_entry$top_down_noi))
  expect_equal(cfg_entry$disc_method, "risk_premium")
  expect_equal(cfg_entry$disc_rate_risk_premium$rf, deal_entry$discount_rate)

  deal_noi <- deal_spec(price = 10e6, noi_y1 = 550000)
  cfg_noi <- deal_to_config(deal_noi)
  expect_no_error(cfg_validate(cfg_noi))
  expect_false(isTRUE(cfg_noi$top_down_noi))
  expect_equal(cfg_noi$entry_yield, 550000 / 10e6)
  expect_length(cfg_noi$leases, 1)
})

test_that("analyze_deal reproduces the engine on entry-yield mode", {
  deal <- deal_spec(
    price = 10e6,
    entry_yield = 0.055,
    horizon_years = 7,
    discount_rate = 0.08,
    debt = debt_terms(ltv = 0.6, rate = 0.045, type = "bullet")
  )

  res_simple <- analyze_deal(deal)
  cfg <- deal_to_config(deal)
  res_engine <- run_case(config = cfg, ltv_base = "price_di")

  expect_equal(res_simple$all_equity$irr_project, res_engine$all_equity$irr_project, tolerance = 1e-10)
  expect_equal(res_simple$leveraged$irr_equity, res_engine$leveraged$irr_equity, tolerance = 1e-10)
  expect_equal(res_simple$cashflows$free_cash_flow, res_engine$cashflows$free_cash_flow)
})

test_that("analyze_deal supports direct NOI and amortising debt", {
  deal <- deal_spec(
    price = 9e6,
    noi_y1 = 540000,
    horizon_years = 6,
    discount_rate = 0.075,
    debt = debt_terms(ltv = 0.55, rate = 0.043, type = "amort", maturity = 6)
  )

  res_simple <- analyze_deal(deal)
  cfg <- deal_to_config(deal)
  res_engine <- run_case(config = cfg, ltv_base = "price_di")

  expect_equal(res_simple$config$debt_type, "amort")
  expect_equal(res_simple$all_equity$irr_project, res_engine$all_equity$irr_project, tolerance = 1e-10)
  expect_equal(res_simple$leveraged$irr_equity, res_engine$leveraged$irr_equity, tolerance = 1e-10)
})

test_that("analyze_deal supports a lease roll without YAML authoring", {
  roll <- lease_roll(list(
    lease_unit(
      "North",
      area_sqm = 1200,
      events = list(
        lease_event(start = 2025, end = 2026, rent = 220, vac = 0.08),
        lease_event(start = 2027, end = 2029, rent = 235, vac = 0, new_lease = TRUE, free_months = 3, capex_sqm = 24)
      )
    ),
    lease_unit(
      "South",
      area_sqm = 800,
      events = list(
        lease_event(start = 2025, end = 2029, rent = 205, vac = 0.03)
      )
    )
  ))

  deal <- deal_spec(
    price = 7.8e6,
    lease_roll = roll,
    purchase_year = 2025,
    horizon_years = 4,
    opex_sqm = 14,
    debt = debt_terms(ltv = 0.55, rate = 0.043, type = "bullet")
  )

  res_simple <- analyze_deal(deal)
  cfg <- deal_to_config(deal)
  res_engine <- run_case(config = cfg, ltv_base = "price_di")

  expect_equal(deal$income_mode, "lease_roll")
  expect_equal(length(cfg$leases), 2)
  expect_equal(res_simple$all_equity$irr_project, res_engine$all_equity$irr_project, tolerance = 1e-10)
  expect_equal(res_simple$leveraged$irr_equity, res_engine$leveraged$irr_equity, tolerance = 1e-10)

  snap <- asset_snapshot(res_simple)
  expect_equal(snap$area_sqm, 2000)
  expect_true(is.finite(snap$rent_sqm))
  expect_true(is.finite(snap$noi_y1))
  expect_true(all(c("year", "gei", "noi", "pbtcf", "opex", "capex") %in% names(deal_cashflows(res_simple, "operating"))))
})

test_that("print, summary and deal_cashflows work on simplified results", {
  deal <- deal_spec(
    price = 8e6,
    rent_sqm = 250,
    area_sqm = 2500,
    vacancy_rate = 0.05,
    opex_sqm = 12
  )

  expect_output(print(deal), "cre_deal_spec")
  expect_output(print(deal), "asset:")
  expect_output(print(deal), "GEI")

  res <- analyze_deal(deal)
  expect_output(print(res), "cre_deal_result")

  snap_deal <- asset_snapshot(deal)
  expect_s3_class(snap_deal, "tbl_df")
  expect_equal(snap_deal$price_per_sqm, 8e6 / 2500)
  expect_equal(snap_deal$gross_potential_rent_y1, 250 * 2500)
  expect_equal(snap_deal$gei_y1, 250 * 2500 * (1 - 0.05))

  snap_res <- asset_snapshot(res)
  year1 <- res$cashflows[res$cashflows$year == 1L, , drop = FALSE]
  expect_s3_class(snap_res, "tbl_df")
  expect_equal(snap_res$area_sqm, 2500)
  expect_equal(snap_res$gei_y1, year1$gei[[1]])
  expect_equal(snap_res$noi_y1, year1$noi[[1]])
  expect_equal(snap_res$pbtcf_y1, year1$pbtcf[[1]])

  smry <- summary(res)
  expect_s3_class(smry, "tbl_df")
  expect_true(all(c(
    "price", "area_sqm", "price_per_sqm", "rent_sqm", "vacancy_rate",
    "gross_potential_rent_y1", "gei_y1", "noi_y1", "pbtcf_y1",
    "horizon_years", "debt_type", "debt_ltv", "debt_rate",
    "irr_project", "irr_equity", "dscr_min", "ltv_max_forward", "ops_share", "tv_share"
  ) %in% names(smry)))

  expect_s3_class(deal_cashflows(res, "full"), "data.frame")
  expect_s3_class(deal_cashflows(res, "operating"), "data.frame")
  expect_s3_class(deal_cashflows(res, "all_equity"), "data.frame")
  expect_s3_class(deal_cashflows(res, "leveraged"), "data.frame")
  expect_s3_class(deal_cashflows(res, "comparison"), "data.frame")
  expect_true(all(c("year", "gei", "noi", "pbtcf", "opex", "capex") %in% names(deal_cashflows(res, "operating"))))
})
