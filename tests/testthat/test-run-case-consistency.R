test_that("run_case respects normalized debt type when not overridden", {
  cfg <- dcf_spec_template()
  cfg$extra_amort_pct <- 0.02
  cfg$leases <- list(
    list(
      unit = "U",
      area = 1000,
      events = list(
        list(
          start = cfg$purchase_year,
          end = cfg$purchase_year + cfg$horizon_years,
          rent = 200,
          free_months = 0,
          capex_sqm = 0,
          vac = 0,
          new_lease = 0
        )
      )
    )
  )

  run_case_res <- run_case(config = cfg)
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, tmp)
  run_from_cfg_res <- run_from_config(tmp, ltv_base = "price_di")

  expect_equal(cfg_normalize(cfg)$type, "amort")
  expect_equal(run_case_res$config$debt_type, "amort")
  expect_equal(run_case_res$cashflows$payment, run_from_cfg_res$full$payment)
})

test_that("run_case comparison matches leveraged metrics with cash fees", {
  cfg <- dcf_spec_template()
  cfg$arrangement_fee_pct <- 0.02
  cfg$capitalized_fees <- FALSE
  cfg$leases <- list(
    list(
      unit = "U",
      area = 1000,
      events = list(
        list(
          start = cfg$purchase_year,
          end = cfg$purchase_year + cfg$horizon_years,
          rent = 200,
          free_months = 0,
          capex_sqm = 0,
          vac = 0,
          new_lease = 0
        )
      )
    )
  )

  out <- run_case(config = cfg, debt_type = "bullet")
  row_bullet <- out$comparison$summary[out$comparison$summary$scenario == "debt_bullet", , drop = FALSE]

  expect_equal(out$leveraged$irr_equity, row_bullet$irr_equity, tolerance = 1e-10)
  expect_equal(out$leveraged$npv_equity, row_bullet$npv_equity, tolerance = 1e-8)
})

test_that("run_case comparison matches leveraged metrics with capitalized fees", {
  cfg <- dcf_spec_template()
  cfg$arrangement_fee_pct <- 0.02
  cfg$capitalized_fees <- TRUE
  cfg$leases <- list(
    list(
      unit = "U",
      area = 1000,
      events = list(
        list(
          start = cfg$purchase_year,
          end = cfg$purchase_year + cfg$horizon_years,
          rent = 200,
          free_months = 0,
          capex_sqm = 0,
          vac = 0,
          new_lease = 0
        )
      )
    )
  )

  out <- run_case(config = cfg, debt_type = "bullet")
  row_bullet <- out$comparison$summary[out$comparison$summary$scenario == "debt_bullet", , drop = FALSE]

  expect_equal(out$leveraged$irr_equity, row_bullet$irr_equity, tolerance = 1e-10)
  expect_equal(out$leveraged$npv_equity, row_bullet$npv_equity, tolerance = 1e-8)
})

test_that("run_from_config shares the v3 pipeline for top-down cases", {
  cfg <- dcf_spec_template()
  cfg$top_down_noi <- TRUE
  cfg$acq_price_ht <- 1e7
  cfg$vacancy_rate <- 0.08
  cfg$simple_capex <- c(0, 15000, rep(0, cfg$horizon_years - 2))

  run_case_res <- run_case(config = cfg, ltv_base = "price_di")
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, tmp)
  run_from_cfg_res <- run_from_config(tmp, ltv_base = "price_di")

  expect_equal(run_case_res$all_equity$irr_project, run_from_cfg_res$dcf$irr_project, tolerance = 1e-10)
  expect_equal(run_case_res$cashflows$free_cash_flow, run_from_cfg_res$ratios$free_cash_flow)
  expect_equal(run_case_res$cashflows$sale_proceeds, run_from_cfg_res$ratios$sale_proceeds)
})
