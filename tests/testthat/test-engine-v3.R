test_that("engine v3 projects lease operations with vacancy, free rent and leasing fees", {
  cfg <- dcf_spec_template()
  cfg$purchase_year <- 2025L
  cfg$horizon_years <- 2L
  cfg$index_rate <- 0
  cfg$opex_inflation_rate <- 0
  cfg$capex_inflation_rate <- 0
  cfg$opex_sqm <- 10
  cfg$landlord_base_opex_sqm <- 5
  cfg$leasing_cost_pct <- 0.10
  cfg$leases <- list(
    list(
      unit = "U1",
      area = 100,
      events = list(
        list(
          start = 2025L,
          end = 2025L,
          rent = 100,
          vac = 0.20,
          free_months = 0,
          capex_sqm = 10,
          new_lease = 0
        ),
        list(
          start = 2026L,
          end = 2026L,
          rent = 120,
          vac = 0,
          free_months = 6,
          capex_sqm = 20,
          new_lease = 1
        )
      )
    )
  )

  ops <- .engine_project_operations(cfg)

  expect_equal(ops$mode, "lease_events")
  expect_equal(ops$noi_price_base, 10000, tolerance = 1e-10)
  expect_equal(ops$noi_vec, c(8000, 10800), tolerance = 1e-10)
  expect_equal(ops$opex_vec, c(700, 500), tolerance = 1e-10)
  expect_equal(ops$capex_vec, c(1000, 2240), tolerance = 1e-10)
})

test_that("engine v3 applies indexation and inflation after base operations are built", {
  cfg <- dcf_spec_template()
  cfg$purchase_year <- 2025L
  cfg$horizon_years <- 3L
  cfg$index_rate <- 0.02
  cfg$opex_inflation_rate <- 0.03
  cfg$capex_inflation_rate <- 0.04
  cfg$opex_sqm <- 0
  cfg$landlord_base_opex_sqm <- 5
  cfg$leases <- list(
    list(
      unit = "U1",
      area = 100,
      events = list(
        list(
          start = 2025L,
          end = 2027L,
          rent = 100,
          vac = 0,
          free_months = 0,
          capex_sqm = 30,
          new_lease = 0
        )
      )
    )
  )

  ops <- .engine_project_operations(cfg)

  expect_equal(ops$noi_vec, c(10000, 10200, 10404), tolerance = 1e-10)
  expect_equal(ops$opex_vec, c(500, 515, 530.45), tolerance = 1e-10)
  expect_equal(ops$capex_vec, c(1000, 1040, 1081.6), tolerance = 1e-10)
})

test_that("cfg_normalize stays a compatibility facade over the v3 internal pipeline", {
  cfg <- dcf_spec_template()
  cfg$purchase_year <- 2025L
  cfg$horizon_years <- 3L
  cfg$acq_price_ht <- 8e6
  cfg$leases <- list(
    list(
      unit = "U1",
      area = 1000,
      events = list(
        list(
          start = 2025L,
          end = 2027L,
          rent = 200,
          vac = 0.05,
          free_months = 0,
          capex_sqm = 5,
          new_lease = 0
        )
      )
    )
  )

  market_inputs <- .engine_normalize_capital_market_inputs(cfg)
  operations <- .engine_project_operations(cfg)
  pricing <- .engine_resolve_pricing(cfg, operations, market_inputs)
  norm_expected <- .engine_flatten_normalized_case(market_inputs, operations, pricing)
  norm_actual <- cfg_normalize(cfg)

  expect_equal(norm_actual, norm_expected, tolerance = 1e-10)
})

test_that("run_case and run_from_config stay aligned on the standard preset YAML", {
  cfg_path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
  testthat::skip_if(cfg_path == "" || !file.exists(cfg_path), "Config d'exemple introuvable")

  cfg <- dcf_read_config(cfg_path)
  run_case_res <- run_case(config = cfg, ltv_base = "price_di")
  run_from_cfg_res <- run_from_config(cfg_path, ltv_base = "price_di")

  expect_equal(run_case_res$all_equity$irr_project, run_from_cfg_res$dcf$irr_project, tolerance = 1e-10)
  expect_equal(run_case_res$cashflows$free_cash_flow, run_from_cfg_res$ratios$free_cash_flow)
  expect_equal(run_case_res$cashflows$sale_proceeds, run_from_cfg_res$ratios$sale_proceeds)
})

test_that("run_case and run_from_config remain numerically aligned on a lease-driven case", {
  cfg <- dcf_spec_template()
  cfg$purchase_year <- 2025L
  cfg$horizon_years <- 4L
  cfg$arrangement_fee_pct <- 0.01
  cfg$capitalized_fees <- FALSE
  cfg$leases <- list(
    list(
      unit = "U1",
      area = 1200,
      events = list(
        list(
          start = 2025L,
          end = 2028L,
          rent = 210,
          free_months = 0,
          capex_sqm = 0,
          vac = 0.03,
          new_lease = 0
        )
      )
    )
  )

  run_case_res <- run_case(config = cfg, ltv_base = "price_di")
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(cfg, tmp)
  run_from_cfg_res <- run_from_config(tmp, ltv_base = "price_di")

  expect_equal(run_case_res$all_equity$irr_project, run_from_cfg_res$dcf$irr_project, tolerance = 1e-10)
  expect_equal(run_case_res$cashflows$free_cash_flow, run_from_cfg_res$ratios$free_cash_flow)
  expect_equal(run_case_res$cashflows$equity_flow, run_from_cfg_res$ratios$equity_flow)
  expect_equal(run_case_res$cashflows$sale_proceeds, run_from_cfg_res$ratios$sale_proceeds)
})
