test_that("dcf_calculate: erreurs de paramétrage détectées", {
  expect_error(dcf_calculate(
    acq_price = 1e7, entry_yield = 0.05,
    exit_yield = NULL, horizon_years = 10, disc_rate = 0.07
  ))
  expect_error(dcf_calculate(
    acq_price = -1, entry_yield = 0.05,
    exit_yield = 0.05, horizon_years = 10, disc_rate = 0.07
  ))
  expect_error(dcf_calculate(
    acq_price = 1e7, entry_yield = 1.2,
    exit_yield = 0.05, horizon_years = 10, disc_rate = 0.07
  ))
})

test_that("dcf_calculate: structure et conventions temporelles", {
  out <- dcf_calculate(
    acq_price = 1e7, entry_yield = 0.05, exit_yield = 0.055,
    horizon_years = 10, disc_rate = 0.07, exit_cost = 0,
    capex = 0, index_rent = 0, vacancy = 0, opex = 0
  )
  cf <- out$cashflows
  expect_equal(min(cf$year), 0)
  expect_equal(max(cf$year), 10)
  # vente uniquement à N
  expect_equal(sum(cf$sale_proceeds[-nrow(cf)]), 0)
  expect_gt(cf$sale_proceeds[nrow(cf)], 0)
  # FCF t=0 = -prix d'acquisition
  expect_equal(cf$free_cash_flow[cf$year == 0], -1e7)
})

test_that("dcf_calculate: identités NPV/IRR cohérentes", {
  out <- dcf_calculate(
    acq_price     = 1e7,
    entry_yield   = 0.05,
    exit_yield    = 0.05,
    horizon_years = 1,
    disc_rate     = 0.05,
    exit_cost     = 0,
    capex         = 0,
    index_rent    = 0,
    vacancy       = 0,
    opex          = 0
  )

  cf <- out$cashflows

  # Cas jouet N = 1 : NPV analytique
  noi1 <- 0.05 * 1e7
  npv_expected <- -1e7 + (noi1 + (noi1 / 0.05)) / 1.05

  expect_equal(
    out$npv,
    npv_expected,
    tolerance = 1e-4
  )

  # Le free cash flow de la dernière année inclut déjà la vente nette.
  expect_equal(
    cf$free_cash_flow[nrow(cf)],
    noi1 + cf$sale_proceeds[nrow(cf)],
    tolerance = 1e-6
  )

  irr_expected <- irr_safe(cf$free_cash_flow)
  expect_true(is.finite(irr_expected))
  expect_equal(out$irr_project, irr_expected, tolerance = 1e-6)

  # Encadrement grossier pour éviter des valeurs aberrantes
  expect_gt(out$irr_project, 0)
  expect_lt(out$irr_project, 0.2)
})



test_that("dcf_calculate: recyclage des vecteurs longueur 1 en N", {
  out <- dcf_calculate(
    acq_price = 1e7, entry_yield = 0.05, exit_yield = 0.055,
    horizon_years = 5, disc_rate = 0.07,
    capex = 10000, index_rent = 0.02, vacancy = 0.05, opex = 200000
  )
  cf <- out$cashflows
  expect_equal(nrow(cf), 6) # 0..5
  # capex/opex présents à t>=1, nuls à t=0
  expect_equal(cf$capex[1], 0)
  expect_equal(cf$opex[1], 0)
})

test_that("dcf_calculate: gei, noi and pbtcf are explicit and coherent", {
  out <- dcf_calculate(
    acq_price = 1e7,
    entry_yield = 0.05,
    exit_yield = 0.055,
    horizon_years = 4,
    disc_rate = 0.07,
    capex = c(10000, 15000, 20000, 0),
    index_rent = 0,
    vacancy = 0,
    opex = c(50000, 50000, 50000, 50000)
  )

  cf <- out$cashflows
  idx <- cf$year >= 1

  expect_true(all(c("gei", "noi", "pbtcf") %in% names(cf)))
  expect_equal(cf$noi[idx], cf$gei[idx] - cf$opex[idx], tolerance = 1e-10)
  expect_equal(cf$pbtcf[idx], cf$noi[idx] - cf$capex[idx], tolerance = 1e-10)
  expect_equal(out$inputs$terminal_noi, tail(cf$noi, 1))
})

test_that("dcf_calculate anchors entry yield on NOI1 in top-down mode", {
  out <- dcf_calculate(
    acq_price = 1e7,
    entry_yield = 0.05,
    exit_yield = 0.055,
    horizon_years = 3,
    disc_rate = 0.07,
    capex = 0,
    index_rent = 0,
    vacancy = 0,
    opex = c(50000, 52000, 54000)
  )

  cf <- out$cashflows

  expect_equal(cf$noi[cf$year == 1], 5e5, tolerance = 1e-10)
  expect_equal(cf$gei[cf$year == 1], 5.5e5, tolerance = 1e-10)
})

test_that("dcf_calculate capitalizes a forward NOI in terminal value", {
  out <- dcf_calculate(
    acq_price = 1e7,
    entry_yield = 0.05,
    exit_yield = 0.05,
    horizon_years = 3,
    disc_rate = 0.07,
    capex = 0,
    index_rent = c(0.02, 0.02, 0.02),
    vacancy = 0,
    opex = 0
  )

  cf <- out$cashflows
  noi_n <- cf$noi[cf$year == 3]
  expected_forward_noi <- noi_n * 1.02

  expect_equal(out$inputs$terminal_noi_base, noi_n, tolerance = 1e-10)
  expect_equal(out$inputs$terminal_noi, expected_forward_noi, tolerance = 1e-10)
  expect_equal(cf$sale_proceeds[cf$year == 3], expected_forward_noi / 0.05, tolerance = 1e-6)
})

test_that("compute_unleveraged_metrics reports operations and terminal shares", {
  base <- dcf_calculate(
    acq_price = 1e7,
    entry_yield = 0.05,
    exit_yield = 0.05,
    horizon_years = 1,
    disc_rate = 0.07
  )
  high_exit_yield <- dcf_calculate(
    acq_price = 1e7,
    entry_yield = 0.05,
    exit_yield = 0.08,
    horizon_years = 1,
    disc_rate = 0.07
  )

  base_metrics <- compute_unleveraged_metrics(base)
  alt_metrics <- compute_unleveraged_metrics(high_exit_yield)

  expect_equal(base_metrics$ops_share + base_metrics$tv_share, 1, tolerance = 1e-10)
  expect_gt(base_metrics$tv_share, 0.8)
  expect_gt(alt_metrics$ops_share, base_metrics$ops_share)
})

test_that("DCF: colonnes 0..N alignées", {
  out <- dcf_calculate(1e7, 0.05, 0.055, 10, 0.07)
  cf <- out$cashflows
  expect_equal(nrow(cf), 11L)
  expect_equal(length(cf$free_cash_flow), 11L)
  expect_equal(sum(cf$sale_proceeds[1:10]), 0)
  expect_gt(cf$sale_proceeds[11], 0)
})
