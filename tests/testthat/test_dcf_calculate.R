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

  # IRR calculé sur les flux totaux (free cash flow + sale_proceeds)
  total_cf   <- cf$free_cash_flow + cf$sale_proceeds
  irr_factor <- irr_safe(total_cf)  # contrat actuel : renvoie (1 + r)

  expect_true(is.finite(irr_factor))

  # Cohérence numérique entre irr_safe() et irr_project (r)
  expect_equal(
    irr_factor - 1,
    out$irr_project,
    tolerance = 1e-6  # tolérance adaptée à un solveur numérique
  )

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

test_that("DCF: colonnes 0..N alignées", {
  out <- dcf_calculate(1e7, 0.05, 0.055, 10, 0.07)
  cf <- out$cashflows
  expect_equal(nrow(cf), 11L)
  expect_equal(length(cf$free_cash_flow), 11L)
  expect_equal(sum(cf$sale_proceeds[1:10]), 0)
  expect_gt(cf$sale_proceeds[11], 0)
})
