test_that("add_credit_ratios: identités DSCR, debt yield et LTV forward", {
  dcf <- dcf_calculate(1e7, 0.05, 0.055, 5, 0.07)
  sch <- debt_built_schedule(6e6, 0.045, 5, type = "bullet")
  full <- cf_make_full_table(dcf, sch)
  rat  <- add_credit_ratios(full, sch, exit_yield = 0.055)

  # 1. conventions de NA au temps 0 
  t0 <- rat$year == 0

  expect_true(all(is.na(rat$dscr[t0])))
  expect_true(all(is.na(rat$interest_cover_ratio[t0])))
  expect_true(all(is.na(rat$debt_yield_current[t0])))

  # 2. identités DSCR et debt_yield_current là où définies 
  idx_dscr <- with(rat, year > 0 & payment > 0)
  idx_dyc  <- with(rat, year > 0 & outstanding_debt > 0)

  # Numerateur : basis = "noi" par défaut
  expect_equal(
    rat$dscr[idx_dscr],
    rat$noi[idx_dscr] / rat$payment[idx_dscr],
    tolerance = 1e-10
  )

  expect_equal(
    rat$debt_yield_current[idx_dyc],
    rat$noi[idx_dyc] / rat$outstanding_debt[idx_dyc],
    tolerance = 1e-10
  )

  # En dehors de ces zones, les ratios doivent être NA
  expect_true(all(is.na(rat$dscr[!idx_dscr])))
  expect_true(all(is.na(rat$debt_yield_current[!idx_dyc])))

  # 3. LTV forward 
  # Valeur forward théorique : NOI_{t+1} / exit_yield
  value_fwd_theo <- dplyr::lead(rat$noi) / 0.055

  # Dernière ligne : pas de NOI_{t+1} => LTV forward doit être NA
  n <- nrow(rat)
  expect_true(is.na(rat$ltv_forward[n]))
  expect_true(is.na(rat$value_forward[n]))

  # Sur les périodes où value_forward est définie et la dette positive
  idx_ltv <- with(rat, year < max(year) & outstanding_debt > 0)

  expect_equal(
    rat$value_forward[idx_ltv],
    value_fwd_theo[idx_ltv],
    tolerance = 1e-10
  )

  expect_equal(
    rat$ltv_forward[idx_ltv],
    rat$outstanding_debt[idx_ltv] / value_fwd_theo[idx_ltv],
    tolerance = 1e-10
  )

  # Là où la dette est nulle ou la valeur forward manquante, LTV forward doit être NA
  expect_true(all(is.na(rat$ltv_forward[!idx_ltv])))
})

test_that("credit_guardrails_fast: équivalence avec add_credit_ratios", {
  dcf <- dcf_calculate(1e7, 0.052, 0.057, 7, 0.072)
  sch <- debt_built_schedule(6.4e6, 0.048, 6, type = "bullet")

  # Chemin moteur: table compacte (sans merge full)
  compact_cf <- as.data.frame(dcf$cashflows)
  compact_cf$gei <- compact_cf$net_operating_income
  compact_cf$noi <- compact_cf$net_operating_income
  compact_cf$loan_init <- NA_real_

  fast_fun <- getFromNamespace("credit_guardrails_fast", "cre.dcf")
  fast <- fast_fun(
    cf_tab = compact_cf,
    debt_sched = sch,
    exit_yield = 0.057,
    dscr_basis = "noi",
    ignore_balloon_in_min = TRUE,
    maturity_year = 6
  )

  ratios <- add_credit_ratios(
    cf_tab = compact_cf,
    debt_sched = sch,
    exit_yield = 0.057,
    dscr_basis = "noi",
    ignore_balloon_in_min = TRUE,
    maturity_year = 6
  )

  ref_min_dscr <- attr(ratios, "min_dscr_pre_maturity")
  ref_max_ltv <- suppressWarnings(max(ratios$ltv_forward[ratios$year >= 1 & ratios$year <= 6], na.rm = TRUE))
  if (!is.finite(ref_max_ltv)) ref_max_ltv <- NA_real_

  expect_equal(fast$min_dscr_pre_maturity, ref_min_dscr, tolerance = 1e-12)
  expect_equal(fast$max_ltv_forward, ref_max_ltv, tolerance = 1e-12)
})
