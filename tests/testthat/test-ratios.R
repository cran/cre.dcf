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
