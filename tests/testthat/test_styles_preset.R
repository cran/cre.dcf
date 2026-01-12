test_that("style manifest preserves canonical risk/leverage hierarchy", {
  tbl <- styles_manifest()

  # ensure styles are present
  expected <- c("core", "core_plus", "value_added", "opportunistic")
  testthat::expect_setequal(tbl$style, expected)

  # order rows to simplify comparisons
  tbl <- dplyr::arrange(
    tbl,
    factor(style, levels = expected)
  )

  irr_proj  <- tbl$irr_project
  irr_equ   <- tbl$irr_equity
  dscr_min  <- tbl$dscr_min_bul
  ltv_fwd   <- tbl$ltv_fwd_max_bul

  # Risk–return: strictly increasing IRRs
  testthat::expect_true(all(diff(irr_proj)  > 0),
    info = "Project IRR must increase from core to opportunistic."
  )
  testthat::expect_true(all(diff(irr_equ)   > 0),
    info = "Equity IRR must increase from core to opportunistic."
  )

  # Coverage: strictly decreasing minimum DSCR
  testthat::expect_true(all(diff(dscr_min) < 0),
    info = "Minimum DSCR (bullet) must decrease from core to opportunistic."
  )

  # Leverage: strictly increasing maximum forward LTV
  testthat::expect_true(all(diff(ltv_fwd)  > 0),
    info = "Maximum forward LTV (bullet) must increase from core to opportunistic."
  )
})


test_that("risk–return cloud: equity IRR dominates and uplift is ordered", {
  tbl <- styles_manifest()

  expected <- c("core", "core_plus", "value_added", "opportunistic")
  tbl <- dplyr::filter(tbl, style %in% expected)
  tbl <- dplyr::arrange(
    tbl,
    factor(style, levels = expected)
  )

  irr_proj <- tbl$irr_project
  irr_equ  <- tbl$irr_equity

  # 1. Equity IRR must exceed project IRR for each style
  testthat::expect_true(
    all(irr_equ > irr_proj),
    info = "For every style, equity IRR must be strictly greater than project IRR."
  )

  # 2. Leverage uplift must be non-decreasing from core to opportunistic
  uplift <- irr_equ - irr_proj

  # numerical tolerance to avoid spurious failures
  tol <- 1e-6

  testthat::expect_true(
    all(diff(uplift) > -tol),
    info = paste(
      "Leverage uplift (equity IRR - project IRR) must be non-decreasing",
      "from core to opportunistic (within numerical tolerance)."
    )
  )
})

test_that("leverage–coverage map: LTV and DSCR follow the canonical ordering", {
  tbl <- styles_manifest()

  expected <- c("core", "core_plus", "value_added", "opportunistic")
  tbl <- dplyr::filter(tbl, style %in% expected)
  tbl <- dplyr::arrange(
    tbl,
    factor(style, levels = expected)
  )

  dscr <- tbl$dscr_min_bul
  ltv  <- tbl$ltv_fwd_max_bul

  core_dscr <- dscr[1L]
  opp_dscr  <- dscr[4L]

  tol <- 1e-6

  # 1) Core must have the highest minimum DSCR (within tolerance)
  testthat::expect_true(
    core_dscr >= max(dscr, na.rm = TRUE) - tol,
    info = "Core style must have the highest minimum DSCR."
  )

  # 2) Opportunistic must have the lowest minimum DSCR (within tolerance)
  testthat::expect_true(
    opp_dscr <= min(dscr, na.rm = TRUE) + tol,
    info = "Opportunistic style must have the lowest minimum DSCR."
  )

  # 3) Forward LTV must still increase monotonically
  testthat::expect_true(
    all(diff(ltv) > 0),
    info = "Maximum forward LTV (bullet) must increase from core to opportunistic."
  )
})

test_that("covenant-breach counts are consistent with style risk buckets", {
  guard_min_dscr <- 1.20
  guard_max_ltv  <- 0.65

  tbl <- styles_breach_counts(
    styles         = c("core", "core_plus", "value_added", "opportunistic"),
    min_dscr_guard = guard_min_dscr,
    max_ltv_guard  = guard_max_ltv
  )

  # Ordre canonique des styles
  tbl <- dplyr::arrange(
    tbl,
    factor(style, levels = c("core", "core_plus", "value_added", "opportunistic"))
  )

  n_dscr <- tbl$n_dscr_breach
  n_ltv  <- tbl$n_ltv_breach

  # Indices de commodité
  low_risk  <- 1:2  # core, core_plus
  high_risk <- 3:4  # value_added, opportunistic

  #  DSCR 

  # 1) Core doit être parmi les styles les plus "propres" en DSCR
  testthat::expect_equal(
    n_dscr[1],
    min(n_dscr),
    info = "Core should have the lowest (or tied lowest) number of DSCR breaches."
  )

  # 2) En moyenne, les styles plus risqués doivent violer plus souvent le DSCR
  mean_low_dscr  <- mean(n_dscr[low_risk])
  mean_high_dscr <- mean(n_dscr[high_risk])

  testthat::expect_true(
    mean_high_dscr >= mean_low_dscr,
    info = "On average, value-added/opportunistic should have at least as many DSCR breaches as core/core_plus."
  )

  #  LTV forward 

  # 3) Core doit aussi être parmi les plus "propres" sur la LTV forward
  testthat::expect_equal(
    n_ltv[1],
    min(n_ltv),
    info = "Core should have the lowest (or tied lowest) number of forward LTV breaches."
  )

  # 4) En moyenne, les styles risqués doivent violer plus souvent la LTV forward
  mean_low_ltv  <- mean(n_ltv[low_risk])
  mean_high_ltv <- mean(n_ltv[high_risk])

  testthat::expect_true(
    mean_high_ltv >= mean_low_ltv,
    info = "On average, value-added/opportunistic should have at least as many forward LTV breaches as core/core_plus."
  )
})

