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
  ltv_init  <- tbl$ltv_init

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

  # Structural leverage: strictly increasing initial LTV
  testthat::expect_true(all(diff(ltv_init) > 0),
    info = "Initial LTV must increase from core to opportunistic."
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

test_that("style presets keep terminal dependence plausible and ordered", {
  tbl <- styles_manifest()

  expected <- c("core", "core_plus", "value_added", "opportunistic")
  tbl <- dplyr::filter(tbl, style %in% expected)
  tbl <- dplyr::arrange(
    tbl,
    factor(style, levels = expected)
  )

  testthat::expect_true(
    all(is.finite(tbl$ops_share)),
    info = "Each style should expose a finite operations PV share."
  )

  testthat::expect_true(
    all(is.finite(tbl$tv_share)),
    info = "Each style should expose a finite terminal-value PV share."
  )

  testthat::expect_true(
    all(abs(tbl$ops_share + tbl$tv_share - 1) < 1e-10),
    info = "Operations and terminal shares should sum to one."
  )

  testthat::expect_true(
    all(diff(tbl$tv_share) > 0),
    info = "Terminal-value dependence should increase from core to opportunistic."
  )

  testthat::expect_true(
    all(tbl$tv_share < 0.90),
    info = "No preset should become implausibly dominated by terminal value."
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
  ltv  <- tbl$ltv_init

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

  # 3) Initial LTV must increase monotonically
  testthat::expect_true(
    all(diff(ltv) > 0),
    info = "Initial LTV must increase from core to opportunistic."
  )
})

test_that("tighter guardrails concentrate LTV pressure in non-core styles", {
  guard_min_dscr <- 1.50
  guard_max_ltv  <- 0.60

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

  testthat::expect_true(
    all(n_dscr >= 0),
    info = "DSCR breach counts should be non-negative."
  )

  testthat::expect_true(
    sum(n_dscr) > 0,
    info = "At least one DSCR breach should be observed under tighter illustrative guardrails."
  )

  #  LTV forward 

  # 1) Core doit être parmi les plus "propres" sur la LTV forward
  testthat::expect_equal(
    n_ltv[1],
    min(n_ltv),
    info = "Core should have the lowest (or tied lowest) number of forward LTV breaches."
  )

  # 2) En moyenne, les styles risqués doivent violer plus souvent la LTV forward
  mean_low_ltv  <- mean(n_ltv[low_risk])
  mean_high_ltv <- mean(n_ltv[high_risk])

  testthat::expect_true(
    mean_high_ltv >= mean_low_ltv,
    info = "On average, value-added/opportunistic should have at least as many forward LTV breaches as core/core_plus."
  )

  testthat::expect_true(
    n_ltv[4] >= n_ltv[1],
    info = "Opportunistic should not have fewer forward-LTV breaches than core under tighter guardrails."
  )
})

test_that("distressed-exit underwriting mode keeps transition logic as default", {
  regimes <- tibble::tibble(
    regime = "strict",
    min_dscr = 1.20,
    max_ltv = 0.65
  )

  legacy_like <- suppressWarnings(
    styles_distressed_exit(
      styles = c("core", "value_added", "opportunistic"),
      regimes = regimes,
      fire_sale_bps = 200,
      refi_min_year = 2L,
      allow_year1_distress = FALSE,
      exit_shock_bps = 200,
      growth_shock = -0.015
    )
  )

  explicit_transition <- suppressWarnings(
    styles_distressed_exit(
      styles = c("core", "value_added", "opportunistic"),
      regimes = regimes,
      fire_sale_bps = 200,
      refi_min_year = 2L,
      allow_year1_distress = FALSE,
      underwriting_mode = "transition",
      exit_shock_bps = 200,
      growth_shock = -0.015
    )
  )

  testthat::expect_identical(
    legacy_like$breach_year,
    explicit_transition$breach_year
  )

  testthat::expect_identical(
    explicit_transition$underwriting_mode,
    rep("transition", nrow(explicit_transition))
  )
})

test_that("stabilized underwriting mode surfaces early lease-up breaches", {
  regimes <- tibble::tibble(
    regime = "strict",
    min_dscr = 1.20,
    max_ltv = 0.65
  )

  transition_tbl <- suppressWarnings(
    styles_distressed_exit(
      styles = "value_added",
      regimes = regimes,
      fire_sale_bps = 200,
      refi_min_year = 2L,
      allow_year1_distress = FALSE,
      underwriting_mode = "transition",
      exit_shock_bps = 200,
      growth_shock = -0.015
    )
  )

  stabilized_tbl <- suppressWarnings(
    styles_distressed_exit(
      styles = "value_added",
      regimes = regimes,
      fire_sale_bps = 200,
      refi_min_year = 2L,
      allow_year1_distress = FALSE,
      underwriting_mode = "stabilized",
      exit_shock_bps = 200,
      growth_shock = -0.015
    )
  )

  testthat::expect_equal(transition_tbl$covenant_start_year, 3L)
  testthat::expect_equal(stabilized_tbl$covenant_start_year, 1L)

  testthat::expect_true(is.na(transition_tbl$breach_year))
  testthat::expect_equal(stabilized_tbl$breach_year, 2L)
  testthat::expect_true(stabilized_tbl$distress_undefined)
  testthat::expect_match(stabilized_tbl$breach_type, "both|dscr|ltv")
})
