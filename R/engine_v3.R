#' Internal v3 engine helpers for CRE case execution
#'
#' The public API still exposes `run_case()` and `run_from_config()`, but the
#' underlying execution now flows through explicit internal stages so that future
#' engine refactors can evolve around stable blocks instead of one monolithic
#' normalization function.
#'
#' @keywords internal
#' @noRd
.engine_normalize_capital_market_inputs <- function(cfg) {
  checkmate::assert_list(cfg, min.len = 1)

  guard_rate <- function(x, nm) {
    if (!is.finite(x) || x < 0 || x > 1) {
      stop(sprintf("Invalid rate '%s': %s", nm, x))
    }
    x
  }

  as_rate <- function(bps = 0) (bps %||% 0) / 1e4

  ltv_init <- cfg$ltv_init %||% 0
  if (!is.finite(ltv_init) || ltv_init < 0 || ltv_init > 1) {
    stop("cfg_normalize(): `ltv_init` must be in [0, 1]. Got: ", ltv_init)
  }

  disc_detail <- NULL
  disc_rate <- switch(
    cfg$disc_method,
    "wacc" = {
      w <- cfg$disc_rate_wacc
      KE <- guard_rate(w$KE, "KE")
      KD <- guard_rate(w$KD, "KD")
      tax <- (w$tax_rate %||% cfg$scr_ratio) %||% 0
      w_e <- 1 - ltv_init
      w_d <- ltv_init

      disc_detail <<- list(
        method = "wacc",
        KE = KE,
        KD = KD,
        tax_rate = tax,
        ltv_structural = ltv_init,
        weight_equity_eff = w_e,
        weight_debt_eff = w_d,
        weight_equity_user = w$weight_equity %||% NA_real_,
        weight_debt_user = w$weight_debt %||% NA_real_
      )

      (w_e * KE) + (w_d * KD * (1 - tax))
    },
    "wacc_capm" = {
      cr <- cfg$disc_rate_wacc_capm
      rf <- cr$risk_free
      beta <- cr$beta
      mrp <- if (!is.null(cr$mrp)) cr$mrp else (cr$mkt_return - rf)
      size <- as_rate(cr$size_illiquidity_bps %||% 0)
      KE <- rf + beta * mrp + size
      KD <- cr$KD
      Lcap <- cr$target_ltv %||% ltv_init
      tax <- (cr$tax_rate %||% cfg$scr_ratio) %||% 0

      disc_detail <<- list(
        method = "wacc_capm",
        risk_free = rf,
        beta = beta,
        mrp = mrp,
        size_illiquidity_bps = cr$size_illiquidity_bps %||% 0,
        KE = KE,
        KD = KD,
        ltv_structural = ltv_init,
        ltv_target = Lcap,
        tax_rate = tax
      )

      (1 - Lcap) * KE + Lcap * KD * (1 - tax)
    },
    "risk_premium" = {
      rp <- cfg$disc_rate_risk_premium
      rf <- rp$rf
      add <- sum(unlist(rp[names(rp) != "rf"]), na.rm = TRUE)

      disc_detail <<- list(
        method = "risk_premium",
        rf = rf,
        premia = rp[names(rp) != "rf"]
      )

      rf + add
    },
    "yield_plus_growth" = {
      yg <- cfg$disc_rate_yield_plus_growth
      adj <- as_rate(yg$adj_obsolescence_bps %||% 0)

      disc_detail <<- list(
        method = "yield_plus_growth",
        property_yield = yg$property_yield,
        growth = yg$growth,
        adj_obsolescence_bps = yg$adj_obsolescence_bps %||% 0
      )

      yg$property_yield + yg$growth + adj
    },
    stop("Unknown disc_method: ", cfg$disc_method)
  )

  list(
    horizon_years = as.integer(cfg$horizon_years),
    index_rate = cfg$index_rate %||% 0,
    opex_inflation_rate = cfg$opex_inflation_rate %||% (cfg$index_rate %||% 0),
    capex_inflation_rate = cfg$capex_inflation_rate %||% (cfg$index_rate %||% 0),
    landlord_base_opex_sqm = cfg$landlord_base_opex_sqm %||% 0,
    leasing_cost_pct = cfg$leasing_cost_pct %||% 0,
    disc_rate = disc_rate,
    disc_detail = disc_detail,
    exit_yield = derive_exit_yield(cfg$entry_yield, cfg$exit_yield_spread_bps),
    exit_cost = if (!is.null(cfg$exit_transaction_costs)) {
      sum(unlist(cfg$exit_transaction_costs), na.rm = TRUE)
    } else {
      cfg$exit_cost %||% 0
    },
    ltv_init = ltv_init,
    rate_annual = cfg$rate_annual,
    maturity = as.integer(cfg$horizon_years),
    type = if ((cfg$extra_amort_pct %||% 0) > 0) "amort" else "bullet",
    arrangement_fee_pct = cfg$arrangement_fee_pct %||% 0,
    capitalized_fees = isTRUE(cfg$capitalized_fees)
  )
}

#' @keywords internal
#' @noRd
.engine_project_lease_unit <- function(unit,
                                       horizon_years,
                                       purchase_year,
                                       landlord_base_opex_sqm,
                                       opex_sqm,
                                       leasing_cost_pct) {
  area_u <- unit$area %||% 0
  ev_u <- lapply(unit$events, function(e) {
    e$rent <- e$rent %||% 0
    e$vac <- e$vac %||% 0
    e$free_months <- e$free_months %||% 0
    e$capex_sqm <- e$capex_sqm %||% 0
    e$new_lease <- e$new_lease %||% 0
    e
  })

  st <- leases_tbl_structuration(
    ev_u,
    horizon = horizon_years,
    base_year = purchase_year
  )

  rent_gross <- st$rent * area_u
  rent_occupied <- rent_gross * (1 - st$vac)
  vac_curr <- st$vac
  vac_prev <- c(st$vac[1], head(st$vac, -1))
  new_leased_share <- pmax(0, vac_prev - vac_curr)
  free_frac_vec <- pmin(st$free, 1)
  free_loss <- rent_gross * new_leased_share * free_frac_vec * st$new_lease
  rent_eff <- rent_occupied - free_loss

  brokerage_cost <- leasing_cost_pct * (st$rent * area_u * new_leased_share)
  opex_vac_u <- (opex_sqm %||% 0) * area_u * st$vac
  opex_base_u <- landlord_base_opex_sqm * area_u

  list(
    noi = rent_eff,
    opex = opex_vac_u + opex_base_u,
    capex = (st$capex_sqm * area_u) + brokerage_cost,
    noi_price_base = st$rent[1L] * area_u
  )
}

#' @keywords internal
#' @noRd
.engine_project_operations <- function(cfg) {
  checkmate::assert_list(cfg, min.len = 1)

  N <- as.integer(cfg$horizon_years)
  yrs_idx <- seq_len(N) - 1L
  idx_rate <- cfg$index_rate %||% 0
  opex_infl <- cfg$opex_inflation_rate %||% idx_rate
  capx_infl <- cfg$capex_inflation_rate %||% idx_rate
  base_opex <- cfg$landlord_base_opex_sqm %||% 0
  leasing_pct <- cfg$leasing_cost_pct %||% 0

  noi_vec <- numeric(N)
  opex_vec <- numeric(N)
  capex_vec <- numeric(N)
  noi_price_base <- 0

  if (!is.null(cfg$leases) && length(cfg$leases) > 0) {
    for (unit in cfg$leases) {
      unit_ops <- .engine_project_lease_unit(
        unit = unit,
        horizon_years = N,
        purchase_year = cfg$purchase_year,
        landlord_base_opex_sqm = base_opex,
        opex_sqm = cfg$opex_sqm %||% 0,
        leasing_cost_pct = leasing_pct
      )

      noi_vec <- noi_vec + unit_ops$noi
      opex_vec <- opex_vec + unit_ops$opex
      capex_vec <- capex_vec + unit_ops$capex
      noi_price_base <- noi_price_base + unit_ops$noi_price_base
    }

    mode <- "lease_events"
  } else {
    noi_vec[1] <- 1
    mode <- if (isTRUE(cfg$top_down_noi)) "top_down_placeholder" else "empty_placeholder"
  }

  list(
    mode = mode,
    noi_vec = noi_vec * (1 + idx_rate)^yrs_idx,
    opex_vec = opex_vec * (1 + opex_infl)^yrs_idx,
    capex_vec = capex_vec * (1 + capx_infl)^yrs_idx,
    noi_price_base = noi_price_base
  )
}

#' @keywords internal
#' @noRd
.engine_resolve_pricing <- function(cfg, operations, market_inputs) {
  acq_price_ht <- if (!is.null(cfg$acq_price_ht)) {
    cfg$acq_price_ht
  } else {
    if (!is.finite(operations$noi_price_base) || operations$noi_price_base <= 0) {
      stop(
        "Computed theoretical first-year NOI for pricing is non-positive. ",
        "Provide `acq_price_ht` explicitly in the preset."
      )
    }
    operations$noi_price_base / (cfg$entry_yield %||% stop("entry_yield is required"))
  }

  acq_price_di <- acq_price_ht * (1 + (cfg$acq_cost_rate %||% 0))

  list(
    acq_price_ht = acq_price_ht,
    acq_price_di = acq_price_di,
    debt_init = acq_price_di * market_inputs$ltv_init,
    equity_init = acq_price_di * (1 - market_inputs$ltv_init),
    value_base = if (isTRUE(cfg$top_down_noi)) {
      acq_price_ht
    } else {
      (operations$noi_vec %||% c(NA_real_))[1] / (cfg$entry_yield %||% 0.05)
    }
  )
}

#' @keywords internal
#' @noRd
.engine_flatten_normalized_case <- function(market_inputs, operations, pricing) {
  list(
    disc_rate = market_inputs$disc_rate,
    disc_detail = market_inputs$disc_detail,
    exit_yield = market_inputs$exit_yield,
    exit_cost = market_inputs$exit_cost,
    acq_price_ht = pricing$acq_price_ht,
    acq_price_di = pricing$acq_price_di,
    ltv_init = market_inputs$ltv_init,
    debt_init = pricing$debt_init,
    equity_init = pricing$equity_init,
    rate_annual = market_inputs$rate_annual,
    maturity = market_inputs$maturity,
    type = market_inputs$type,
    arrangement_fee_pct = market_inputs$arrangement_fee_pct,
    capitalized_fees = market_inputs$capitalized_fees,
    noi_vec = operations$noi_vec,
    opex_vec = operations$opex_vec,
    capex_vec = operations$capex_vec
  )
}

#' @keywords internal
#' @noRd
.engine_resolve_acquisition_price <- function(pricing, ltv_base) {
  switch(
    ltv_base,
    "price_di" = pricing$acq_price_di,
    "price_ht" = pricing$acq_price_ht,
    "value" = pricing$value_base
  )
}

#' @keywords internal
#' @noRd
.engine_build_dcf_inputs <- function(config, operations, market_inputs, acq_price) {
  use_top_down_noi <- isTRUE(config$top_down_noi)
  horizon <- length(operations$noi_vec)

  list(
    acq_price = acq_price,
    entry_yield = get_cfg(config, "entry_yield"),
    exit_yield = market_inputs$exit_yield,
    horizon_years = horizon,
    disc_rate = market_inputs$disc_rate,
    exit_cost = market_inputs$exit_cost,
    capex = if (use_top_down_noi) {
      get_cfg(config, "simple_capex", default = 0)
    } else {
      operations$capex_vec
    },
    index_rent = if (use_top_down_noi) {
      rep(get_cfg(config, "index_rate", default = 0), length.out = horizon)
    } else {
      0
    },
    vacancy = if (use_top_down_noi) {
      rep(get_cfg(config, "vacancy_rate", default = 0), length.out = horizon)
    } else {
      0
    },
    opex = if (use_top_down_noi) {
      0
    } else {
      operations$opex_vec
    },
    noi = if (use_top_down_noi) {
      NULL
    } else {
      operations$noi_vec
    }
  )
}

#' @keywords internal
#' @noRd
.engine_prepare_case <- function(config = NULL,
                                 config_file = NULL,
                                 debt_type = NULL,
                                 ltv_base = c("price_di", "price_ht", "value")) {
  if (!is.null(debt_type)) {
    debt_type <- match.arg(debt_type, choices = c("bullet", "amort"))
  }
  ltv_base <- match.arg(ltv_base)

  if (is.null(config)) {
    if (is.null(config_file)) {
      config_file <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
    }
    config <- dcf_read_config(config_file)
  }

  cfg_validate(config)

  market_inputs <- .engine_normalize_capital_market_inputs(config)
  operations <- .engine_project_operations(config)
  pricing <- .engine_resolve_pricing(config, operations, market_inputs)
  norm <- .engine_flatten_normalized_case(
    market_inputs = market_inputs,
    operations = operations,
    pricing = pricing
  )

  acq_price <- .engine_resolve_acquisition_price(
    pricing = pricing,
    ltv_base = ltv_base
  )

  dcf_inputs <- .engine_build_dcf_inputs(
    config = config,
    operations = operations,
    market_inputs = market_inputs,
    acq_price = acq_price
  )

  debt_type_eff <- debt_type %||% get_cfg(config, "simple_debt_type", default = NULL) %||% market_inputs$type
  fee_pct <- market_inputs$arrangement_fee_pct %||% 0

  financing <- resolve_financing_inputs(
    acq_price = acq_price,
    ltv_init = market_inputs$ltv_init %||% 0,
    arrangement_fee_pct = fee_pct,
    capitalized_fees = market_inputs$capitalized_fees
  )

  list(
    config = config,
    config_file = config_file,
    market_inputs = market_inputs,
    operations = operations,
    pricing = pricing,
    norm = norm,
    ltv_base = ltv_base,
    acq_price = acq_price,
    dcf_inputs = dcf_inputs,
    debt_type_eff = debt_type_eff,
    fee_pct = fee_pct,
    financing = financing,
    debt_schedule_args = list(
      principal = financing$principal_sched,
      rate_annual = market_inputs$rate_annual,
      maturity = market_inputs$maturity,
      type = debt_type_eff,
      extra_amort_pct = get_cfg(config, "extra_amort_pct", default = 0),
      arrangement_fee_pct = financing$arrangement_fee_pct_sched
    ),
    credit_ratio_args = list(
      exit_yield = market_inputs$exit_yield,
      covenants = list(
        dscr_min = 1.25,
        ltv_max = (market_inputs$ltv_init %||% get_cfg(config, "ltv_init", default = 0.60)) + 0.05,
        debt_yield_min = norm$debt_yield_min %||% 0.08
      ),
      dscr_basis = "noi",
      ignore_balloon_in_min = TRUE,
      maturity_year = market_inputs$maturity %||% get_cfg(config, "maturity", default = 5L)
    )
  )
}

#' @keywords internal
#' @noRd
.engine_execute_case <- function(prepared, include_comparison = TRUE) {
  dcf_res <- do.call(dcf_calculate, prepared$dcf_inputs)
  debt_sched <- do.call(debt_built_schedule, prepared$debt_schedule_args)

  full_cf <- cf_make_full_table(dcf_res, debt_sched)
  full_cf_ratios <- do.call(
    add_credit_ratios,
    c(
      list(cf_tab = full_cf, debt_sched = debt_sched),
      prepared$credit_ratio_args
    )
  )

  comparison <- NULL
  if (isTRUE(include_comparison)) {
    comparison <- compare_financing_scenarios(
      dcf_res,
      acq_price = prepared$acq_price,
      ltv = prepared$market_inputs$ltv_init,
      rate = prepared$market_inputs$rate_annual,
      maturity = prepared$market_inputs$maturity,
      arrangement_fee_pct = prepared$fee_pct,
      capitalized_fees = prepared$market_inputs$capitalized_fees
    )
  }

  list(
    dcf = dcf_res,
    debt = debt_sched,
    full = full_cf,
    ratios = full_cf_ratios,
    all_equity = compute_unleveraged_metrics(dcf_res),
    leveraged = compute_leveraged_metrics(
      dcf_res,
      debt_sched,
      equity_invest = prepared$financing$equity_invest
    ),
    comparison = comparison
  )
}
