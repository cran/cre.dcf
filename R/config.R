
#' Minimal specification template for a Discounted Cash Flow (DCF) case
#'
#' Returns a ready-to-edit list that matches the package's YAML grammar.
#' Use this for interactive prototyping or to generate a YAML file.
#'
#' @return A named list with all required top-level keys and sane defaults.
#' @examples
#' cfg <- dcf_spec_template()
#' str(cfg, max.level = 1)
#' @export
dcf_spec_template <- function() {
  list(
    purchase_year = as.integer(format(Sys.Date(), "%Y")),
    horizon_years = 10L,
    index_rate = 0.02,
    entry_yield = 0.065,
    acq_cost_rate = 0.06,
    exit_yield_spread_bps = 0,
    exit_cost = 0.015,
    disc_method = "wacc",
    disc_rate_wacc = list(KE = 0.08, KD = 0.04, tax_rate = 0.28),
    # alternatives (not used unless disc_method changes)
    disc_rate_wacc_capm = list(risk_free = 0.03, beta = 1.0,
                               mkt_return = 0.07, KD = 0.04,
                               size_illiquidity_bps = 0, target_ltv = 0.5, tax_rate = 0.28),
    disc_rate_risk_premium = list(rf = 0.03,
                                  liquidity_premium = 0.00,
                                  obsolescence_premium = 0.00,
                                  income_risk_premium = 0.00),
    disc_rate_yield_plus_growth = list(property_yield = 0.065, growth = 0.01,
                                       adj_obsolescence_bps = 0),
    ltv_init = 0.55,
    rate_annual = 0.045,
    extra_amort_pct = 0.00,
    scr_ratio = 0.28,
    opex_sqm = 0,
    leasing_cost_pct = 0,
    capitalized_fees = FALSE,
    arrangement_fee_pct = 0.00,
    maturity = NULL,          # default is min(horizon, 5) in cfg_normalize()
    ltv_base = "price_ht",    # or "price_di" | "value"
    leases = list()           # fill with per-unit structures as in your examples
  )
}

#' Run a full DCF case from a list or a YAML file
#'
#' User-facing single entry point. Accepts either an in-memory \code{config} list
#' or a \code{config_file} path to YAML. Both routes share the same validation
#' and normalization pathway, ensuring identical downstream behavior.
#'
#' @param config Optional list configuration following the YAML grammar.
#' @param config_file Optional path to a YAML configuration file. If both
#'   \code{config} and \code{config_file} are \code{NULL}, defaults to the package
#'   example at \code{inst/extdata/config.yml}.
#' @param debt_type Debt schedule type to use (\code{"bullet"} or \code{"amort"}).
#'   This parameter overrides any implicit type inferred in normalization.
#' @param ltv_base Base for loan-to-value (LTV) and initial principal. One of
#'   \code{"price_di"}, \code{"price_ht"}, or \code{"value"}.
#'
#' @return A list containing pricing (acquisition price net of taxes, acquisition costs,
#'   and acquisition price including costs), all-equity metrics, leveraged metrics,
#'   a comparison table, the full cash-flow table with credit ratios, and selected
#'   configuration flags.
#'
#' @details
#' The function centralizes user ergonomics:
#' \itemize{
#'   \item Reads either a list or a YAML file.
#'   \item Validates and normalizes with \code{cfg_validate()} and \code{cfg_normalize()}.
#'   \item Computes the unlevered discounted cash flow (DCF), builds a debt schedule,
#'     computes leveraged metrics, and adds credit ratios to the full cash-flow table.
#'   \item Handles capitalized arrangement fees by adjusting the scheduled principal
#'     to avoid double-counting.
#' }
#'
#' @importFrom yaml read_yaml
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#'
#' @examples
#' # R list route
#' cfg <- dcf_spec_template()
#' cfg$leases <- list(
#'   list(
#'     unit = "U",
#'     area = 1000,
#'     events = list(
#'       list(
#'         start = cfg$purchase_year,
#'         end   = cfg$purchase_year + cfg$horizon_years,  # keep NOI positive in terminal year
#'         rent = 200,
#'         free_months = 0,
#'         capex_sqm = 0,
#'         vac = 0,
#'         new_lease = 0
#'       )
#'     )
#'   )
#' )
#' out <- run_case(config = cfg, debt_type = "bullet")
#' names(out)
#' @export
run_case <- function(config = NULL,
                     config_file = NULL,
                     debt_type = c("bullet", "amort"),
                     ltv_base  = c("price_di", "price_ht", "value")) {

  debt_type <- match.arg(debt_type)
  ltv_base  <- match.arg(ltv_base)

  # 1) Source of truth: config list or YAML file -
  if (is.null(config)) {
    if (is.null(config_file)) {
      config_file <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
    }
    config <- dcf_read_config(config_file)
  }
  cfg_validate(config)

  norm <- cfg_normalize(config)

  # 2) Acquisition pricing base -
  acq_price <- switch(
    ltv_base,
    "price_di" = norm$acq_price_di,
    "price_ht" = norm$acq_price_ht,
    "value"    = (norm$noi_vec %||% c(NA_real_))[1] /
      (get_cfg(config, "entry_yield") %||% 0.05)
  )

  # 3) Unlevered DCF -
  dcf_res <- dcf_calculate(
    acq_price     = acq_price,
    entry_yield   = get_cfg(config, "entry_yield"),
    exit_yield    = norm$exit_yield,
    horizon_years = length(norm$noi_vec),
    disc_rate     = norm$disc_rate,
    exit_cost     = norm$exit_cost,
    capex         = norm$capex_vec,
    opex          = norm$opex_vec,
    noi           = norm$noi_vec
  )

  metrics_eq <- compute_unleveraged_metrics(dcf_res)

  # 4) Debt schedule build with fee capitalization logic -
  principal_base <- acq_price * (norm$ltv_init %||% 0)
  fee_pct        <- norm$arrangement_fee_pct %||% 0
  capitalized    <- isTRUE(norm$capitalized_fees)

  if (capitalized) {
    # Les frais sont capitalisés dans le principal tiré
    principal_sched        <- principal_base + principal_base * fee_pct
    arrangement_for_sched  <- 0
    # Equity = prix DI - tirage (frais inclus)
    equity_invest          <- acq_price - principal_sched
  } else {
    # Frais payés cash à t0 (supportés par l'equity)
    principal_sched        <- principal_base
    arrangement_for_sched  <- fee_pct
    equity_invest          <- acq_price - principal_sched + principal_base * fee_pct
  }

  # For diagnostic/exposition
  debt_init   <- principal_sched
  equity_init <- equity_invest

  debt_sched <- debt_built_schedule(
    principal           = principal_sched,
    rate_annual         = norm$rate_annual,
    maturity            = norm$maturity,
    type                = debt_type,
    extra_amort_pct     = get_cfg(config, "extra_amort_pct", default = 0),
    arrangement_fee_pct = arrangement_for_sched
  )

  # 5) Leveraged metrics & comparison 
  metrics_lvd <- compute_leveraged_metrics(
    dcf_res,
    debt_sched,
    equity_invest = equity_invest
  )

  comparison <- compare_financing_scenarios(
    dcf_res,
    acq_price = acq_price,
    ltv       = norm$ltv_init,
    rate      = norm$rate_annual,
    maturity  = norm$maturity
  )

  # 6) Table complète + ratios de crédit -
  full_cf <- cf_make_full_table(dcf_res, debt_sched)
  full_cf <- add_credit_ratios(
    full_cf, debt_sched,
    exit_yield = norm$exit_yield,
    covenants  = list(
      dscr_min       = 1.25,
      ltv_max        = (norm$ltv_init %||% get_cfg(config, "ltv_init", 0.60)) + 0.05,
      debt_yield_min = norm$debt_yield_min %||% 0.08
    ),
    dscr_basis            = "noi",
    ignore_balloon_in_min = TRUE,
    maturity_year         = norm$maturity %||% get_cfg(config, "maturity", 5L)
  )

  # 7) Pricing breakdown (HT / costs / DI) 
  price_ht <- norm$acq_price_ht
  price_di <- norm$acq_price_di
  acq_cost <- price_di - price_ht

  list(
    pricing    = list(price_ht = price_ht, acq_cost = acq_cost, price_di = price_di),
    all_equity = metrics_eq,
    leveraged  = metrics_lvd,
    comparison = comparison,
    cashflows  = full_cf,
    config     = list(
      ltv_base            = ltv_base,          # "price_di" / "price_ht" / "value"
      ltv_init            = norm$ltv_init,     # structural LTV
      debt_init           = debt_init,         # actual initial principal
      equity_init         = equity_init,       # actual initial equity
      capitalized_fees    = capitalized,
      arrangement_fee_pct = fee_pct,
      disc_method         = config$disc_method,
      disc_rate           = norm$disc_rate,
      disc_detail         = norm$disc_detail   # <- new: full WACC decomposition
    )
  )
}


#' Safe access to nested YAML values
#' @param cfg list configuration object.
#' @param ... nested keys.
#' @param default value if missing.
#' @return value or default.
#' @export
get_cfg <- function(cfg, ..., default = NULL) {
  cur <- cfg
  for (k in list(...)) {
    if (is.null(cur[[k]])) return(default)
    cur <- cur[[k]]
  }
  cur %||% default
}

#' Validate YAML configuration structure
#' @param cfg list returned by dcf_read_config().
#' @return cfg invisibly (or error if invalid).
#' @export
cfg_validate <- function(cfg) {
  checkmate::assert_list(cfg, min.len = 1)
  checkmate::assert_integerish(cfg$purchase_year, any.missing = FALSE, len = 1)
  checkmate::assert_integerish(cfg$horizon_years, lower = 1, len = 1)

  checkmate::assert_number(cfg$index_rate, lower = 0, upper = 1)
  checkmate::assert_number(cfg$entry_yield, lower = 0, upper = 1)
  checkmate::assert_number(cfg$acq_cost_rate, lower = 0, upper = 1)
  checkmate::assert_number(cfg$exit_yield_spread_bps, finite = TRUE)

  # exit_cost is either legacy or new structure
  if (!is.null(cfg$exit_transaction_costs)) {
    checkmate::assert_list(cfg$exit_transaction_costs)
  } else {
    checkmate::assert_number(cfg$exit_cost, lower = 0, upper = 1)
  }

  checkmate::assert_choice(
    cfg$disc_method,
    choices = c("wacc", "wacc_capm", "risk_premium", "yield_plus_growth")
  )

  # Discount rate block checks -
  if (cfg$disc_method == "wacc") {
    w <- cfg$disc_rate_wacc
    checkmate::assert_list(w)
    checkmate::assert_number(w$KE, lower = 0, upper = 1)
    checkmate::assert_number(w$KD, lower = 0, upper = 1)
  }

  if (cfg$disc_method == "wacc_capm") {
    cr <- cfg$disc_rate_wacc_capm
    checkmate::assert_list(cr)
    checkmate::assert_number(cr$risk_free)
    checkmate::assert_number(cr$beta)
    checkmate::assert_number(cr$mkt_return, null.ok = TRUE)
    checkmate::assert_number(cr$KD)
    checkmate::assert_number(cr$target_ltv, null.ok = TRUE)
  }

  if (cfg$disc_method == "risk_premium") {
    rp <- cfg$disc_rate_risk_premium
    checkmate::assert_list(rp)
    checkmate::assert_number(rp$rf)
  }

  if (cfg$disc_method == "yield_plus_growth") {
    yg <- cfg$disc_rate_yield_plus_growth
    checkmate::assert_list(yg)
    checkmate::assert_number(yg$property_yield)
    checkmate::assert_number(yg$growth)
  }

  checkmate::assert_number(cfg$ltv_init,       lower = 0, upper = 1)
  checkmate::assert_number(cfg$rate_annual,    lower = 0, upper = 1)
  checkmate::assert_number(cfg$extra_amort_pct, lower = 0, upper = 1)
  checkmate::assert_number(cfg$scr_ratio,      lower = 0, upper = 1)
  checkmate::assert_number(cfg$opex_sqm,       lower = 0)

  # -
  # Helper: validate events for one unit (no overlaps, no gaps, sane bounds)
  # -
  validate_unit_events <- function(events, unit_label, purchase_year, horizon_years) {
    checkmate::assert_list(events, min.len = 1)

    # Extract and validate start/end as integer vectors
    starts <- vapply(
      events,
      function(e) {
        checkmate::assert_integerish(e$start, any.missing = FALSE, len = 1)
        as.integer(e$start)
      },
      integer(1)
    )

    ends <- vapply(
      events,
      function(e) {
        checkmate::assert_integerish(e$end, any.missing = FALSE, len = 1)
        as.integer(e$end)
      },
      integer(1)
    )

    # Basic temporal consistency
    if (any(ends < starts)) {
      idx <- which(ends < starts)[1L]
      stop(sprintf(
        "Lease '%s': event %d has end (%d) < start (%d).",
        unit_label, idx, ends[idx], starts[idx]
      ))
    }

    # Sort events by (start, end) to have a stable ordering
    ord    <- order(starts, ends)
    starts <- starts[ord]
    ends   <- ends[ord]
    events <- events[ord]

    # Check that lease timeline does not start before purchase_year
    if (min(starts) < purchase_year) {
      stop(sprintf(
        "Lease '%s': first event starts in %d before purchase_year = %d.",
        unit_label, min(starts), purchase_year
      ))
    }

    # Optional: disallow events that extend strictly beyond the simulation horizon
    horizon_end <- purchase_year + cfg$horizon_years
    if (max(ends) > horizon_end) {
      warning(sprintf(
        "Lease '%s': last event ends in %d beyond horizon end %d. ",
        unit_label, max(ends), horizon_end
      ))
    }

    # Enforce no overlaps and no gaps between successive events
    if (length(starts) > 1L) {
      gaps <- starts[-1L] - (ends[-length(ends)] + 1L)

      if (any(gaps > 0L)) {
        k <- which(gaps > 0L)[1L]
        stop(sprintf(
          paste0(
            "Lease '%s': gap detected between events %d and %d ",
            "(end = %d, next start = %d). ",
            "Encode vacancy as an explicit event with vac = 1 rather than leaving a hole."
          ),
          unit_label,
          k, k + 1L,
          ends[k], starts[k + 1L]
        ))
      }

      if (any(gaps < 0L)) {
        k <- which(gaps < 0L)[1L]
        stop(sprintf(
          paste0(
            "Lease '%s': overlapping events %d and %d ",
            "(end = %d, next start = %d)."
          ),
          unit_label,
          k, k + 1L,
          ends[k], starts[k + 1L]
        ))
      }
    }

    # Per-event numeric checks (kept close to your original logic)
    for (i in seq_along(events)) {
      e <- events[[i]]

      checkmate::assert_number(e$rent,       lower = 0, null.ok = TRUE)
      checkmate::assert_number(e$free_months, lower = 0, null.ok = TRUE)
      checkmate::assert_number(e$capex_sqm,  lower = 0, null.ok = TRUE)
      checkmate::assert_number(e$vac,        lower = 0, upper = 1, null.ok = TRUE)
      checkmate::assert_integerish(e$new_lease, null.ok = TRUE, len = 1)
    }

    list(
      first_start = min(starts),
      last_end    = max(ends)
    )
  }

  # ===========================================================================
  # Lease block: stronger structural checks on events timelines
  # ===========================================================================
  if (!is.null(cfg$leases)) {
    checkmate::assert_list(cfg$leases)

    max_end <- cfg$purchase_year

    for (u in cfg$leases) {
      # unit label is only for error messages, falls back to "unknown"
      unit_label <- u$unit %||% "unknown_unit"

      checkmate::assert_number(u$area, lower = 0)

      res <- validate_unit_events(
        events        = u$events,
        unit_label    = unit_label,
        purchase_year = cfg$purchase_year,
        horizon_years = cfg$horizon_years
      )

      # Update global max_end across all units
      max_end <- max(max_end, res$last_end)
    }

    # Compute required minimum horizon (same logic as before)
    required_horizon <- max_end - cfg$purchase_year

    if (cfg$horizon_years < required_horizon) {
      stop(sprintf(
        paste(
          "Invalid  Discounted Cash Flow (DCF) horizon: horizon_years = %d, but leases run until %d.",
          "Required minimum horizon = %d."
        ),
        cfg$horizon_years, max_end, required_horizon
      ))
    }
  }

  invisible(cfg)
}

# Safe helper for defaulting NULL
`%||%` <- function(x, y) if (is.null(x)) y else x



#' Normalize YAML into canonical  Discounted Cash Flow (DCF)/debt parameters
#'
#' @description
#' Converts a raw YAML configuration into a set of scalars and vectors
#' directly usable by `dcf_calculate()` and `debt_built_schedule()`.
#'
#' @param cfg list parsed from YAML (raw, not yet normalized).
#'
#' @return list including in particular:
#' \itemize{
#'   \item `disc_rate`, `exit_yield`, `exit_cost`,
#'   \item `acq_price_ht`, `acq_price_di`,
#'   \item `ltv_init`, `rate_annual`, `maturity`, `type`,
#'   \item `arrangement_fee_pct`, `capitalized_fees`,
#'   \item `noi_vec`, `opex_vec`, `capex_vec` (vectors of length `N`).
#' }
#' @export
cfg_normalize <- function(cfg) {
  N <- as.integer(cfg$horizon_years)

  #  0) Safe defaults and structural leverage -

  idx_rate   <- cfg$index_rate %||% 0
  opex_infl  <- cfg$opex_inflation_rate %||% idx_rate
  capx_infl  <- cfg$capex_inflation_rate %||% idx_rate

  base_opex   <- cfg$landlord_base_opex_sqm %||% 0
  leasing_pct <- cfg$leasing_cost_pct %||% 0

  ltv_init <- cfg$ltv_init %||% 0

  if (!is.finite(ltv_init) || ltv_init < 0 || ltv_init > 1) {
    stop("cfg_normalize(): `ltv_init` must be in [0, 1]. Got: ", ltv_init)
  }

  #  1) Discount rate synthesis -

  guard_rate <- function(x, nm) {
    if (!is.finite(x) || x < 0 || x > 1) {
      stop(sprintf("Invalid rate '%s': %s", nm, x))
    }
    x
  }

  as_rate <- function(bps = 0) (bps %||% 0) / 1e4

  disc_detail <- NULL  # container for diagnostic output

  disc_rate <- switch(
    cfg$disc_method,
    "wacc" = {
      w   <- cfg$disc_rate_wacc
      KE  <- guard_rate(w$KE, "KE")
      KD  <- guard_rate(w$KD, "KD")
      tax <- (w$tax_rate %||% cfg$scr_ratio) %||% 0

      # Effective weights used by the engine: derived from structural LTV
      w_e <- 1 - ltv_init
      w_d <- ltv_init

      # Optional: keep user-supplied weights as documentation (if present)
      w_e_user <- w$weight_equity %||% NA_real_
      w_d_user <- w$weight_debt   %||% NA_real_

      disc_detail <<- list(
        method               = "wacc",
        KE                   = KE,
        KD                   = KD,
        tax_rate             = tax,
        ltv_structural       = ltv_init,
        weight_equity_eff    = w_e,
        weight_debt_eff      = w_d,
        weight_equity_user   = w_e_user,
        weight_debt_user     = w_d_user
      )

      (w_e * KE) + (w_d * KD * (1 - tax))
    },
    "wacc_capm" = {
      cr   <- cfg$disc_rate_wacc_capm
      rf   <- cr$risk_free
      beta <- cr$beta
      mrp  <- if (!is.null(cr$mrp)) cr$mrp else (cr$mkt_return - rf)
      size <- as_rate(cr$size_illiquidity_bps %||% 0)
      KE   <- rf + beta * mrp + size
      KD   <- cr$KD
      Lcap <- cr$target_ltv %||% ltv_init
      tax  <- (cr$tax_rate %||% cfg$scr_ratio) %||% 0

      disc_detail <<- list(
        method               = "wacc_capm",
        risk_free            = rf,
        beta                 = beta,
        mrp                  = mrp,
        size_illiquidity_bps = cr$size_illiquidity_bps %||% 0,
        KE                   = KE,
        KD                   = KD,
        ltv_structural       = ltv_init,
        ltv_target           = Lcap,
        tax_rate             = tax
      )

      (1 - Lcap) * KE + Lcap * KD * (1 - tax)
    },
    "risk_premium" = {
      rp  <- cfg$disc_rate_risk_premium
      rf  <- rp$rf
      add <- sum(unlist(rp[names(rp) != "rf"]), na.rm = TRUE)

      disc_detail <<- list(
        method = "risk_premium",
        rf     = rf,
        premia = rp[names(rp) != "rf"]
      )

      rf + add
    },
    "yield_plus_growth" = {
      yg  <- cfg$disc_rate_yield_plus_growth
      adj <- as_rate(yg$adj_obsolescence_bps %||% 0)

      disc_detail <<- list(
        method               = "yield_plus_growth",
        property_yield       = yg$property_yield,
        growth               = yg$growth,
        adj_obsolescence_bps = yg$adj_obsolescence_bps %||% 0
      )

      yg$property_yield + yg$growth + adj
    },
    stop("Unknown disc_method: ", cfg$disc_method)
  )

  #  2) Exit yield from entry + spread -

  exit_yield <- derive_exit_yield(cfg$entry_yield, cfg$exit_yield_spread_bps)

  #  3) Build NOI / OPEX / CAPEX vectors 

  yrs_idx   <- seq_len(N) - 1L
  noi_vec   <- numeric(N)
  opex_vec  <- numeric(N)  # OPEX borne by the landlord
  capex_vec <- numeric(N)

  # Theoretical year-1 NOI (no vacancy, no free months) for pricing
  noi_price_base <- 0

  if (!is.null(cfg$leases) && length(cfg$leases) > 0) {
    for (u in cfg$leases) {
      area_u <- u$area %||% 0

      # Events per unit (non-indexed, annual rents)
      ev_u <- lapply(u$events, function(e) {
        e$rent        <- e$rent        %||% 0
        e$vac         <- e$vac         %||% 0
        e$free_months <- e$free_months %||% 0
        e$capex_sqm   <- e$capex_sqm   %||% 0
        e$new_lease   <- e$new_lease   %||% 0
        e
      })

      st <- leases_tbl_structuration(ev_u, horizon = N, base_year = cfg$purchase_year)
      # st$rent in €/sqm/year, st$vac in [0,1], st$free = fraction of the year at the start,
      # st$new_lease in {0,1}

      # Theoretical first-year NOI for pricing (no vacancy, no free) -
      # Year index 1 corresponds to purchase_year
      noi_price_base <- noi_price_base + st$rent[1L] * area_u

      # NOI path (effective, with vacancy and free months) -
      rent_gross    <- st$rent * area_u
      rent_occupied <- rent_gross * (1 - st$vac)

      # Change in vacancy: vac_prev - vac_curr > 0 ⇒ re-let area
      vac_curr <- st$vac
      vac_prev <- c(st$vac[1], head(st$vac, -1))
      new_leased_share <- pmax(0, vac_prev - vac_curr)   # share of newly leased area

      # Free-rent fraction (0..1), bounded
      free_frac_vec <- pmin(st$free, 1)

      # Lost rent due to free months:
      # only on the newly leased area,
      # only when new_lease == 1.
      free_loss <- rent_gross * new_leased_share * free_frac_vec * st$new_lease

      # Effective rent after accounting for free months
      rent_eff <- rent_occupied - free_loss

      noi_vec <- noi_vec + rent_eff

      # CAPEX works: applied in the event year, €/sqm × area
      capex_vec <- capex_vec + (st$capex_sqm * area_u)

      # Brokerage / leasing fees: leasing commissions 

      # Face annual rent on the newly leased area
      rent_new_area <- st$rent * area_u * new_leased_share

      # Leasing fees = % of one year of rent on this newly leased area
      brokerage_cost <- leasing_pct * rent_new_area

      # Treat leasing fees as additional capex in year t
      capex_vec <- capex_vec + brokerage_cost

      # Landlord OPEX:
      # opex_sqm (rechargeable charges): borne only on vacant space,
      # landlord_base_opex_sqm (non-recoverable): borne continuously.
      opex_vac_u  <- (cfg$opex_sqm %||% 0) * area_u * st$vac
      opex_base_u <- base_opex * area_u
      opex_vec    <- opex_vec + (opex_vac_u + opex_base_u)
    }

    # Indexation / inflation over the horizon
    noi_vec   <- noi_vec   * (1 + idx_rate)  ^ yrs_idx
    opex_vec  <- opex_vec  * (1 + opex_infl) ^ yrs_idx
    capex_vec <- capex_vec * (1 + capx_infl) ^ yrs_idx

  } else {
    # Minimal fallback when no leases are provided
    noi_vec[1] <- 1
    noi_vec    <- noi_vec  * (1 + idx_rate)  ^ yrs_idx
    opex_vec   <- opex_vec * (1 + opex_infl) ^ yrs_idx
    capex_vec  <- capex_vec * (1 + capx_infl)^ yrs_idx
  }

  # 4) Acquisition pricing base 

  acq_ht <- if (!is.null(cfg$acq_price_ht)) {
    cfg$acq_price_ht
  } else {
    if (!is.finite(noi_price_base) || noi_price_base <= 0) {
      stop(
        "Computed theoretical first-year NOI for pricing is non-positive. ",
        "Provide `acq_price_ht` explicitly in the preset."
      )
    }
    noi_price_base / (cfg$entry_yield %||% stop("entry_yield is required"))
  }

  acq_di <- acq_ht * (1 + (cfg$acq_cost_rate %||% 0))

  # For convenience downstream, initial debt / equity amounts
  debt_init   <- acq_di * ltv_init
  equity_init <- acq_di - debt_init

  # 5) Debt flags passed downstream 

  maturity <- as.integer(cfg$horizon_years)
  type     <- if ((cfg$extra_amort_pct %||% 0) > 0) "amort" else "bullet"

  exit_cost <- if (!is.null(cfg$exit_transaction_costs)) {
    sum(unlist(cfg$exit_transaction_costs), na.rm = TRUE)
  } else {
    cfg$exit_cost %||% 0
  }

  #  6) Return normalized bundle -

  list(
    disc_rate           = disc_rate,
    disc_detail         = disc_detail,
    exit_yield          = exit_yield,
    exit_cost           = exit_cost,
    acq_price_ht        = acq_ht,
    acq_price_di        = acq_di,
    ltv_init            = ltv_init,
    debt_init           = debt_init,
    equity_init         = equity_init,
    rate_annual         = cfg$rate_annual,
    maturity            = maturity,
    type                = type,
    arrangement_fee_pct = cfg$arrangement_fee_pct %||% 0,
    capitalized_fees    = isTRUE(cfg$capitalized_fees),
    noi_vec             = noi_vec,
    opex_vec            = opex_vec,
    capex_vec           = capex_vec
  )
}



#' Read a configuration YAML
#' @param config_file path; default to inst/extdata/config.yml in the package.
#' @return list
#' @export
dcf_read_config <- function(
    config_file = system.file("extdata", "preset_default.yml", package = "cre.dcf")
) {
  checkmate::assert(
    "File must be readable",
    checkmate::check_file_exists(config_file),
    checkmate::check_true(isOpen(file(config_file)))
  )

  if (!file.exists(config_file)) {
    stop("Configuration file not found: ", config_file)
  }
  yaml::read_yaml(config_file)
}


#' Canonical pipeline from a YAML file
#' @param config_file path to YAML.
#' @param ltv_base "price_ht" | "price_di" | "value".
#' @return list(dcf, debt, full, ratios, norm)
#' @export
run_from_config <- function(config_file, ltv_base = c("price_ht", "price_di", "value")) {
  cfg  <- dcf_read_config(config_file)
  norm <- cfg_normalize(cfg)
  ltv_base <- match.arg(ltv_base)

  acq_price <- switch(ltv_base,
                      "price_ht" = norm$acq_price_ht,
                      "price_di" = norm$acq_price_di,
                      "value"    = norm$noi_vec[1] / (cfg$entry_yield %||% 0.05)
  )

  dcf <- dcf_calculate(
    acq_price         = acq_price,
    entry_yield       = cfg$entry_yield,
    exit_yield        = norm$exit_yield,
    horizon_years     = length(norm$noi_vec),
    disc_rate         = norm$disc_rate,
    exit_cost         = norm$exit_cost,
    capex             = norm$capex_vec,
    opex              = norm$opex_vec,
    noi                = norm$noi_vec
  )

  principal <- acq_price * norm$ltv_init
  debt <- debt_built_schedule(
    principal           = principal,
    rate_annual         = norm$rate_annual,
    maturity            = norm$maturity,
    type                = norm$type,
    extra_amort_pct     = get_cfg(cfg, "extra_amort_pct", default = 0),
    arrangement_fee_pct = norm$arrangement_fee_pct
  )

  full <- cf_make_full_table(dcf, debt)
  full <- dcf_add_noi_columns(full)

  rat <- add_credit_ratios(full, debt,
                           exit_yield = norm$exit_yield,
                           dscr_basis = "noi",           # pas de capex ponctuels
                           ignore_balloon_in_min = TRUE, # si souhaité pour le reporting
                           maturity_year = norm$maturity)

  list(dcf = dcf, debt = debt, full = full, ratios = rat, norm = norm)
}

#' Serialize a validated configuration list to YAML
#'
#' @description
#' Validates a configuration list against the package grammar using
#' \code{cfg_validate()} and serializes it to a YAML file on disk.
#' This helper is intended for reproducibility and interoperability,
#' allowing a fully specified in-memory configuration to be persisted
#' and reused in subsequent runs or edited manually by users.
#'
#' @param config List specification following the package configuration grammar
#'   (typically created with \code{dcf_spec_template()} and possibly modified).
#' @param path Character scalar. Output file path where the YAML file is written
#'   (for example \code{"case.yml"}).
#'
#' @return
#' The input \code{path}, returned invisibly, to allow use in pipelines.
#'
#' @details
#' The function performs validation before writing to disk. If validation
#' fails, an error is raised and no file is written. The YAML output is a
#' direct serialization of the validated configuration list and therefore
#' preserves all fields, including nested structures.
#'
#' @examples
#' tmp <- tempfile(fileext = ".yml")
#' cfg <- dcf_spec_template()
#' cfg$entry_yield <- 0.06
#' as_yaml(cfg, tmp)
#' stopifnot(file.exists(tmp))
#'
#' @importFrom yaml write_yaml
#' @export
as_yaml <- function(config, path) {
  cfg_validate(config)
  yaml::write_yaml(config, path)
  invisible(path)
}

#' Report missing or inconsistent fields in a config list
#'
#' Runs lightweight checks aligned with \code{cfg_validate()} and returns a table
#' of issues, if any. This is a convenience wrapper for user-facing diagnostics;
#' it does not replace \code{cfg_validate()}.
#'
#' @param config List configuration to inspect.
#' @return A tibble with columns \code{field}, \code{problem}, \code{hint}, or an
#'   empty tibble if no issues are detected.
#'
#' @examples
#' tib <- cfg_missing(list())
#' tib
#'
#' @export
cfg_missing <- function(config) {
  checkmate::assert_list(config)
  issues <- list()
  add <- function(field, problem, hint = NULL) {
    issues[[length(issues) + 1L]] <<- tibble::tibble(
      field = field, problem = problem, hint = hint %||% ""
    )
  }

  # Required top-level keys (per cfg_validate)
  req <- c(
    "purchase_year","horizon_years","index_rate","entry_yield",
    "acq_cost_rate","exit_yield_spread_bps","exit_cost",
    "disc_method","ltv_init","rate_annual",
    "extra_amort_pct","scr_ratio","opex_sqm"
  )
  for (k in req) if (is.null(config[[k]])) add(k, "missing", "Provide a numeric value.")

  # Discount method specific
  dm <- config$disc_method
  if (!is.null(dm)) {
    if (dm == "wacc") {
      w <- config$disc_rate_wacc
      if (is.null(w) || is.null(w$KE) || is.null(w$KD)) {
        add("disc_rate_wacc", "missing", "Provide KE and KD in [0,1].")
      }
    } else if (dm == "wacc_capm") {
      cr <- config$disc_rate_wacc_capm
      needed <- c("risk_free","beta","KD")
      if (is.null(cr) || any(vapply(needed, function(nm) is.null(cr[[nm]]), logical(1)))) {
        add("disc_rate_wacc_capm", "missing", "Provide risk_free, beta, KD (and mkt_return or mrp).")
      }
    } else if (dm == "risk_premium") {
      rp <- config$disc_rate_risk_premium
      if (is.null(rp) || is.null(rp$rf)) {
        add("disc_rate_risk_premium", "missing", "Provide rf and any premia.")
      }
    } else if (dm == "yield_plus_growth") {
      yg <- config$disc_rate_yield_plus_growth
      if (is.null(yg) || is.null(yg$property_yield) || is.null(yg$growth)) {
        add("disc_rate_yield_plus_growth", "missing", "Provide property_yield and growth.")
      }
    }
  }

  if (length(issues) == 0L) {
    return(tibble::tibble(field = character(), problem = character(), hint = character()))
  }
  dplyr::bind_rows(issues)
}

#' Serialize a validated configuration list to YAML
#'
#' Validates \code{config} and writes it to \code{path} as \code{'YAML'}.
#'
#' @param config List specification following the package grammar.
#' @param path Output file path (for example \code{"case.yml"}).
#' @return The input \code{path}, invisibly.
#' @importFrom yaml write_yaml
#'
#' @examples
#' cfg <- dcf_spec_template()
#' cfg$entry_yield <- 0.06
#' tmp <- tempfile(fileext = ".yml")
#' as_yaml(cfg, tmp)
#' stopifnot(file.exists(tmp))
#' unlink(tmp)
#'
#' @export
as_yaml <- function(config, path) {
  cfg_validate(config)
  yaml::write_yaml(config, path)
  invisible(path)
}


#' Explain effective parameters after normalization
#'
#' Produces a compact tibble that reports selected effective inputs used by the
#' engine after validation and normalization (see \code{cfg_normalize()}).
#'
#' @param config List configuration (not a file path).
#' @return A tibble with selected effective parameters and derived values.
#'
#' @examples
#' cfg <- dcf_spec_template()
#' cfg$acq_price_ht <- 1e6
#' ex <- cfg_explain(cfg)
#' str(ex)
#'
#' @export
cfg_explain <- function(config) {
  cfg_validate(config)
  norm <- cfg_normalize(config)
  tibble::tibble(
    param = c("disc_rate","exit_yield","exit_cost",
              "acq_price_ht","acq_price_di","ltv_init","rate_annual",
              "maturity","debt_type","arrangement_fee_pct","capitalized_fees",
              "noi_y1","opex_y1","capex_y1"),
    value = c(
      norm$disc_rate, norm$exit_yield, norm$exit_cost,
      norm$acq_price_ht, norm$acq_price_di, norm$ltv_init, norm$rate_annual,
      norm$maturity, (norm$type %||% NA_character_), norm$arrangement_fee_pct,
      isTRUE(norm$capitalized_fees),
      (norm$noi_vec %||% numeric(1))[1],
      (norm$opex_vec %||% numeric(1))[1],
      (norm$capex_vec %||% numeric(1))[1]
    )
  )

}
