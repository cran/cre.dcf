
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
#' @param debt_type Optional debt schedule type to use (\code{"bullet"} or
#'   \code{"amort"}). When \code{NULL} (default), the normalized type inferred
#'   from the configuration is used. A non-\code{NULL} value overrides it.
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
                     debt_type = NULL,
                     ltv_base  = c("price_di", "price_ht", "value")) {
  prepared <- .engine_prepare_case(
    config = config,
    config_file = config_file,
    debt_type = debt_type,
    ltv_base = ltv_base
  )
  executed <- .engine_execute_case(prepared, include_comparison = TRUE)

  # 7) Pricing breakdown (HT / costs / DI)
  price_ht <- prepared$norm$acq_price_ht
  price_di <- prepared$norm$acq_price_di
  acq_cost <- price_di - price_ht

  list(
    pricing    = list(price_ht = price_ht, acq_cost = acq_cost, price_di = price_di),
    all_equity = executed$all_equity,
    leveraged  = executed$leveraged,
    comparison = executed$comparison,
    cashflows  = executed$ratios,
    config     = list(
      ltv_base            = prepared$ltv_base,   # "price_di" / "price_ht" / "value"
      debt_type           = prepared$debt_type_eff,
      ltv_init            = prepared$norm$ltv_init,
      debt_init           = prepared$financing$debt_init,
      equity_init         = prepared$financing$equity_invest,
      capitalized_fees    = prepared$financing$capitalized_fees,
      arrangement_fee_pct = prepared$fee_pct,
      disc_method         = prepared$config$disc_method,
      disc_rate           = prepared$norm$disc_rate,
      disc_detail         = prepared$norm$disc_detail
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



#' Normalize YAML into Discounted Cash Flow (DCF) and debt parameters
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
  market_inputs <- .engine_normalize_capital_market_inputs(cfg)
  operations <- .engine_project_operations(cfg)
  pricing <- .engine_resolve_pricing(
    cfg = cfg,
    operations = operations,
    market_inputs = market_inputs
  )

  .engine_flatten_normalized_case(
    market_inputs = market_inputs,
    operations = operations,
    pricing = pricing
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
  prepared <- .engine_prepare_case(
    config_file = config_file,
    ltv_base = ltv_base
  )
  executed <- .engine_execute_case(prepared, include_comparison = FALSE)

  list(
    dcf = executed$dcf,
    debt = executed$debt,
    full = executed$full,
    ratios = executed$ratios,
    norm = prepared$norm
  )
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
#' of issues, if any. This is a convenience wrapper for user-facing checks;
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
