#' Robust internal rate of return (adaptive bracketing)
#'
#' @description
#' Computes a real IRR from a vector of dated cash flows \eqn{t = 0, \dots, T}.
#' The algorithm first searches for a root in an initial interval
#' \code{[lower, upper]}. If this interval does not \emph{bracket} a root
#' (that is, if the net present value function does not change sign),
#' the upper bound is expanded multiplicatively up to \code{max_upper}.
#'
#' If the cash-flow series exhibits no sign change (all flows are
#' \code{>= 0} or all \code{<= 0}), or if no root can be bracketed after
#' expansion, the function silently returns \code{NA_real_} (optionally
#' with a warning if \code{warn = TRUE}).
#'
#' @param cf Numeric. Vector of cash flows \eqn{t = 0, \dots, T}.
#' @param lower,upper Initial search bounds for the IRR (decimal rates).
#' @param max_upper Maximum upper bound when automatically expanding the
#'   bracketing interval.
#' @param tol Numerical tolerance passed to \code{\link[stats]{uniroot}}.
#' @param warn Logical. If \code{TRUE}, emits a warning when the IRR cannot be
#'   computed (no sign change or failure of bracketing).
#'
#' @return A numeric scalar (decimal rate) corresponding to the IRR, or
#'   \code{NA_real_} if the IRR is not defined or could not be located
#'   numerically.
#'
#' @examples
#' irr_safe(c(-100, 60, 60))   # IRR defined
#' irr_safe(c(-100, -20, -5))  # no sign change -> NA
#'
#' @export
irr_safe <- function(cf,
                     lower = -0.9999,
                     upper = 0.10,
                     max_upper = 1e4,
                     tol = sqrt(.Machine$double.eps),
                     warn = FALSE) {
  # Basic checks
  if (!is.numeric(cf) || length(cf) < 2L) {
    if (warn) warning("irr_safe(): cash-flow vector is non-numeric or too short, returning NA.")
    return(NA_real_)
  }
  if (anyNA(cf)) {
    if (warn) warning("irr_safe(): cash-flow vector contains NA, returning NA.")
    return(NA_real_)
  }

  # 1) Existence condition for a real IRR: sign change
  has_pos <- any(cf > 0)
  has_neg <- any(cf < 0)

  if (!(has_pos && has_neg)) {
    # Typical case: equity fully destroyed or never invested
    if (warn) {
      warning("irr_safe(): no sign change in cash flows, IRR not defined (returning NA).")
    }
    return(NA_real_)
  }

  # 2) Net present value (NPV) function associated with the cash flows
  pv <- function(r) {
    t <- seq_along(cf) - 1L
    sum(cf / (1 + r)^t)
  }

  pv_lower <- pv(lower)
  pv_upper <- pv(upper)

  # 3) If the initial interval already brackets a root
  if (pv_lower * pv_upper < 0) {
    return(stats::uniroot(pv, c(lower, upper), tol = tol)$root)
  }

  # 4) Progressive expansion of the upper bound
  while (pv_lower * pv_upper > 0 && upper < max_upper) {
    upper    <- upper * 2
    pv_upper <- pv(upper)
  }

  # 5) Final bracketing attempt after expansion
  if (pv_lower * pv_upper < 0) {
    return(stats::uniroot(pv, c(lower, upper), tol = tol)$root)
  }

  # 6) Failure: optionally document, then return NA
  if (warn) {
    warning(
      sprintf(
        "irr_safe(): unable to bracket a root in [%.4f, %.4f]; returning NA.",
        lower, upper
      )
    )
  }

  NA_real_
}


#' Net present value at constant rate
#' @description NPV of `cf` evaluated at `times` (default 0..T).
#' @param cf numeric. Cash flows.
#' @param rate numeric(1). Discount rate (decimal).
#' @param times integer. Time indices (same length as `cf`).
#' @return numeric(1) NPV.
#' @export
npv_rate <- function(cf, rate, times = seq_along(cf) - 1L) {
  stopifnot(length(cf) == length(times))
  sum(cf / (1 + rate)^times)
}


#' Rate conversion (decimal vs bps)
#' @param dec numeric(1). Decimal rate.
#' @param bps numeric(1). Basis points.
#' @return numeric(1) as decimal.
#' @export
as_rate <- function(dec = NULL, bps = NULL) {
  if (!is.null(dec)) {
    return(as.numeric(dec))
  }
  if (!is.null(bps)) {
    return(as.numeric(bps) / 10000)
  }
  0
}


#' Guardrail on an input rate (message if scale likely incorrect)
#' @param x numeric(1).
#' @param name character(1). Parameter name used in messages.
#' @return numeric(1) unchanged.
#' @export
guard_rate <- function(x, name) {
  if (!is.finite(x)) stop(sprintf("%s is missing or non-numeric", name))
  if (x > 1.5) {
    warning(sprintf(
      "%s=%.4f appears to be in unconverted bps; decimal conversion expected",
      name, x
    ))
  }
  x
}


#' Monetary rounding to 2 decimals
#' @keywords internal
rnd <- function(x) round(x, 2)


#' Or-or helper
#' @name or-or
#' @aliases %||%
#' @keywords internal
NULL

#' @rdname or-or
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a


# helper used internally (in ratios.R)
#' @keywords internal
safe_div <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok  <- is.finite(num) & is.finite(den) & (abs(den) > 0)
  out[ok] <- num[ok] / den[ok]
  out
}


#' Summarise a financing scenario from ratios and metrics
#'
#' Builds a one-line summary for a given scenario using:
#' - levered / unlevered metrics (IRR / NPV);
#' - credit ratios computed by \code{add_credit_ratios()}.
#'
#' ## Canonical rules (maturity = horizon_years)
#'
#' *Maturity is now strictly equal to the investment horizon,
#' without implicit fallbacks such as min(N, 5).*
#'
#' As a consequence:
#' - \strong{min DSCR} takes the value of the attribute
#'   \code{min_dscr_pre_maturity} if available (set by
#'   `add_credit_ratios(ignore_balloon_in_min = TRUE)`).
#' - Otherwise, \strong{min DSCR} is the minimum over years 1 ... maturity-1
#'   (all operational years before the balloon repayment).
#' - \strong{max forward LTV} is the maximum over years 1 ... maturity,
#'   always explicitly excluding t = 0.
#'
#' @param tag      Character(1). Scenario name ("all_equity", "debt_bullet",
#'   "debt_amort", ...).
#' @param lev_obj  List. Metrics object (contains irr_equity, npv_equity,
#'   irr_project, npv_project).
#' @param rat_tbl  data.frame. Ratios table (contains year, dscr, ltv_forward).
#' @param maturity Integer(1). Maturity year (must correspond to horizon_years).
#'
#' @return Tibble with columns:
#'   scenario, irr_equity, npv_equity, irr_project, npv_project,
#'   min_dscr, max_ltv_forward, ops_share, tv_share.
#'
#' @keywords internal
summarize_case <- function(tag, lev_obj, rat_tbl, maturity) {

  # Robust defensive checks 
  if (is.null(rat_tbl)) {
    stop("summarize_case(): 'rat_tbl' is NULL; expected a data.frame of ratios.")
  }

  if (is.atomic(rat_tbl)) {
    stop("summarize_case(): 'rat_tbl' is atomic (", typeof(rat_tbl),
         "); expected a data.frame. Check add_credit_ratios() output.")
  }

  if (!inherits(rat_tbl, "data.frame")) {
    rat_tbl <- try(tibble::as_tibble(rat_tbl), silent = TRUE)
    if (inherits(rat_tbl, "try-error") || !inherits(rat_tbl, "data.frame")) {
      stop("summarize_case(): cannot coerce 'rat_tbl' to a data.frame.")
    }
  }

  required_cols <- c("year", "dscr", "ltv_forward")
  missing_cols  <- setdiff(required_cols, names(rat_tbl))
  if (length(missing_cols) > 0) {
    stop("summarize_case(): 'rat_tbl' is missing required columns: ",
         paste(missing_cols, collapse = ", "))
  }

  if (!is.finite(maturity)) {
    stop("summarize_case(): 'maturity' must be a finite integer.")
  }

  # 1) Determination of min DSCR -
  # Prefer attribute set by add_credit_ratios()
  min_dscr_val <- attr(rat_tbl, "min_dscr_pre_maturity")

  # Otherwise: minimum over 1 ... maturity-1
  if (is.null(min_dscr_val) || !is.finite(min_dscr_val)) {
    inside <- (rat_tbl$year >= 1) & (rat_tbl$year < maturity)
    suppressWarnings(
      min_dscr_val <- min(rat_tbl$dscr[inside], na.rm = TRUE)
    )
    if (!is.finite(min_dscr_val)) min_dscr_val <- NA_real_
  }

  # 2) Determination of max forward LTV 
  # Full life of the debt: years 1 ... maturity
  inside_ltv <- (rat_tbl$year >= 1) & (rat_tbl$year <= maturity)

  suppressWarnings(
    max_ltv <- max(rat_tbl$ltv_forward[inside_ltv], na.rm = TRUE)
  )
  if (!is.finite(max_ltv)) max_ltv <- NA_real_

  # 3) Build summary row 
  tibble::tibble(
    scenario        = tag,
    irr_equity      = lev_obj$irr_equity,
    npv_equity      = lev_obj$npv_equity,
    irr_project     = lev_obj$irr_project,
    npv_project     = lev_obj$npv_project,
    min_dscr        = min_dscr_val,
    max_ltv_forward = max_ltv,
    ops_share       = lev_obj$ops_share %||% NA_real_,
    tv_share        = lev_obj$tv_share %||% NA_real_
  )
}

#' Derive an exit yield from an entry yield and a spread (bps)
#'
#' @param entry_yield numeric(1) >= 0. Entry cap-rate in decimal form.
#' @param spread_bps numeric(1). Spread in basis points (may be negative).
#'
#' @return numeric(1) Exit yield in decimal form.
#'
#' @examples
#' derive_exit_yield(0.055, 50) # 0.060
#'
#' @export
#' @importFrom checkmate assert_number
derive_exit_yield <- function(entry_yield, spread_bps) {
  checkmate::assert_number(entry_yield, lower = 0, finite = TRUE)
  checkmate::assert_number(spread_bps, finite = TRUE)
  entry_yield + spread_bps / 10000
}

#' Safely compute the equity multiple for an equity cash-flow series
#'
#' This helper computes the equity multiple as total distributions divided by
#' total contributions in absolute value.
#'
#' @param cf_equity Numeric vector of equity cash-flows over time.
#' @return A single numeric scalar giving the equity multiple.
#' @importFrom checkmate assert_numeric
#' @keywords internal
#' @export
#'
#' @examples
#' equity_multiple_safe(c(-100, 10, 10, 110))
#' equity_multiple_safe(c(-100, 30, 70))
#'
#' \donttest{
#' err <- try(equity_multiple_safe(c(-100, -20)), silent = TRUE)
#' stopifnot(inherits(err, "try-error"))
#' }
equity_multiple_safe <- function(cf_equity) {
  checkmate::assert_numeric(cf_equity, any.missing = FALSE)

  neg <- cf_equity[cf_equity < 0]
  pos <- cf_equity[cf_equity > 0]

  if (length(neg) == 0L || length(pos) == 0L) {
    stop(
      "Need at least one strictly negative and one strictly positive equity ",
      "cash-flow to compute the equity multiple."
    )
  }

  contrib <- sum(neg)
  distrib <- sum(pos)

  distrib / abs(contrib)
}


#' Compute the style-by-style manifest for preset scenarios
#'
#' This helper runs the four preset style scenarios
#' (\code{"core"}, \code{"core_plus"}, \code{"value_added"}, \code{"opportunistic"})
#' through [[run_case()]] and extracts a compact set of indicators that are
#' useful for both investors and lenders:
#'
#' - project IRR (all-equity),
#' - equity IRR (levered),
#' - minimum DSCR under a bullet structure,
#' - initial LTV at origination under a bullet structure,
#' - maximum forward LTV under a bullet structure,
#' - equity NPV.
#'
#' The result is a tibble that can be reused in vignettes and automated tests
#' to check that the presets preserve the intended risk-return and
#' leverage-coverage hierarchies. The initial LTV is the structural leverage
#' choice at origination. By contrast, \code{ltv_max_fwd} is a conditional
#' stress indicator computed along the simulated business plan; for transitional
#' or lease-up strategies it may therefore be non-monotonic across styles even
#' when the overall risk ordering remains economically coherent.
#'
#' @param styles Character vector of style names to include.
#'   Defaults to the four preset scenarios:
#'   \code{c("core", "core_plus", "value_added", "opportunistic")}.
#'
#' @return A tibble with one row per style and the columns:
#'   \code{style}, \code{class}, \code{irr_project}, \code{irr_equity},
#'   \code{dscr_min_bul}, \code{ltv_init}, \code{ltv_max_fwd},
#'   \code{ops_share}, \code{tv_share}, and \code{npv_equity}.
#' @export
styles_manifest <- function(
    styles = c("core", "core_plus", "value_added", "opportunistic")
) {

  ext_dir <- system.file("extdata", package = "cre.dcf")

  if (!nzchar(ext_dir)) {
    stop("inst/extdata not found. Load the package with devtools::load_all() or install it.")
  }

  # Only keep styles with an existing preset file
  present <- styles[file.exists(file.path(ext_dir, paste0("preset_", styles, ".yml")))]
  if (length(present) == 0L) {
    stop("No expected presets found in inst/extdata. Aborting styles_manifest().")
  }

  purrr::map_dfr(
    present,
    function(s) {
      # 1) Load preset and get structural initial LTV
      cfg <- load_style_preset(s)

      ltv_init <- cfg$ltv_init %||% 0
      if (!is.finite(ltv_init) || ltv_init < 0 || ltv_init > 1) {
        stop(
          "styles_manifest(): `ltv_init` must be in [0, 1] in preset '",
          s, "'. Got: ", ltv_init
        )
      }

      stab_year <- cfg$stabilization_year %||% 1L

      # 2) Run case through engine
      case <- run_case(cfg)

      sum_tbl <- case[["comparison"]][["summary"]]
      row_bul <- sum_tbl[sum_tbl$scenario == "debt_bullet", , drop = FALSE]

      if (nrow(row_bul) != 1L) {
        stop(
          "styles_manifest(): expected exactly one 'debt_bullet' row in comparison$summary ",
          "for style '", s, "'."
        )
      }

      # 3) Stabilized DSCR: min DSCR from stabilization_year onward,
      #    excluding the balloon year (maturity) where DSCR is mechanically low.
      horizon <- cfg$horizon_years %||% 10L
      ratios <- case$comparison$details$debt_bullet$ratios
      dscr_stab <- NA_real_
      if (!is.null(ratios) && "dscr" %in% names(ratios) && "year" %in% names(ratios)) {
        stab_dscr_vec <- ratios$dscr[ratios$year >= stab_year & ratios$year < horizon]
        stab_dscr_vec <- stab_dscr_vec[is.finite(stab_dscr_vec)]
        if (length(stab_dscr_vec) > 0L) {
          dscr_stab <- min(stab_dscr_vec)
        }
      }

      tibble::tibble(
        style              = s,
        class              = cfg$class %||% NA_character_,
        irr_project        = row_bul$irr_project,
        irr_equity         = row_bul$irr_equity,
        dscr_min_bul       = row_bul$min_dscr,
        dscr_min_stabilized = dscr_stab,
        stabilization_year = as.integer(stab_year),
        ltv_init           = ltv_init,
        ltv_max_fwd        = row_bul$max_ltv_forward,
        ops_share          = row_bul$ops_share,
        tv_share           = row_bul$tv_share,
        npv_equity         = row_bul$npv_equity
      )
    }
  )
}



#' Load a preset style YAML file
#'
#' This loader reads preset YAML files from `inst/extdata` inside the installed
#' package.
#'
#' @keywords internal
load_style_preset <- function(style) {

  ext_dir <- system.file("extdata", package = "cre.dcf")

  if (!nzchar(ext_dir)) {
    stop("Could not locate inst/extdata. The package must be loaded with devtools::load_all() or installed.")
  }

  path <- file.path(ext_dir, paste0("preset_", style, ".yml"))

  if (!file.exists(path)) {
    stop("Preset not found: ", path)
  }

  yaml::read_yaml(path)
}


#' Extract bullet credit-ratio path for a style preset
#'
#' For a given style, this helper:
#' - loads the preset YAML via load_style_preset(),
#' - runs the case through run_case(),
#' - extracts the 'ratios' table for the bullet-debt scenario.
#'
#' The output is a tibble with the original ratio columns plus a
#' 'style' column identifying the preset.
#'
#' @param style Character scalar, e.g. "core" or "opportunistic".
#'
#' @return A tibble containing at least columns:
#'   year, dscr, ltv_forward, style.
#' @keywords internal
style_bullet_ratios <- function(style) {
  cfg  <- load_style_preset(style)
  case <- run_case(cfg)

  ratios <- case$comparison$details$debt_bullet$ratios

  if (is.null(ratios)) {
    stop("style_bullet_ratios(): no 'debt_bullet$ratios' table found for style: ", style)
  }

  ratios$style <- style
  tibble::as_tibble(ratios)
}

#' Count covenant breaches by style under the bullet-debt scenario
#'
#' This helper aggregates, for a set of styles, the number of periods in which
#' bullet-debt credit metrics breach simple covenant guardrails:
#' - DSCR < `min_dscr_guard`,
#' - forward LTV > `max_ltv_guard`.
#'
#' It relies on [style_bullet_ratios()], which is expected to return, for each
#' style, a tibble of yearly ratios in the bullet-debt scenario with at least
#' the columns: `style`, `year`, `dscr`, `ltv_forward`.
#'
#' @param styles Character vector of style names (e.g. `"core"`, `"core_plus"`,
#'   `"value_added"`, `"opportunistic"`). The output `style` factor will follow
#'   this ordering.
#' @param min_dscr_guard Numeric scalar, DSCR guardrail below which a period is
#'   counted as a DSCR breach.
#' @param max_ltv_guard Numeric scalar, forward-LTV guardrail above which a
#'   period is counted as an LTV breach.
#'
#' @return A tibble with one row per style and the columns:
#'   - `style` (factor, levels = `styles`),
#'   - `n_dscr_breach`: number of years with `dscr < min_dscr_guard`,
#'   - `n_ltv_breach`:  number of years with `ltv_forward > max_ltv_guard`.
#'   Year 0 is excluded from the counts.
#' @export
styles_breach_counts <- function(
    styles         = c("core", "core_plus", "value_added", "opportunistic"),
    min_dscr_guard = 1.20,
    max_ltv_guard  = 0.65
) {
  # Gather bullet-debt ratios for each style -
  ratios <- purrr::map_dfr(styles, style_bullet_ratios)

  # Minimal structural check: we need these columns
  needed <- c("style", "year", "dscr", "ltv_forward")
  missing <- setdiff(needed, names(ratios))
  if (length(missing) > 0L) {
    stop(
      "styles_breach_counts(): missing required column(s) in ratios: ",
      paste(missing, collapse = ", ")
    )
  }

  ratios |>
    # Ignore t0: breaches are only meaningful from year >= 1
    dplyr::filter(.data$year >= 1) |>
    dplyr::mutate(
      breach_dscr = .data$dscr        <  min_dscr_guard,
      breach_ltv  = .data$ltv_forward >  max_ltv_guard
    ) |>
    dplyr::group_by(.data$style) |>
    dplyr::summarise(
      n_dscr_breach = sum(breach_dscr, na.rm = TRUE),
      n_ltv_breach  = sum(breach_ltv,  na.rm = TRUE),
      .groups       = "drop"
    ) |>
    dplyr::mutate(
      style = factor(.data$style, levels = styles)
    ) |>
    dplyr::arrange(.data$style)
}


#' Re-evaluate styles under a yield-plus-growth discounting rule
#'
#' This helper re-runs a set of preset styles under a simplified
#' \code{"yield_plus_growth"} discounting convention, leaving all cash-flow
#' assumptions unchanged. It is primarily used in vignettes and tests to check
#' that the qualitative ordering of styles (in terms of equity IRR and NPV) is
#' robust to the choice of discounting scheme.
#'
#' For each style, the function:
#' \enumerate{
#'   \item loads the corresponding YAML preset file;
#'   \item overrides \code{disc_method <- "yield_plus_growth"};
#'   \item sets \code{disc_rate_yield_plus_growth} so that the property yield
#'         equals \code{entry_yield} and the growth component equals
#'         \code{index_rate};
#'   \item calls [[run_case()]] and extracts the leveraged equity IRR and NPV.
#' }
#'
#' @param styles Character vector of style identifiers, e.g.
#'   \code{c("core", "core_plus", "value_added", "opportunistic")}.
#' @param config_dir Directory from which preset YAML files are loaded.
#'   Defaults to the package \code{inst/extdata} folder.
#'
#' @return A tibble with one row per style and the columns:
#'   \itemize{
#'     \item \code{style} (character),
#'     \item \code{irr_equity_y}: leveraged equity IRR under the
#'           \code{"yield_plus_growth"} convention,
#'     \item \code{npv_equity_y}: leveraged equity NPV under the same
#'           convention.
#'   }
#' @export
styles_revalue_yield_plus_growth <- function(
    styles,
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_revalue_yield_plus_growth(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(
    styles,
    function(style_tag) {
      path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
      if (!file.exists(path)) {
        stop(
          "styles_revalue_yield_plus_growth(): preset not found for style '",
          style_tag, "' at: ", path
        )
      }

      cfg <- yaml::read_yaml(path)

      # Switch to a simple property-yield + growth rule
      cfg$disc_method <- "yield_plus_growth"
      cfg$disc_rate_yield_plus_growth <- list(
        property_yield = cfg$entry_yield,
        growth         = cfg$index_rate
      )

      out <- run_case(cfg)

      tibble::tibble(
        style        = style_tag,
        irr_equity_y = out$leveraged$irr_equity,
        npv_equity_y = out$leveraged$npv_equity
      )
    }
  )
}


#' Extract leveraged equity cash flows by style
#'
#' This helper loads a set of preset styles from YAML, runs each configuration
#' through [[run_case()]] under the leveraged (debt) scenario, and extracts the
#' yearly equity cash flows. It is primarily used in vignettes and tests to
#' document the time profile of equity outflows and inflows by style.
#'
#' For each style, the function:
#' \enumerate{
#'   \item reads \code{preset_<style>.yml} from \code{config_dir};
#'   \item calls [[run_case()]] and accesses \code{out$leveraged$cashflows};
#'   \item returns the pair \code{(year, equity_cf)} with a style label.
#' }
#'
#' The sign convention follows [[compute_leveraged_metrics()]]:
#' the initial equity outlay at \eqn{t = 0} is negative, subsequent net equity
#' distributions are positive when cash is returned to equity.
#'
#' @param styles Character vector of style identifiers, e.g.
#'   \code{c("core", "core_plus", "value_added", "opportunistic")}.
#' @param config_dir Directory from which preset YAML files are loaded.
#'   Defaults to the package \code{inst/extdata} folder.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{style} (character),
#'     \item \code{year} (integer),
#'     \item \code{equity_cf} (numeric), the leveraged equity cash flow in year
#'           \code{year}.
#'   }
#' @export
styles_equity_cashflows <- function(
    styles,
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_equity_cashflows(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(
    styles,
    function(style_tag) {
      path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
      if (!file.exists(path)) {
        stop(
          "styles_equity_cashflows(): preset not found for style '",
          style_tag, "' at: ", path
        )
      }

      cfg <- yaml::read_yaml(path)
      out <- run_case(cfg)

      lev_cf <- out$leveraged$cashflows

      if (!all(c("year", "equity_cf") %in% names(lev_cf))) {
        stop(
          "styles_equity_cashflows(): `leveraged$cashflows` is missing ",
          "`year` or `equity_cf` for style '", style_tag, "'."
        )
      }

      tibble::tibble(
        style     = style_tag,
        year      = lev_cf$year,
        equity_cf = lev_cf$equity_cf
      )
    }
  )
}
#' Present-value split between income and resale by style
#'
#' For each style preset, this helper:
#' - runs [run_case()] under the all-equity scenario,
#' - takes the cash-flow table used for the unlevered DCF,
#' - discounts positive cash inflows at the DCF discount rate,
#' - decomposes the resulting present value into:
#'     * income  = free cash flow excluding resale proceeds,
#'     * resale  = terminal sale proceeds.
#'
#' Year 0 (initial outlay) is excluded from the income/resale split so that
#' shares remain numerically stable and interpretable.
#'
#' @param styles Character vector of style identifiers.
#' @param config_dir Directory where preset YAML files are stored.
#' @return A tibble with columns:
#'   style, pv_income, pv_resale, share_pv_income, share_pv_resale.
#' @export
styles_pv_split <- function(
    styles,
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_pv_split(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(styles, function(style_tag) {
    path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
    if (!file.exists(path)) {
      stop("styles_pv_split(): preset not found for style '", style_tag, "'.")
    }

    cfg <- yaml::read_yaml(path)
    out <- run_case(cfg)

    cf <- out$all_equity$cashflows

    # Income component = free cash flow excluding resale
    income_cf <- cf$free_cash_flow - cf$sale_proceeds
    resale_cf <- cf$sale_proceeds

    # Use the DCF discount factors already computed (disc_rate, not IRR)
    df_disc <- cf$discount_factor

    # Exclude year 0 from the split (initial outlay)
    idx_pos <- which(cf$year >= 1L)

    pv_income <- sum(income_cf[idx_pos] / df_disc[idx_pos], na.rm = TRUE)
    pv_resale <- sum(resale_cf[idx_pos] / df_disc[idx_pos], na.rm = TRUE)

    pv_total  <- pv_income + pv_resale
    if (!is.finite(pv_total) || pv_total <= 0) {
      stop("styles_pv_split(): non-positive total PV for style '", style_tag, "'.")
    }

    tibble::tibble(
      style            = style_tag,
      pv_income        = pv_income,
      pv_resale        = pv_resale,
      share_pv_income  = pv_income  / pv_total,
      share_pv_resale  = pv_resale  / pv_total
    )
  })
}


#' Exit-yield sensitivity of leveraged equity IRR by style
#'
#' For each style, this helper:
#' - loads the corresponding YAML preset,
#' - perturbs the `exit_yield_spread_bps` parameter by a grid of deltas,
#' - reruns [run_case()] for each perturbation,
#' - collects the leveraged equity IRR.
#'
#' Economically, this approximates how sensitive each style's equity IRR is to
#' small shifts in the exit_yield, and therefore to terminal_value
#' risk. Strategies that concentrate value creation at exit (e.g. value_added,
#' opportunistic) should display stronger IRR reactions to a given shock.
#'
#' @param styles Character vector of style identifiers
#'   (e.g. `"core"`, `"core_plus"`, `"value_added"`, `"opportunistic"`).
#' @param delta_bps Numeric vector of exit-yield spread shocks in basis points,
#'   applied additively to the `exit_yield_spread_bps` field of each preset.
#' @param config_dir Directory where preset YAML files are stored.
#'   Defaults to the package's `inst/extdata` folder.
#'
#' @return A tibble with columns:
#'   * `style` (character),
#'   * `shock_bps` (numeric, the applied spread shock),
#'   * `irr_equity` (numeric, leveraged equity IRR under the shock).
#' @export
styles_exit_sensitivity <- function(
    styles,
    delta_bps  = c(-50, 0, 50),
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_exit_sensitivity(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(styles, function(style_tag) {
    path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
    if (!file.exists(path)) {
      stop("styles_exit_sensitivity(): preset not found for style '", style_tag, "'.")
    }

    base_cfg <- yaml::read_yaml(path)

    purrr::map_dfr(delta_bps, function(dbps) {
      cfg <- base_cfg
      cfg$exit_yield_spread_bps <- (cfg$exit_yield_spread_bps %||% 0) + dbps

      out <- run_case(cfg)

      tibble::tibble(
        style      = style_tag,
        shock_bps  = dbps,
        irr_equity = out$leveraged$irr_equity
      )
    })
  })
}

#' Rental-growth (indexation) sensitivity of leveraged equity IRR by style
#'
#' This helper perturbs the global `index_rate` parameter of each style preset
#' by a given grid of additive shocks and recomputes the leveraged equity IRR.
#'
#' It therefore measures how dependent each style is on rental growth
#' (via indexation and lease renewals) to reach its target equity IRR.
#' In typical preset calibrations, core strategies tend to be less sensitive
#' than value_added or opportunistic profiles, which rely more heavily on
#' growth and lease-up.
#'
#' @param styles Character vector of style identifiers.
#' @param delta Numeric vector of rental-growth shocks (additive) applied to
#'   the `index_rate` parameter of the preset.
#' @param config_dir Directory where preset YAML files are stored.
#'
#' @return A tibble with columns:
#'   * `style` (character),
#'   * `shock_growth` (numeric, growth shock added to `index_rate`),
#'   * `irr_equity` (numeric, leveraged equity IRR under the shock).
#' @export
styles_growth_sensitivity <- function(
    styles,
    delta      = c(-0.01, 0, 0.01),
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_growth_sensitivity(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(styles, function(style_tag) {
    path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
    if (!file.exists(path)) {
      stop("styles_growth_sensitivity(): preset not found for style '", style_tag, "'.")
    }

    base_cfg <- yaml::read_yaml(path)

    purrr::map_dfr(delta, function(dg) {
      cfg <- base_cfg
      cfg$index_rate <- (cfg$index_rate %||% 0) + dg

      out <- run_case(cfg)

      tibble::tibble(
        style        = style_tag,
        shock_growth = dg,
        irr_equity   = out$leveraged$irr_equity
      )
    })
  })
}

#' Break-even exit yield for a target leveraged equity IRR, by style
#'
#' For each style, this helper solves (via [uniroot()]) for the exit yield
#' that delivers a specified target leveraged equity IRR, holding all other
#' assumptions of the preset constant.
#'
#' It proceeds by:
#' - reading the YAML preset,
#' - defining a root-finding function that, for a candidate absolute exit
#'   yield, adjusts `exit_yield_spread_bps` accordingly,
#' - calling [run_case()] and returning the difference between the resulting
#'   equity IRR and `target_irr`,
#' - bracketing the root over a user-specified interval.
#'
#' The lower the break-even exit yield, the tighter the exit pricing
#' assumption needed to reach the hurdle, and the more the style depends on
#' favourable market conditions at sale.
#'
#' @param styles Character vector of style identifiers.
#' @param target_irr Numeric, target leveraged equity IRR to hit (in decimal).
#' @param interval Numeric vector of length 2 giving the bracketing interval
#'   for the absolute exit yield (e.g. `c(0.03, 0.10)` for 3%–10%).
#' @param config_dir Directory where preset YAML files are stored.
#'
#' @return A tibble with columns:
#'   * `style` (character),
#'   * `target_irr` (numeric),
#'   * `be_exit_yield` (numeric, break-even exit yield in decimal, or `NA`
#'     if no root was found in `interval`).
#' @export
styles_break_even_exit_yield <- function(
    styles,
    target_irr,
    interval   = c(0.03, 0.10),
    config_dir = system.file("extdata", package = "cre.dcf")
) {
  if (!nzchar(config_dir)) {
    stop("styles_break_even_exit_yield(): `config_dir` is empty or invalid.")
  }

  purrr::map_dfr(styles, function(style_tag) {
    path <- file.path(config_dir, paste0("preset_", style_tag, ".yml"))
    if (!file.exists(path)) {
      stop("styles_break_even_exit_yield(): preset not found for style '", style_tag, "'.")
    }

    base_cfg <- yaml::read_yaml(path)

    f <- function(exit_y) {
      cfg <- base_cfg
      # reconstruct spread from absolute exit_y and entry_yield
      entry_y <- cfg$entry_yield %||% stop("entry_yield missing in preset.")
      cfg$exit_yield_spread_bps <- 1e4 * (exit_y - entry_y)

      out <- run_case(cfg)
      out$leveraged$irr_equity - target_irr
    }

    root <- try(uniroot(f, interval = interval), silent = TRUE)

    be_yield <- if (inherits(root, "try-error")) NA_real_ else root$root

    tibble::tibble(
      style         = style_tag,
      target_irr    = target_irr,
      be_exit_yield = be_yield
    )
  })
}

#' Distressed exit summary across CRE investment styles
#'
#' This helper applies a simple lender-driven distressed-exit rule to a set of
#' preset style scenarios. For each style and covenant regime, it:
#'   1. Runs the baseline case via [run_case()].
#'   2. Identifies the first covenant breach under the bullet-debt scenario
#'      (DSCR and forward LTV).
#'   3. Optionally shifts very early breaches to a minimum refinancing year
#'      (refinancing window logic).
#'   4. Re-runs the case with a shortened horizon and a fire-sale exit-yield
#'      penalty, and extracts:
#'        - distressed equity IRR (possibly NA),
#'        - distressed equity multiple and loss percentage,
#'        - distressed sale value.
#'
#' @param styles Character vector of style tags, e.g.
#'   `c("core", "core_plus", "value_added", "opportunistic")`.
#' @param regimes A data frame or tibble with at least three columns:
#'   `regime` (label), `min_dscr` (numeric), `max_ltv` (numeric).
#'   Each row defines a covenant regime (strict / baseline / flexible, etc.).
#' @param fire_sale_bps Numeric scalar. Widening (in basis points) applied to
#'   the exit-yield spread in the distressed run (e.g. `+100` for +100 bps).
#' @param refi_min_year Integer scalar. Minimum year at which a lender-driven
#'   distressed exit can occur. If a breach is detected before this year and
#'   `allow_year1_distress = FALSE`, the distressed exit is moved to
#'   `refi_min_year`.
#' @param allow_year1_distress Logical. If `TRUE`, distress can occur in year 1.
#'   If `FALSE`, breaches in years `< refi_min_year` are shifted to
#'   `refi_min_year` (refinancing window logic).
#' @param underwriting_mode Character scalar. Either `"transition"` or
#'   `"stabilized"`. In `"transition"` mode (default), covenant testing starts
#'   at `stabilization_year` when the preset defines one, which is useful for
#'   lease-up or refurbishment business plans. In `"stabilized"` mode, covenant
#'   testing starts in year 1, which is more conservative and closer to a
#'   standard stabilized-income loan reading.
#' @param exit_shock_bps Numeric scalar. Additive shock (in basis points)
#'   applied to the preset's `exit_yield_spread_bps` before running the case
#'   and detecting breaches. This simulates a market repricing environment.
#'   Default `0` (no shock).
#' @param growth_shock Numeric scalar. Additive shock applied to the preset's
#'   `index_rate` before running the case. This simulates a rental growth
#'   slowdown. Default `0` (no shock).
#' @param ext_dir Optional directory where style presets (YAML) are stored.
#'   Defaults to the package `inst/extdata` folder.
#'
#' @return A tibble with one row per combination of style and regime, and the
#'   columns:
#'   - `style`, `regime`, `min_dscr`, `max_ltv`,
#'   - `underwriting_mode`, `covenant_start_year`,
#'   - `breach_year`, `breach_type`,
#'   - `irr_equity_base`, `irr_equity_distress`,
#'   - `distress_undefined` (logical),
#'   - `equity_multiple_base`, `equity_multiple_distress`,
#'   - `equity_loss_pct_base`, `equity_loss_pct_distress`,
#'   - `sale_value_distress`.
#' @export
styles_distressed_exit <- function(
    styles,
    regimes,
    fire_sale_bps      = 100,
    refi_min_year      = 3L,
    allow_year1_distress = TRUE,
    underwriting_mode  = c("transition", "stabilized"),
    exit_shock_bps     = 0,
    growth_shock       = 0,
    ext_dir            = system.file("extdata", package = "cre.dcf")
) {
  underwriting_mode <- match.arg(underwriting_mode)

  if (!nzchar(ext_dir)) {
    stop("inst/extdata not found. Load the package with devtools::load_all() or install it.")
  }

  if (!all(c("regime", "min_dscr", "max_ltv") %in% names(regimes))) {
    stop("regimes must contain columns: 'regime', 'min_dscr', 'max_ltv'.")
  }

  # Small internal helper: first breach (DSCR or forward LTV) under bullet
  # stab_year: earliest year from which breaches are meaningful (skips ramp-up)
  find_first_breach <- function(case_obj, min_dscr, max_ltv, stab_year = 1L) {
    ratios <- case_obj$comparison$details$debt_bullet$ratios |>
      dplyr::filter(.data$year >= stab_year)

    breach_dscr_idx <- which(ratios$dscr < min_dscr)
    breach_ltv_idx  <- which(ratios$ltv_forward > max_ltv)

    if (length(breach_dscr_idx) == 0L && length(breach_ltv_idx) == 0L) {
      return(list(year = NA_integer_, type = NA_character_))
    }

    idx_all <- c(breach_dscr_idx, breach_ltv_idx)
    i_min   <- min(idx_all)

    type <- dplyr::case_when(
      i_min %in% breach_dscr_idx & i_min %in% breach_ltv_idx ~ "both",
      i_min %in% breach_dscr_idx                             ~ "dscr",
      i_min %in% breach_ltv_idx                              ~ "ltv",
      TRUE                                                   ~ NA_character_
    )

    list(year = ratios$year[i_min], type = type)
  }

  # Internal helper: equity multiple and loss percentage from an equity CF path
  compute_equity_multiple <- function(eq_cf) {
    if (all(eq_cf == 0)) return(list(multiple = NA_real_, loss_pct = NA_real_))

    paid_in   <- -sum(eq_cf[eq_cf < 0], na.rm = TRUE)
    returned  <-  sum(eq_cf[eq_cf > 0], na.rm = TRUE)

    if (paid_in <= 0) {
      return(list(multiple = NA_real_, loss_pct = NA_real_))
    }

    multiple <- returned / paid_in
    loss_pct <- multiple - 1

    list(multiple = multiple, loss_pct = loss_pct)
  }

  # Cartesian product of styles and regimes
  grid <- tidyr::expand_grid(
    style  = styles,
    regimes
  )

  purrr::pmap_dfr(
    grid,
    function(style, regime, min_dscr, max_ltv) {
      # 1) Load base config and run base case -
      cfg_path <- file.path(ext_dir, paste0("preset_", style, ".yml"))
      if (!file.exists(cfg_path)) {
        stop("Preset not found for style '", style, "': ", cfg_path)
      }

      cfg_base <- yaml::read_yaml(cfg_path)
      stab_year <- cfg_base$stabilization_year %||% 1L
      covenant_start_year <- if (identical(underwriting_mode, "transition")) {
        stab_year
      } else {
        1L
      }

      # Apply market shock to the base configuration before breach detection.
      # This ensures that breaches are identified under stressed conditions,
      # not just under the unstressed baseline.
      if (exit_shock_bps != 0) {
        cfg_base$exit_yield_spread_bps <- (cfg_base$exit_yield_spread_bps %||% 0) + exit_shock_bps
      }
      if (growth_shock != 0) {
        cfg_base$index_rate <- max(0, (cfg_base$index_rate %||% 0) + growth_shock)
      }

      case_base <- run_case(cfg_base)

      # Base equity IRR and CFs (under market shock, before fire-sale)
      irr_base <- case_base$leveraged$irr_equity
      cf_base  <- case_base$leveraged$cashflows
      eq_cf_b  <- cf_base$equity_cf

      mult_base <- compute_equity_multiple(eq_cf_b)

      # Base sale value (for reference only)
      cf_proj   <- case_base$all_equity$cashflows
      sale_base <- dplyr::last(cf_proj$sale_proceeds)

      # 2) Identify first breach under bullet (from stabilization onward) -
      br <- find_first_breach(case_base, min_dscr = min_dscr, max_ltv = max_ltv,
                              stab_year = covenant_start_year)
      breach_year <- br$year
      breach_type <- br$type

      # If no breach, no distressed exit: return baseline only
      if (is.na(breach_year)) {
        return(
          tibble::tibble(
            style                   = style,
            underwriting_mode       = underwriting_mode,
            covenant_start_year     = covenant_start_year,
            breach_year             = NA_integer_,
            breach_type             = NA_character_,
            irr_equity_base         = irr_base,
            irr_equity_distress     = NA_real_,
            distress_undefined      = FALSE,
            regime                  = regime,
            min_dscr                = min_dscr,
            max_ltv                 = max_ltv,
            equity_multiple_base    = mult_base$multiple,
            equity_multiple_distress = NA_real_,
            equity_loss_pct_base    = mult_base$loss_pct,
            equity_loss_pct_distress = NA_real_,
            sale_value_distress     = NA_real_
          )
        )
      }

      # 3) Refinancing window: shift very early breaches if required 
      distress_year <- breach_year
      if (!allow_year1_distress && breach_year < refi_min_year) {
        distress_year <- refi_min_year
      }

      # Guardrail: if distress_year exceeds original horizon, treat as no breach
      horizon <- cfg_base$horizon_years %||% nrow(cf_proj) - 1L
      if (!is.null(horizon) && distress_year > horizon) {
        return(
          tibble::tibble(
            style                   = style,
            underwriting_mode       = underwriting_mode,
            covenant_start_year     = covenant_start_year,
            breach_year             = NA_integer_,
            breach_type             = NA_character_,
            irr_equity_base         = irr_base,
            irr_equity_distress     = NA_real_,
            distress_undefined      = FALSE,
            regime                  = regime,
            min_dscr                = min_dscr,
            max_ltv                 = max_ltv,
            equity_multiple_base    = mult_base$multiple,
            equity_multiple_distress = NA_real_,
            equity_loss_pct_base    = mult_base$loss_pct,
            equity_loss_pct_distress = NA_real_,
            sale_value_distress     = NA_real_
          )
        )
      }

      # 4) Build distressed config: truncated horizon + fire-sale penalty 
      cfg_dist <- cfg_base
      cfg_dist$horizon_years         <- distress_year
      cfg_dist$exit_yield_spread_bps <- cfg_dist$exit_yield_spread_bps + fire_sale_bps

      # Try to run the distressed case. If IRR cannot be computed, we keep NA.
      case_dist <- try(run_case(cfg_dist), silent = TRUE)

      if (inherits(case_dist, "try-error")) {
        # Validation or IRR problem: IRR undefined, but loss metrics can be
        # approximated ex post if needed. For now, flag as undefined.
        return(
          tibble::tibble(
            style                   = style,
            underwriting_mode       = underwriting_mode,
            covenant_start_year     = covenant_start_year,
            breach_year             = distress_year,
            breach_type             = breach_type,
            irr_equity_base         = irr_base,
            irr_equity_distress     = NA_real_,
            distress_undefined      = TRUE,
            regime                  = regime,
            min_dscr                = min_dscr,
            max_ltv                 = max_ltv,
            equity_multiple_base    = mult_base$multiple,
            equity_multiple_distress = NA_real_,
            equity_loss_pct_base    = mult_base$loss_pct,
            equity_loss_pct_distress = NA_real_,
            sale_value_distress     = NA_real_
          )
        )
      }

      irr_dist <- case_dist$leveraged$irr_equity

      # Distressed equity CFs and multiple
      cf_dist  <- case_dist$leveraged$cashflows
      eq_cf_d  <- cf_dist$equity_cf
      mult_dist <- compute_equity_multiple(eq_cf_d)

      # Distressed sale value (last-period sale proceeds under fire-sale)
      cf_proj_d <- case_dist$all_equity$cashflows
      sale_dist <- dplyr::last(cf_proj_d$sale_proceeds)

      tibble::tibble(
        style                    = style,
        underwriting_mode        = underwriting_mode,
        covenant_start_year      = covenant_start_year,
        breach_year              = distress_year,
        breach_type              = breach_type,
        irr_equity_base          = irr_base,
        irr_equity_distress      = irr_dist,
        distress_undefined       = is.na(irr_dist),
        regime                   = regime,
        min_dscr                 = min_dscr,
        max_ltv                  = max_ltv,
        equity_multiple_base     = mult_base$multiple,
        equity_multiple_distress = mult_dist$multiple,
        equity_loss_pct_base     = mult_base$loss_pct,
        equity_loss_pct_distress = mult_dist$loss_pct,
        sale_value_distress      = sale_dist
      )
    }
  )
}

# Internal: truncate leases in a config at a given model year 
# cutoff_year is the model year index (1..N) at which the asset is sold.
# purchase_year is the calendar base year used in the YAML (e.g. 2025).
truncate_leases_at <- function(cfg, cutoff_year) {
  if (is.null(cfg$leases) || length(cfg$leases) == 0L) {
    return(cfg)
  }

  # Calendar year corresponding to the last cash-flow year
  cut_year_cal <- cfg$purchase_year + cutoff_year - 1L

  cfg$leases <- lapply(cfg$leases, function(u) {
    if (is.null(u$events) || length(u$events) == 0L) {
      return(u)
    }

    evs_trunc <- purrr::map(u$events, function(ev) {
      # Drop events that start strictly after the cut-off year
      if (!is.null(ev$start) && ev$start > cut_year_cal) {
        return(NULL)
      }

      # If an event extends beyond the cut-off, cap its end
      if (!is.null(ev$end) && ev$end > cut_year_cal) {
        ev$end <- cut_year_cal
      }

      ev
    })

    # Remove NULL events that have been dropped
    u$events <- Filter(Negate(is.null), evs_trunc)
    u
  })

  cfg
}

#' Write a commented YAML template for users to edit
#'
#' Creates a \code{'YAML'} file on disk from \code{dcf_spec_template()},
#' suitable for manual editing.
#'
#' @param path File path where to write the \code{'YAML'} file
#'   (for example \code{"my_case.yml"}).
#' @return The input \code{path}, invisibly.
#' @importFrom yaml write_yaml
#'
#' @examples
#' tmp <- tempfile(fileext = ".yml")
#' dcf_write_yaml_template(tmp)
#' stopifnot(file.exists(tmp))
#' unlink(tmp)
#'
#' @export
dcf_write_yaml_template <- function(path) {
  cfg <- dcf_spec_template()
  yaml::write_yaml(cfg, path)
  invisible(path)
}
