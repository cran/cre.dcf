#' Apply scenario shocks to a set of  Discounted Cash Flow (DCF) assumptions
#'
#' @description
#' Applies additive shifts (rates and yields in decimal form) or proportional
#' scalings (NOI, CAPEX) to a list of parameters. Preserves field names.
#'
#' @param cfg list. Base assumptions (e.g. those passed to [dcf_calculate()]).
#'   Fields read if present: `disc_rate`, `exit_yield`, `entry_yield`, `capex`,
#'   `index_rent`, `vacancy`.
#' @param deltas list. Supported keywords:
#'   - `d_rate` (additive on `disc_rate`, decimal),
#'   - `d_exit_yield` (additive on `exit_yield`, decimal),
#'   - `d_noi` (multiplicative on `entry_yield`, i.e. on year-1 net operating income `NOI_y1`),
#'   - `d_capex` (multiplicative on `capex`),
#'   - `d_index` (multiplicative on `index_rent`),
#'   - `d_vacancy` (multiplicative on `vacancy`).
#'
#' @return list `cfg_choc` with the same structure as `cfg`.
#' @export
simulate_shock <- function(cfg, deltas = list()) {
  stopifnot(is.list(cfg), is.list(deltas))

  clamp01 <- function(x) {
    x[!is.na(x) & x < 0]  <- 0
    x[!is.na(x) & x >= 1] <- (1 - .Machine$double.eps)
    x
  }
  scale_if <- function(x, d) if (is.null(d)) x else x * (1 + d)

  # additive shocks (decimals)
  if (!is.null(deltas$d_rate)) {
    cfg$disc_rate <- (cfg$disc_rate %||% NA_real_) + deltas$d_rate
  }
  if (!is.null(deltas$d_exit_yield)) {
    cfg$exit_yield <- (cfg$exit_yield %||% NA_real_) + deltas$d_exit_yield
  }

  # multiplicative shocks
  if (!is.null(deltas$d_noi)) {
    cfg$entry_yield <- scale_if(cfg$entry_yield %||% NA_real_, deltas$d_noi)
  }
  if (!is.null(deltas$d_capex)) {
    cfg$capex <- scale_if(cfg$capex %||% 0, deltas$d_capex)
  }
  if (!is.null(deltas$d_index)) {
    cfg$index_rent <- scale_if(cfg$index_rent %||% 0, deltas$d_index)
  }
  if (!is.null(deltas$d_vacancy)) {
    cfg$vacancy <- scale_if(cfg$vacancy %||% 0, deltas$d_vacancy)
  }

  # guards for [0,1) domains
  if (!is.null(cfg$disc_rate))   cfg$disc_rate   <- clamp01(cfg$disc_rate)
  if (!is.null(cfg$exit_yield))  cfg$exit_yield  <- clamp01(cfg$exit_yield)
  if (!is.null(cfg$entry_yield)) cfg$entry_yield <- clamp01(cfg$entry_yield)
  if (!is.null(cfg$index_rent))  cfg$index_rent  <- clamp01(cfg$index_rent)
  if (!is.null(cfg$vacancy))     cfg$vacancy     <- clamp01(cfg$vacancy)

  cfg
}


#' Test the feasibility of a refinancing at year T (interest-only diagnostic)
#'
#' @description
#' Assesses at \(T\) the simultaneous feasibility of DSCR and forward LTV
#' covenants assuming an interest-only payment at \(T+1\). This diagnostic
#' isolates covenant feasibility from the precise structure of the new loan.
#'
#' @param full data.frame. Merged table (0..N) from [cf_make_full_table()],
#'   containing at least: `year`, `net_operating_income`, `outstanding_debt`.
#' @param year_T integer(1). Evaluation year \(T\) (0..N).
#' @param covenants list. Thresholds: `dscr_min` (default 1.25),
#'   `ltv_max` (default 0.65).
#' @param new_rate numeric(1). New annual nominal rate (decimal).
#' @param new_exit_yield numeric(1). New exit yield (decimal) for forward value.
#'   `NOI_{T+1}` is missing (default 0 if not provided as an attribute of `full`
#'   or in the DCF inputs).
#'
#' @return list with `status` ("ok"/"fail"), `reasons` (character) and `snapshot` (tibble).
#' @export
#' @importFrom checkmate assert_data_frame assert_number
#' @importFrom dplyr lag lead
#' @importFrom tibble tibble
test_refi <- function(full, year_T, covenants, new_rate, new_exit_yield) {
  checkmate::assert_data_frame(full, min.rows = 2)
  if (!("year" %in% names(full))) stop("full$year is missing.")
  checkmate::assert_number(year_T, lower = 0, finite = TRUE)
  checkmate::assert_number(new_rate, lower = 0)
  checkmate::assert_number(new_exit_yield, lower = .Machine$double.eps)

  idx <- match(year_T, full$year)
  if (is.na(idx)) stop("year_T is not present in the table.")

  # opening balance at T (before payment)
  out_open_T <- dplyr::lag(full$outstanding_debt)[idx]
  if (is.na(out_open_T)) out_open_T <- full$outstanding_debt[idx]

  # NOI_{T+1}: lead if available, otherwise use NOI_T
  noi_T1 <- dplyr::lead(full$net_operating_income)[idx]
  if (is.na(noi_T1)) {
    noi_T1 <- full$net_operating_income[idx]
  }

  # diagnostic payment (interest-only)
  payment_T1   <- out_open_T * new_rate
  dscr_T1      <- noi_T1 / pmax(payment_T1, 1e-9)
  value_forward <- noi_T1 / new_exit_yield
  ltv_T1       <- out_open_T / pmax(value_forward, 1e-9)

  # thresholds
  dmin <- covenants$dscr_min %||% 1.25
  lmax <- covenants$ltv_max %||% 0.65

  reasons <- character(0)
  if (!is.na(dscr_T1) && dscr_T1 < dmin) {
    reasons <- c(reasons, "Insufficient DSCR at T+1")
  }
  if (!is.na(ltv_T1) && ltv_T1 > lmax) {
    reasons <- c(reasons, "Excessive forward LTV at T+1")
  }

  status <- if (length(reasons) == 0L && is.finite(dscr_T1) && is.finite(ltv_T1)) "ok" else "fail"

  list(
    status  = status,
    reasons = reasons,
    snapshot = tibble::tibble(
      year_T        = year_T,
      out_T         = out_open_T,
      noi_T1        = noi_T1,
      payment_T1    = payment_T1,
      dscr_T1       = dscr_T1,
      value_forward = value_forward,
      ltv_T1        = ltv_T1,
      new_rate      = new_rate,
      new_exit_yield = new_exit_yield
    )
  )
}


#' Sensitivity grid (rate / exit yield) and monotonicity of ratios
#'
#' @description
#' For each (`rate`, `exit_yield`) pair, builds a bullet schedule, merges it
#' with [dcf_calculate()] cash flows, computes ratios via [add_credit_ratios()],
#' and returns `min_dscr` (t >= 1) and `max_ltv_forward` (t >= 1).
#'
#' @param dcf_res list. Output of [dcf_calculate()].
#' @param rate_grid numeric. Grid of annual nominal rates (decimal).
#' @param exit_yield_grid numeric. Grid of `exit_yield` values (decimal).
#' @param ltv numeric(1). Initial LTV (default 0.60).
#' @param maturity integer(1). Maturity (years) of the bullet schedule.
#'
#' @return tibble with columns `rate`, `exit_yield`, `min_dscr`, `max_ltv_forward`.
#' @export
#' @importFrom checkmate assert_numeric assert_integerish
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
sweep_sensitivities <- function(dcf_res,
                                rate_grid,
                                exit_yield_grid,
                                ltv = 0.60,
                                maturity = 5L) {
  checkmate::assert_numeric(rate_grid, lower = 0, upper = 1,
                            min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(exit_yield_grid, lower = 0, upper = 1,
                            min.len = 1, any.missing = FALSE)
  checkmate::assert_integerish(maturity, lower = 1, len = 1)

  cf   <- dcf_res$cashflows
  acq  <- cf$acquisition_price[1] %||% (-cf$free_cash_flow[1])
  principal <- ltv * acq

  out_rate <- lapply(rate_grid, function(r) {
    sch  <- debt_built_schedule(principal, r, maturity = maturity, type = "bullet")
    base <- cf_make_full_table(dcf_res, sch)
    out_y <- lapply(exit_yield_grid, function(ey) {
      rat <- add_credit_ratios(base, sch, exit_yield = ey)
      tibble::tibble(
        rate            = r,
        exit_yield      = ey,
        min_dscr        = suppressWarnings(min(rat$dscr[rat$year >= 1], na.rm = TRUE)),
        max_ltv_forward = suppressWarnings(max(rat$ltv_forward[rat$year >= 1], na.rm = TRUE))
      )
    })
    dplyr::bind_rows(out_y)
  })

  dplyr::bind_rows(out_rate)
}
