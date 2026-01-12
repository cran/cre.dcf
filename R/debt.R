#' Initial debt fees (arrangement fee)
#'
#' @param loan_draw_0 Initial loan drawdown amount (before any possible capitalization of fees).
#' @param arrangement_fee_pct Arrangement fee rate (0–1).
#' @param capitalized Logical: TRUE = fee is capitalized into the loan principal,
#'   FALSE = fee is paid in cash.
#'
#' @return A list: amount (numeric), capitalized (logical).
#' @export
init_debt_fees <- function(loan_draw_0, arrangement_fee_pct = 0, capitalized = TRUE) {
  fee <- (arrangement_fee_pct %||% 0) * loan_draw_0
  list(amount = fee, capitalized = isTRUE(capitalized))
}


#' Compute equity invested at t0 (acquisition costs already included in acq_price)
#'
#' @param acq_price All-in acquisition price (basis for financing).
#' @param ltv_init Initial LTV (0–1).
#' @param arrangement_fee_pct Arrangement fee rate (0–1).
#' @param capitalized_fees TRUE if fees are capitalized into the loan principal.
#'
#' @return A list with: equity_0, loan_draw_0, fees_init, fees_cap.
#' @export
compute_equity_invest <- function(acq_price,
                                  ltv_init,
                                  arrangement_fee_pct = 0,
                                  capitalized_fees = TRUE) {
  stopifnot(
    is.finite(acq_price), acq_price > 0,
    is.finite(ltv_init), ltv_init >= 0, ltv_init <= 1
  )

  loan0 <- ltv_init * acq_price
  fees  <- init_debt_fees(loan0, arrangement_fee_pct, capitalized = capitalized_fees)

  loan_draw_0 <- if (fees$capitalized) loan0 + fees$amount else loan0
  equity_0    <- acq_price - loan_draw_0

  list(
    equity_0    = equity_0,
    loan_draw_0 = loan_draw_0,
    fees_init   = fees$amount,
    fees_cap    = fees$capitalized
  )
}

#' Debt schedule for bullet and amortising loans
#'
#' Creates an annual schedule indexed from \code{0..maturity} with an initial
#' draw at \code{year = 0}, interest, amortisation, total payment, and end-of-year
#' outstanding balance. The convention is no payment at \code{year = 0}. For both
#' loan types, the outstanding principal is 0 at maturity up to rounding.
#'
#' @param principal Numeric scalar. Amount borrowed at \code{year = 0} (greater than or equal to 0).
#' @param rate_annual Numeric scalar in \code{[0, 1]}. Annual nominal interest rate.
#' @param maturity Integer scalar greater than or equal to 1. Duration in years; returned years are \code{0..maturity}.
#' @param type Character scalar. Either \code{"amort"} (constant payment) or \code{"bullet"}.
#' @param extra_amort_pct Numeric scalar in \code{[0, 1]}. Additional annual amortisation rate (used only for \code{"bullet"}).
#' @param arrangement_fee_pct Numeric scalar in \code{[0, 1]}. Arrangement fee rate applied to \code{principal}.
#'
#' @return A tibble with columns \code{year}, \code{debt_draw}, \code{interest}, \code{amortization},
#' \code{payment}, \code{arrangement_fee}, \code{outstanding_debt}, and \code{loan_init}.
#'
#' @examples
#' sch_b <- debt_built_schedule(6e6, 0.045, maturity = 5, type = "bullet")
#' sch_a <- debt_built_schedule(6e6, 0.045, maturity = 5, type = "amort")
#' sch_b
#' sch_a
#'
#' @export
debt_built_schedule <- function(
  principal,
  rate_annual,
  maturity,
  type = c("amort", "bullet"),
  extra_amort_pct = 0,
  arrangement_fee_pct = 0
) {
  checkmate::assert_numeric(principal, lower = 0, finite = TRUE, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(rate_annual, lower = 0, upper = 1, finite = TRUE, any.missing = FALSE, len = 1)
  checkmate::assert_integerish(maturity, lower = 1, any.missing = FALSE, len = 1)
  checkmate::assert_choice(type, choices = c("amort", "bullet"))
  checkmate::assert_numeric(extra_amort_pct, lower = 0, upper = 1, len = 1)
  checkmate::assert_numeric(arrangement_fee_pct, lower = 0, upper = 1, len = 1)

  type <- match.arg(type)

  years <- 0:maturity
  nP <- maturity

  debt_draw        <- numeric(nP + 1)
  arrangement_fee  <- numeric(nP + 1)
  interest         <- numeric(nP + 1)
  amortization     <- numeric(nP + 1)
  payment          <- numeric(nP + 1)
  outstanding_debt <- numeric(nP + 1)

  # t = 0: initial drawdown
  debt_draw[1] <- rnd(principal)
  outstanding_debt[1] <- debt_draw[1]

  # arrangement fee at t = 1 (if not capitalized elsewhere)
  arrangement_fee[2] <- rnd(principal * arrangement_fee_pct)

  balance <- principal

  if (type == "amort") {
    # constant instalment; special case rate = 0
    pmt_const <- if (rate_annual == 0) {
      if (nP > 0) principal / nP else principal
    } else {
      principal * rate_annual / (1 - (1 + rate_annual)^(-nP))
    }

    for (t in 1:nP) {
      int_t  <- balance * rate_annual
      prin_t <- pmt_const - int_t
      if (t == nP) prin_t <- balance  # final payoff
      pay_t  <- int_t + prin_t

      interest[t + 1]     <- int_t
      amortization[t + 1] <- prin_t
      payment[t + 1]      <- pay_t

      balance <- balance - prin_t
      outstanding_debt[t + 1] <- balance
    }

  } else { # bullet
    extra_amt <- principal * extra_amort_pct

    for (t in 1:nP) {
      int_t  <- balance * rate_annual
      prin_t <- if (t < nP) min(extra_amt, balance) else balance
      pay_t  <- int_t + prin_t

      interest[t + 1]     <- int_t
      amortization[t + 1] <- prin_t
      payment[t + 1]      <- pay_t

      balance <- balance - prin_t
      outstanding_debt[t + 1] <- balance
    }
  }

  # rounding and residual cleanup
  interest         <- rnd(interest)
  amortization     <- rnd(amortization)
  payment          <- rnd(payment)
  outstanding_debt <- rnd(outstanding_debt)

  if (abs(outstanding_debt[length(outstanding_debt)]) < 0.01) {
    outstanding_debt[length(outstanding_debt)] <- 0
  }

  tibble::tibble(
    year             = years,
    debt_draw        = debt_draw,
    interest         = interest,
    amortization     = amortization,
    payment          = payment,
    arrangement_fee  = arrangement_fee,
    outstanding_debt = outstanding_debt,
    loan_init        = rep(rnd(principal), length(years))
  )
}

#' Covenant flags after computing credit ratios
#'
#' Adds logical indicator columns for covenant breaches based on three ratios:
#' debt service coverage ratio (DSCR), forward loan-to-value ratio (LTV), and
#' current debt yield.
#'
#' @param cf A data.frame or tibble containing at least \code{dscr},
#'   \code{ltv_forward}, and \code{debt_yield_current}.
#' @param cov A list of covenant thresholds. Supported elements include:
#'   \itemize{
#'     \item \code{dscr_min} numeric, default 1.25,
#'     \item \code{ltv_max} numeric in \code{[0, 1]}, default 0.65,
#'     \item \code{debt_yield_min} numeric, default 0.08.
#'   }
#'
#' @return The input table \code{cf} enriched with logical columns
#'   \code{cov_dscr_breach}, \code{cov_ltv_breach}, and \code{cov_dy_breach}.
#'
#' @examples
#' cf <- tibble::tibble(
#'   year = 1:3,
#'   dscr = c(1.40, 1.10, NA),
#'   ltv_forward = c(0.60, 0.70, 0.64),
#'   debt_yield_current = c(0.09, 0.07, 0.08)
#' )
#' cov <- list(dscr_min = 1.25, ltv_max = 0.65, debt_yield_min = 0.08)
#' flag_covenants(cf, cov)
#'
#' @export
flag_covenants <- function(cf, cov) {
  checkmate::assert_data_frame(cf, min.rows = 1)
  checkmate::assert_list(cov, any.missing = FALSE)

  # robust defaults
  dscr_min        <- cov$dscr_min %||% 1.25
  ltv_max         <- cov$ltv_max %||% 0.65
  debt_yield_min  <- cov$debt_yield_min %||% cov$dy_min %||% 0.08

  # required columns (create NA if missing to avoid hard failure)
  need <- c("dscr", "ltv_forward", "debt_yield_current")
  for (nm in need) if (!nm %in% names(cf)) cf[[nm]] <- NA_real_

  dplyr::mutate(
    cf,
    cov_dscr_breach = is.finite(dscr) & (dscr < dscr_min),
    cov_ltv_breach  = is.finite(ltv_forward) & (ltv_forward > ltv_max),
    cov_dy_breach   = is.finite(debt_yield_current) & (debt_yield_current < debt_yield_min)
  )
}
