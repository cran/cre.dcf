#' Add credit ratios for debt service, interest cover, debt yield, and forward loan-to-value
#'
#' Align a project cash-flow table with a debt schedule and compute
#' standard credit ratios for each period:
#' \itemize{
#'   \item debt service coverage ratio (DSCR),
#'   \item interest cover ratio (ICR),
#'   \item initial and current debt yield,
#'   \item forward loan-to-value (LTV) based on next-period NOI.
#' }
#' Optionally, simple covenant flags are added when threshold values
#' are supplied.
#'
#' @param cf_tab A data.frame or tibble of project cash flows over years 0..N,
#'   typically the output of \code{dcf_calculate()} or \code{cf_make_full_table()}.
#'   It must at least contain a \code{year} column and either
#'   \code{net_operating_income} or \code{gei}. When available, the following
#'   columns are used:
#'   \code{opex}, \code{cf_pre_debt}, \code{capex_recur}, \code{leasing_costs},
#'   \code{loan_init}.
#' @param debt_sched A data.frame or tibble representing the debt schedule,
#'   typically the output of \code{debt_built_schedule()}. It must contain
#'   \code{year}, \code{payment}, \code{interest}, and \code{outstanding_debt},
#'   and may also include \code{debt_draw} and \code{loan_init}.
#' @param exit_yield Numeric scalar; exit yield (in decimal form, for example
#'   0.05) used to compute forward values as \code{NOI_next / exit_yield}.
#' @param covenants Optional list with elements \code{dscr_min}, \code{ltv_max}
#'   and/or \code{debt_yield_min}. When supplied, the function adds simple
#'   covenant indicators to the output table.
#' @param dscr_basis Character string specifying the numerator used for DSCR.
#'   One of \code{"noi"}, \code{"gei"} or \code{"cfads"}. The default is
#'   \code{"noi"}.
#' @param cfads_ti_lc Optional object used to construct a CFADS adjustment for
#'   tenant-improvement or leasing-cost allowances. If a list, the element
#'   \code{annual_allowance} (numeric scalar or vector) is subtracted from NOI.
#'   If a function, it is called as \code{cfads_ti_lc(cf_tab)} and the returned
#'   numeric vector is subtracted from NOI.
#' @param ignore_balloon_in_min Logical scalar. If \code{TRUE} and
#'   \code{maturity_year} is not \code{NULL}, the attribute
#'   \code{"min_dscr_pre_maturity"} is attached to the result and stores the
#'   minimum DSCR computed only over years 1 to \code{maturity_year - 1},
#'   ignoring any balloon repayment at maturity.
#' @param maturity_year Optional integer scalar giving the contractual maturity
#'   year of the facility. Periods with \code{year > maturity_year} are treated
#'   as post-maturity (no outstanding debt, no payment, no interest). This
#'   parameter is required when \code{ignore_balloon_in_min = TRUE}.
#'
#' @return A tibble equal to \code{cf_tab} with the following additional
#'   columns:
#'   \itemize{
#'     \item \code{gei}, \code{noi} (created if missing),
#'     \item \code{payment}, \code{interest}, \code{outstanding_debt},
#'     \item \code{noi_fwd}, \code{value_forward},
#'     \item \code{dscr}, \code{interest_cover_ratio},
#'     \item \code{debt_yield_init}, \code{debt_yield_current},
#'     \item \code{ltv_forward},
#'     \item covenant indicators when \code{covenants} is supplied.
#'   }
#'   When \code{ignore_balloon_in_min = TRUE} and \code{maturity_year} is
#'   provided, the object also carries an attribute
#'   \code{"min_dscr_pre_maturity"} containing the minimum DSCR before maturity.
#'
#' @examples
#' cf_tab <- data.frame(
#'   year = 0:3,
#'   gei  = c(0, 120, 123, 126),
#'   opex = c(0, 40, 41, 42),
#'   loan_init = c(2000, NA, NA, NA)
#' )
#'
#' debt_sched <- data.frame(
#'   year = 0:3,
#'   payment = c(0, 150, 150, 2150),
#'   interest = c(0, 100, 95, 90),
#'   outstanding_debt = c(2000, 2000, 1950, 1900),
#'   debt_draw = c(2000, 0, 0, 0)
#' )
#'
#' out <- add_credit_ratios(
#'   cf_tab = cf_tab,
#'   debt_sched = debt_sched,
#'   exit_yield = 0.05,
#'   covenants = list(dscr_min = 1.10, ltv_max = 0.70)
#' )
#'
#' out
#'
#' @export
add_credit_ratios <- function(cf_tab, debt_sched, exit_yield,
                              covenants = NULL,
                              dscr_basis = c("noi","gei","cfads"),
                              cfads_ti_lc = NULL,
                              ignore_balloon_in_min = FALSE,
                              maturity_year = NULL) {

  dscr_basis <- match.arg(dscr_basis)

  # Normalisation des vecteurs dette (positifs)
  idx_match <- match(cf_tab$year, debt_sched$year)

  payment_raw  <- as.numeric(debt_sched$payment)[idx_match]
  interest_raw <- as.numeric(debt_sched$interest)[idx_match]
  out_raw      <- as.numeric(debt_sched$outstanding_debt)[idx_match]

  payment  <- pmax(+payment_raw,  0, na.rm = TRUE)
  interest <- pmax(+interest_raw, 0, na.rm = TRUE)
  out      <- pmax(+out_raw,      0, na.rm = TRUE)

  # Si maturity_year est fourni, expliciter la logique post-maturité
  if (!is.null(maturity_year)) {
    post_mat <- cf_tab$year > maturity_year

    # Après la maturité : plus de dette en encours
    out[post_mat]      <- 0
    payment[post_mat]  <- 0
    interest[post_mat] <- 0
  }

  # Bases de flux
  gei <- cf_tab$gei %||% cf_tab$net_operating_income
  noi <- cf_tab$noi %||% (gei - (cf_tab$opex %||% 0))

  # CFADS optionnel (NOI – allowance TI/LC)
  cfads <- noi
  if (!is.null(cfads_ti_lc)) {
    if (is.list(cfads_ti_lc) && !is.null(cfads_ti_lc$annual_allowance)) {
      cfads <- noi - as.numeric(cfads_ti_lc$annual_allowance)
    } else if (is.function(cfads_ti_lc)) {
      cfads <- noi - as.numeric(cfads_ti_lc(cf_tab))
    }
  }
  base_num <- switch(dscr_basis, noi = noi, gei = gei, cfads = cfads)

  # Valeur forward
  noi_fwd       <- dplyr::lead(noi)
  value_forward <- noi_fwd / exit_yield

  # Ratios
  dscr <- safe_div(base_num, payment)
  icr  <- safe_div(base_num, interest)

  # Pas de ratio en t=0 et là où il n'y a pas de service de dette
  dscr[payment <= 0 | cf_tab$year == 0]  <- NA_real_
  icr[interest <= 0 | cf_tab$year == 0]  <- NA_real_

  # DebtYield_init
  loan_init_vec <- cf_tab$loan_init %||% rep(NA_real_, nrow(cf_tab))
  loan0 <- if (is.finite(loan_init_vec[1])) loan_init_vec[1]
           else sum(pmax(debt_sched$debt_draw, 0), na.rm = TRUE)

  noi_y1 <- if (0 %in% cf_tab$year && 1 %in% cf_tab$year) {
    base_num[match(1, cf_tab$year)]
  } else if (length(base_num) >= 2) {
    base_num[2]
  } else {
    NA_real_
  }

  dy0 <- rep(NA_real_, nrow(cf_tab))
  dy0[1] <- if (is.finite(loan0) && loan0 > 0 && is.finite(noi_y1)) noi_y1 / loan0 else NA_real_

  # DebtYield_current
  dyc <- safe_div(base_num, out)
  dyc[out <= 0 | cf_tab$year == 0] <- NA_real_

  # LTV forward
  ltvf <- safe_div(out, value_forward)

  out_tbl <- dplyr::mutate(
    cf_tab,
    payment = payment, interest = interest, outstanding_debt = out,
    gei = gei, noi = noi,
    noi_fwd = noi_fwd, value_forward = value_forward,
    dscr = dscr, interest_cover_ratio = icr,
    debt_yield_init = dy0, debt_yield_current = dyc,
    ltv_forward = ltvf
  )

  if (!is.null(covenants)) {
    out_tbl <- flag_covenants(out_tbl, list(
      dscr_min       = covenants$dscr_min %||% NA_real_,
      ltv_max        = covenants$ltv_max  %||% NA_real_,
      debt_yield_min = covenants$debt_yield_min %||% NA_real_
    ))
  }

  # Calcul du min DSCR pré-maturité si demandé
  if (isTRUE(ignore_balloon_in_min) && !is.null(maturity_year)) {
    pre_mat <- which(cf_tab$year >= 1 & cf_tab$year < maturity_year)
    min_pre <- suppressWarnings(min(dscr[pre_mat], na.rm = TRUE))
    if (!is.finite(min_pre)) min_pre <- NA_real_
    attr(out_tbl, "min_dscr_pre_maturity") <- min_pre
  }

  out_tbl
}



#' Forward value from next-period NOI
#'
#' Compute a forward-value vector based on next-period NOI and an exit yield.
#' Given a series of annual NOI values, the function constructs a vector
#' NOI can be obtained either from a fixed forward growth rate or from a
#' simple extrapolation of observed growth.
#'
#' @param noi_vec Numeric vector of annual NOI values.
#' @param exit_yield Numeric scalar; exit yield in decimal form (for example
#'   0.05).
#' @param g_forward Optional numeric scalar giving a constant forward growth
#'   rate. When supplied, the last element of \code{NOI_next} is constructed
#'   as the last NOI multiplied by \code{1 + g_forward}. When \code{g_forward}
#'   is \code{NA} (the default), a capped log-growth extrapolation is used
#'   instead.
#'
#' @return A numeric vector of forward values with the same length as
#'   \code{noi_vec}.
#'
#' @export
forward_value_from_noi <- function(noi_vec, exit_yield, g_forward = NA_real_) {
  stopifnot(is.numeric(noi_vec), length(noi_vec) >= 1, exit_yield > 0)
  n <- length(noi_vec)
  if (!is.na(g_forward)) {
    noi_next <- c(noi_vec[-1], tail(noi_vec, 1) * (1 + g_forward))
  } else if (n >= 2) {
    g_hat <- c(diff(log(pmax(noi_vec, 1e-9))), 0)
    g_hat <- pmax(pmin(g_hat, 0.20), -0.20)
    noi_next <- noi_vec * (1 + g_hat)
  } else {
    noi_next <- noi_vec
  }
  noi_next / exit_yield
}

#' Compare three financing structures on a common  Discounted Cash Flow (DCF) base
#'
#' Build and compare three financing setups for a given unlevered DCF:
#' \itemize{
#'   \item an all-equity case,
#'   \item a bullet debt structure,
#'   \item an amortizing debt structure.
#' }
#' All three scenarios share the same acquisition base, interest rate,
#' maturity and target LTV. The function returns a summary table of key
#' investment and credit metrics, together with detailed objects for each
#' scenario.
#'
#' @param dcf_res List; result of \code{dcf_calculate()} for the unlevered
#'   project. It is assumed to contain the cash-flow table and the input
#'   exit yield in \code{dcf_res$inputs$exit_yield}.
#' @param acq_price Numeric scalar; acquisition base consistent with the
#'   pricing convention used in \code{dcf_res} (for example HT, DI or value).
#' @param ltv Numeric scalar in \code{[0, 1)}; target loan-to-value ratio
#'   at origination.
#' @param rate Numeric scalar in \code{[0, 1]}; annual interest rate used
#'   to build the debt schedules.
#' @param maturity Integer scalar greater than or equal to 1; debt maturity
#'   in years.
#' @param covenants Optional list of covenant thresholds, for example
#'   \code{list(dscr_min = 1.25, ltv_max = 0.65)}. These values are passed
#'   to \code{add_credit_ratios()} when computing credit ratios.
#'
#' @return A list with two components:
#'   \item{summary}{A tibble that summarizes, for the all-equity, bullet and
#'     amortizing cases, the main valuation metrics (IRR, NPV) and selected
#'     credit indicators (for example minimum DSCR and maximum forward LTV).}
#'   \item{details}{A named list with one element per scenario. Each element
#'     contains the debt schedule (\code{schedule}), the full joined project
#'     and debt cash-flow table (\code{full}), the credit-ratio table
#'     (\code{ratios}), and the leveraged metrics object (\code{metrics}).}
#'
#' @export
compare_financing_scenarios <- function(dcf_res,
                                        acq_price,
                                        ltv,
                                        rate,
                                        maturity,
                                        covenants = list(dscr_min = 1.25, ltv_max = 0.65)) {
  checkmate::assert_list(dcf_res, any.missing = FALSE)
  checkmate::assert_number(acq_price, lower = 0)
  checkmate::assert_number(ltv, lower = 0, upper = 0.999)
  checkmate::assert_number(rate, lower = 0, upper = 1)
  checkmate::assert_integerish(maturity, lower = 1)

  # 1) all-equity
  unlev <- compute_unleveraged_metrics(dcf_res)

  # helper pour éviter double-comptage des fees
  build_case <- function(type) {
    principal <- ltv * acq_price
    sch <- debt_built_schedule(
      principal = principal, rate_annual = rate,
      maturity = maturity, type = type
      # arrangement_fee_pct si nécessaire
    )
    full <- cf_make_full_table(dcf_res, sch)
    rat  <- add_credit_ratios(
      full, sch,
      exit_yield            = dcf_res$inputs$exit_yield,
      covenants             = covenants,
      dscr_basis            = "noi",
      ignore_balloon_in_min = TRUE,
      maturity_year         = maturity
    )
    lev  <- compute_leveraged_metrics(dcf_res, sch, equity_invest = acq_price - principal)
    list(schedule = sch, full = full, ratios = rat, metrics = lev)
  }

  bullet <- build_case("bullet")
  amort  <- build_case("amort")

  summary <- dplyr::bind_rows(
    summarize_case(
      "all_equity",
      lev_obj = unlev,
      rat_tbl = dplyr::mutate(dcf_res$cashflows, year = as.integer(year), dscr = Inf, ltv_forward = 0),
      maturity = maturity
    ),
    summarize_case("debt_bullet", bullet$metrics, bullet$ratios, maturity = maturity),
    summarize_case("debt_amort",  amort$metrics,  amort$ratios,  maturity = maturity)
  )

  list(
    summary = summary,
    details = list(
      all_equity = list(metrics = unlev, cashflows = unlev$cashflows),
      debt_bullet = bullet,
      debt_amort  = amort
    )
  )
}
