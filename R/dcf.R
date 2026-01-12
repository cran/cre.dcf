#' Unlevered discounted cash flow model for a commercial real estate asset
#'
#' @description
#' Builds an indexed annual pro forma over years 0..N, a terminal value, and
#' unlevered valuation metrics including net present value (NPV) and internal
#' rate of return (IRR) for a directly held commercial real estate (CRE) asset,
#' without debt. The income base is net operating income (NOI).
#'
#' @details
#' Time convention: \code{year = 0..N}. The acquisition is booked at \code{year = 0}
#' in \code{free_cash_flow} as a negative cash flow equal to the acquisition price,
#' and the sale is booked only at \code{year = N} in \code{sale_proceeds}. The
#' project NPV corresponds to the sum of \code{discounted_cash_flow}.
#'
#' Two construction modes are available for the NOI path:
#'
#' \itemize{
#' \item \strong{Top-down mode} (default): when \code{noi} is \code{NULL}, the NOI
#'   path is derived from the entry yield and acquisition price:
#'   \code{NOI[1] = entry_yield * acq_price}, then indexed with \code{index_rent}
#'   and adjusted by \code{vacancy}.
#' \item \strong{Bottom-up mode}: when \code{noi} is supplied (scalar or vector), it
#'   is recycled to length \code{N} and used as the \code{NOI[1..N]} path. In this
#'   case, \code{entry_yield}, \code{index_rent}, and \code{vacancy} are not used to
#'   recompute NOI.
#' }
#'
#' @param acq_price Numeric scalar. Acquisition price (net of tax or all-in,
#'   depending on the chosen convention).
#' @param entry_yield Numeric scalar in \code{[0, 1]}. Entry yield; in top-down mode,
#'   \code{NOI[1] = entry_yield * acq_price}.
#' @param exit_yield Numeric scalar in \code{(0, 1]}. Exit yield.
#' @param horizon_years Integer scalar greater than or equal to 1. Projection horizon \code{N} in years.
#' @param disc_rate Numeric scalar in \code{(0, 1]}. Discount rate.
#' @param exit_cost Numeric scalar in \code{[0, 1)}. Exit cost as a fraction of the sale price. Default is 0.
#' @param capex Numeric scalar or numeric vector of length \code{N}. Capital expenditure per year. Default is 0.
#' @param index_rent Numeric scalar or numeric vector of length \code{N}. Annual rent indexation rate.
#'   Used only in top-down mode. Default is 0.
#' @param vacancy Numeric scalar or numeric vector of length \code{N} in \code{[0, 1)}. Average annual vacancy.
#'   Used only in top-down mode. Default is 0.
#' @param opex Numeric scalar or numeric vector of length \code{N}. Operating expenses (non-recoverable). Default is 0.
#' @param noi Numeric scalar or numeric vector of length \code{N}, optional. Exogenous NOI path (for example
#'   computed from leases). When non-\code{NULL}, it replaces the internal NOI calculation.
#'
#' @return A list with:
#' \itemize{
#'   \item \code{inputs}: list of main assumptions,
#'   \item \code{cashflows}: tibble 0..N with standardised columns,
#'   \item \code{npv}: project net present value (NPV),
#'   \item \code{irr_project}: project internal rate of return (IRR), unlevered.
#' }
#'
#' @examples
#' res <- dcf_calculate(
#'   acq_price = 1000,
#'   entry_yield = 0.06,
#'   exit_yield = 0.055,
#'   horizon_years = 3,
#'   disc_rate = 0.08,
#'   capex = c(5, 5, 0),
#'   index_rent = c(0.01, 0.01, 0.01),
#'   vacancy = c(0.05, 0.05, 0),
#'   opex = c(10, 10, 10)
#' )
#' res$npv
#' res$irr_project
#' head(res$cashflows)
#'
#' @export

dcf_calculate <- function(
    acq_price,
    entry_yield,
    exit_yield,
    horizon_years,
    disc_rate,
    exit_cost = 0,
    capex = 0,
    index_rent = 0,
    vacancy = 0,
    opex = 0,
    noi = NULL
) {
  # ── scalar validations ─────────────────────────────────────────────────────
  checkmate::assert_number(acq_price, lower = 0)
  checkmate::assert_number(entry_yield, lower = 0, upper = 1)
  checkmate::assert_number(exit_yield, lower = .Machine$double.eps, upper = 1)
  checkmate::assert_int(horizon_years, lower = 1)
  checkmate::assert_number(disc_rate, lower = .Machine$double.eps, upper = 1)
  checkmate::assert_number(exit_cost, lower = 0, upper = 1)

  # ── recycling to length N ──────────────────────────────────────────────────
  N <- horizon_years
  recycle <- function(x) if (length(x) == 1L) rep(x, N) else x

  capex      <- recycle(capex)
  index_rent <- recycle(index_rent)
  vacancy    <- recycle(vacancy)
  opex       <- recycle(opex)

  checkmate::assert_numeric(capex,      len = N, any.missing = FALSE)
  checkmate::assert_numeric(index_rent, len = N, any.missing = FALSE)
  checkmate::assert_numeric(vacancy,    len = N, lower = 0, upper = 0.999, any.missing = FALSE)
  checkmate::assert_numeric(opex,       len = N, any.missing = FALSE)

  # ── NOI path: bottom-up (if supplied) or top-down (fallback) ──────────────
  if (!is.null(noi)) {
    # bottom-up mode: imposed NOI (for example, aggregated leases)
    noi <- recycle(noi)
    checkmate::assert_numeric(noi, len = N, any.missing = FALSE)
  } else {
    # historical top-down mode: NOI derived from entry cap rate and trajectories
    noi <- numeric(N)
    noi[1] <- entry_yield * acq_price
    if (N > 1) {
      for (t in 2:N) {
        # indexed rent, adjusted for relative vacancy
        noi[t] <- noi[t - 1] * (1 + index_rent[t]) * (1 - vacancy[t]) / (1 - vacancy[t - 1])
      }
    }
  }

  # ── free cash flow before debt ─────────────────────────────────────────────
  free_cf <- noi - opex - capex

  # ── terminal value and net sale proceeds ───────────────────────────────────
  # choice of stabilised terminal NOI
  noi_terminal <- select_terminal_noi(
    noi             = noi,
    vacancy         = vacancy,
    capex           = capex,
    noi_theoretical = NULL  # or a value computed upstream if passed from cfg_normalize()
  )

  gross_sale <- noi_terminal / exit_yield
  net_sale   <- gross_sale * (1 - exit_cost)

  # ── table 0...N (sale only at N) ─────────────────────────────────────────────
  # value components
  free_cf <- noi - opex - capex

  # add the sale in the final year
  free_cf[N] <- free_cf[N] + net_sale

  cash <- tibble::tibble(
    year                 = 0:N,
    net_operating_income = c(0, noi),
    capex                = c(0, capex),
    opex                 = c(0, opex),
    free_cash_flow       = c(-acq_price, free_cf),
    sale_proceeds        = c(rep(0, N), net_sale),
    discount_factor      = (1 + disc_rate)^(0:N),
    discounted_cash_flow = free_cash_flow / discount_factor,
    asset_value          = c(rep(NA_real_, N), gross_sale),
    acquisition_price    = c(acq_price, rep(NA_real_, N))
  )

  # minimal contract on columns and uniqueness of the sale
  req <- c(
    "year", "net_operating_income", "opex", "capex", "free_cash_flow",
    "sale_proceeds", "discount_factor", "discounted_cash_flow",
    "asset_value", "acquisition_price"
  )
  miss <- setdiff(req, names(cash))
  if (length(miss)) {
    stop("dcf_calculate(): missing columns: ", paste(miss, collapse = ", "))
  }
  if (sum(cash$sale_proceeds[cash$year < N]) != 0) {
    stop("dcf_calculate(): sale_proceeds must be zero for year < N.")
  }

  # alias for potential compatibility
  cash$discounted_cf <- cash$discounted_cash_flow

  # ── project aggregates ─────────────────────────────────────────────────────
  npv_proj <- sum(cash$discounted_cash_flow, na.rm = TRUE)
  irr_proj <- irr_safe(cash$free_cash_flow)

  list(
    inputs = list(
      acq_price          = acq_price,
      entry_yield        = entry_yield,
      exit_yield         = exit_yield,
      horizon_years      = N,
      disc_rate          = disc_rate,
      exit_cost          = exit_cost
    ),
    cashflows   = cash,
    npv         = npv_proj,
    irr_project = irr_proj
  )
}

#' Discount factor
#' @keywords internal
discount_factor <- function(r, t) 1 / (1 + r)^t

#' Unlevered summary (project metrics)
#' @description Derives project-level metrics from the standard DCF table.
#' @param dcf_res list. Output of `dcf_calculate()`.
#' @return list containing `irr_project`, `npv_project`, `irr_equity`,
#'   `npv_equity`, and `cashflows`.
#' @export
compute_unleveraged_metrics <- function(dcf_res) {
  checkmate::assert_list(dcf_res, any.missing = FALSE)
  checkmate::assert_data_frame(dcf_res$cashflows, min.rows = 2L)
  cf <- dcf_res$cashflows

  irr_proj <- irr_safe(cf$free_cash_flow)

  npv_proj <- sum(cf$discounted_cash_flow, na.rm = TRUE)

  list(
    scenario     = "all_equity",
    irr_equity   = irr_proj,
    npv_equity   = npv_proj,
    irr_project  = irr_proj,
    npv_project  = npv_proj,
    cashflows    = cf
  )
}

#' Levered summary (equity cash flows and equity metrics)
#' @description Builds equity cash flows from a  Discounted Cash Flow (DCF) table and a standardised
#'   debt schedule.
#' @param dcf_res list. Output of `dcf_calculate()`.
#' @param debt_sched data.frame. Output of `debt_built_schedule()` (0...N).
#' @param equity_invest numeric(1). Equity contribution at `t = 0` (positive).
#' @return list containing `irr_equity`, `npv_equity`, `cashflows` (levered table),
#'   and a reminder of the project-level metrics.
#' @export
compute_leveraged_metrics <- function(dcf_res, debt_sched, equity_invest) {

  # --- validations ----------------------------------------------------------
  checkmate::assert_list(dcf_res, min.len = 1)
  checkmate::assert_data_frame(dcf_res$cashflows, min.rows = 2)
  checkmate::assert_data_frame(debt_sched, min.rows = 2)
  checkmate::assert_number(equity_invest, lower = 0)

  cf <- dcf_res$cashflows

  req_cf <- c("year", "free_cash_flow", "discount_factor")
  miss_cf <- setdiff(req_cf, names(cf))
  if (length(miss_cf))
    stop("compute_leveraged_metrics(): missing DCF-side columns: ",
         paste(miss_cf, collapse = ", "))

  req_ds <- c("year", "payment", "interest", "outstanding_debt",
              "arrangement_fee", "debt_draw")
  miss_ds <- setdiff(req_ds, names(debt_sched))
  if (length(miss_ds))
    stop("compute_leveraged_metrics(): missing debt-side columns: ",
         paste(miss_ds, collapse = ", "))

  # --- Alignment 0...N --------------------------------------------------------
  years <- cf$year
  idx <- match(debt_sched$year, years)

  serv <- intr <- outst <- arr <- draw <- rep(0, length(years))
  serv[idx] <- as.numeric(debt_sched$payment)
  intr[idx] <- as.numeric(debt_sched$interest)
  outst[idx] <- as.numeric(debt_sched$outstanding_debt)
  arr[idx]   <- as.numeric(debt_sched$arrangement_fee)
  draw[idx]  <- as.numeric(debt_sched$debt_draw)

  # --- Equity cash flows ----------------------------------------------------
  eq_cf <- as.numeric(cf$free_cash_flow)

  # t = 0 : equity investment (negative from investor perspective)
  eq_cf[1] <- -equity_invest

  # t >= 1 : subtract debt service + fees
  if (length(eq_cf) >= 2) {
    eq_cf[2:length(eq_cf)] <- cf$free_cash_flow[2:length(eq_cf)] -
                              serv[2:length(eq_cf)] -
                              arr[2:length(eq_cf)]
  }

  # t = N : DO NOT ADD ANYTHING --> the sale is already in free_cash_flow[N]

  # --- Metrics --------------------------------------------------------------
  df_vec <- cf$discount_factor

  irr_eq <- irr_safe(eq_cf)
  npv_eq <- if (all(is.finite(df_vec))) sum(eq_cf / df_vec) else NA_real_

  irr_prj <- dcf_res$irr_project %||%
    irr_safe(cf$free_cash_flow)

  npv_prj <- dcf_res$npv %||%
    sum(cf$free_cash_flow / df_vec)

  # --- Levered table --------------------------------------------------------
  levered_cf <- tibble::tibble(
    year             = years,
    free_cash_flow   = cf$free_cash_flow,
    discount_factor  = df_vec,
    payment          = serv,
    interest         = intr,
    outstanding_debt = outst,
    arrangement_fee  = arr,
    debt_draw        = draw,
    equity_cf        = eq_cf
  )

  list(
    scenario      = "levered",
    irr_equity    = irr_eq,
    npv_equity    = npv_eq,
    irr_project   = irr_prj,
    npv_project   = npv_prj,
    cashflows     = levered_cf,
    equity_0      = equity_invest,
    loan_draw_0   = draw[1]
  )
}

#' Assemble the full cash-flow table (discounted cash flow and debt)
#'
#' Builds an annual table by merging operating cash flows from a discounted cash
#' flow model with a debt schedule; standardises gross effective income (GEI) and
#' net operating income (NOI), computes post-debt cash flows, the equity cash flow,
#' and discounted equity cash flows. Enforces a minimal contract on expected
#' columns on both inputs.
#'
#' @param dcf A `list` containing at least an element `cashflows` (data.frame or tibble)
#'   with one row per `year` and the following columns:
#'   - `year` (integer, 0 = acquisition date),
#'   - `net_operating_income` (numeric),
#'   - `capex` (numeric, optional),
#'   - `free_cash_flow` (numeric, pre-debt cash flow),
#'   - `sale_proceeds` (numeric, sale proceeds in the exit year, 0 otherwise),
#'   - `discount_factor` (numeric, strictly positive discount factor).
#'
#'   If `gei` or `noi` are missing, they are derived according to the convention:
#'   `gei := net_operating_income` and `noi := gei - opex`. If `opex` is missing,
#'   it is set to 0.
#'
#' @param schedule A data.frame or tibble of the debt schedule with one row per
#'   `year` and the required columns:
#'   - `year` (integer, aligned with `dcf$cashflows$year`),
#'   - `debt_draw` (numeric, drawdown; typically positive at `year == 0`),
#'   - `interest` (numeric),
#'   - `amortization` (numeric),
#'   - `payment` (numeric, debt service = interest + amortization; must be 0 at `year == 0`),
#'   - `arrangement_fee` (numeric, upfront or recurring fees),
#'   - `outstanding_debt` (numeric, end-of-period outstanding balance).
#'
#' @return A merged tibble (join on `year`) containing:
#'   - all input columns from the  Discounted Cash Flow (DCF) and the debt schedule,
#'   - `df` (alias of `discount_factor`),
#'   - `cf_pre_debt` (= `free_cash_flow`),
#'   - `cf_post_debt` (= `free_cash_flow - payment - arrangement_fee + debt_draw`),
#'   - `equity_flow` (= `cf_post_debt + sale_proceeds`),
#'   - `equity_disc` (= `equity_flow / df`).
#'
#' @details
#' Invariants and checks:
#' - Stop if required columns are missing on the  Discounted Cash Flow (DCF) or the debt side.
#' - Stop if `payment[year == 0] != 0`.
#' - Warn if `debt_draw[year == 0] <= 0`.
#'
#' @examples
#' cf <- tibble::tibble(
#'   year = 0:2,
#'   net_operating_income = c(NA, 120, 124),
#'   opex = c(0, 20, 21),
#'   capex = c(0, 5, 5),
#'   free_cash_flow = c(-100, 95, 98),
#'   sale_proceeds = c(0, 0, 150),
#'   discount_factor = c(1, 1.05, 1.1025)
#' )
#' dcf <- list(cashflows = cf)
#'
#' schedule <- tibble::tibble(
#'   year = 0:2,
#'   debt_draw = c(60, 0, 0),
#'   interest = c(0, 3, 2),
#'   amortization = c(0, 10, 50),
#'   payment = interest + amortization,
#'   arrangement_fee = c(0.6, 0, 0),
#'   outstanding_debt = c(60, 50, 0)
#' )
#'
#' res <- cf_make_full_table(dcf, schedule)
#' res
#'
#' @export

cf_make_full_table <- function(dcf, schedule) {
  stopifnot(is.list(dcf), "cashflows" %in% names(dcf))
  cf <- dcf$cashflows

  # Minimal  Discounted Cash Flow (DCF)-side contract
  req_cf <- c("year","net_operating_income","opex","capex",
              "free_cash_flow","sale_proceeds","discount_factor")
  miss_cf <- setdiff(req_cf, names(cf))
  if (length(miss_cf)) {
    stop("cf_make_full_table(): missing DCF-side columns: ",
         paste(miss_cf, collapse = ", "))
  }

  # GEI/NOI standardisation
  if (!"gei" %in% names(cf))  cf$gei  <- cf$net_operating_income
  if (!"opex" %in% names(cf)) cf$opex <- 0
  if (!"noi" %in% names(cf))  cf$noi  <- cf$gei - cf$opex

  # Minimal debt-side contract
  req_debt <- c("year","debt_draw","interest","amortization","payment",
                "arrangement_fee","outstanding_debt")
  miss_debt <- setdiff(req_debt, names(schedule))
  if (length(miss_debt)) {
    stop("cf_make_full_table(): missing debt-side columns: ",
         paste(miss_debt, collapse = ", "))
  }

  df <- dplyr::left_join(cf, schedule, by = "year") %>%
    dplyr::mutate(
      df           = discount_factor,
      cf_pre_debt  = free_cash_flow,
      cf_post_debt = free_cash_flow - payment - arrangement_fee + debt_draw,
      equity_flow  = cf_post_debt + sale_proceeds,
      equity_disc  = equity_flow / df
    )

  if (any(df$year == 0 & df$payment != 0, na.rm = TRUE)) {
    stop("payment[t = 0] must be 0.")
  }
  if (any(df$year == 0 & df$debt_draw <= 0, na.rm = TRUE)) {
    warning("debt_draw[t = 0] should be > 0 for an initial loan drawdown.")
  }

  df
}

#' Equity cash flows and metrics in the presence of debt
#'
#' @description
#' Computes equity cash flows over \eqn{t = 0..N} from an unlevered Discounted Cash Flow (DCF) and an
#' annual debt schedule, then derives equity IRR and equity NPV. The convention
#' is that \code{free_cash_flow} includes the acquisition at \eqn{t = 0} as a
#' negative flow and includes operating free cash flows for \eqn{t >= 1}. Sale
#' proceeds are booked at \eqn{t = N} via \code{sale_proceeds}.
#'
#' @param dcf_res list. Result of \code{dcf_calculate()}. Must contain:
#'   \itemize{
#'     \item \code{inputs} with at least \code{acq_price}, \code{disc_rate}, \code{exit_yield},
#'     \item \code{cashflows} with at least \code{year}, \code{free_cash_flow}, \code{sale_proceeds},
#'           \code{net_operating_income}.
#'   }
#'
#' @param debt_sched data.frame or tibble. Debt schedule (output of
#'   \code{debt_built_schedule()}). Minimal columns: \code{year}, \code{payment},
#'   \code{interest}, \code{amortization}, \code{outstanding_debt}. Years must be
#'   compatible with \code{dcf_res$cashflows$year}.
#'
#' @param cfg list. Financing parameters. Must contain \code{ltv_init}. Optional:
#'   \code{arrangement_fee_pct} (default 0) and \code{capitalized_fees} (default TRUE).
#'
#' @return
#' A list with:
#' \itemize{
#'   \item \code{equity_cf}: numeric vector of equity cash flows,
#'   \item \code{metrics}: list with \code{irr_equity}, \code{npv_equity},
#'         \code{equity_0}, \code{loan_draw_0},
#'   \item \code{full}: \code{dcf_res$cashflows} enriched by \code{add_credit_ratios()}.
#' }
#'
#' @examples
#' dcf <- dcf_calculate(
#'   acq_price = 1e7, entry_yield = 0.05, exit_yield = 0.055,
#'   horizon_years = 10, disc_rate = 0.07
#' )
#' sch <- debt_built_schedule(
#'   principal = 6e6, rate_annual = 0.045, maturity = 5, type = "bullet"
#' )
#' out <- cf_compute_levered(
#'   dcf_res = dcf,
#'   debt_sched = sch,
#'   cfg = list(ltv_init = 0.6, arrangement_fee_pct = 0, capitalized_fees = TRUE)
#' )
#' stopifnot(is.numeric(out$metrics$irr_equity) || is.na(out$metrics$irr_equity))
#' stopifnot(is.numeric(out$equity_cf))
#' @export
cf_compute_levered <- function(dcf_res, debt_sched, cfg) {
  checkmate::assert_list(dcf_res, any.missing = FALSE)
  checkmate::assert_data_frame(debt_sched, min.rows = 1)
  checkmate::assert_list(cfg, any.missing = FALSE)

  acq_price <- dcf_res$inputs$acq_price %||% stop("dcf_res$inputs$acq_price is missing")
  ltv_init  <- cfg$ltv_init %||% stop("cfg$ltv_init is missing")
  arr_fee   <- cfg$arrangement_fee_pct %||% 0
  cap_fees  <- cfg$capitalized_fees %||% TRUE

  eq0 <- compute_equity_invest(
    acq_price, ltv_init,
    arrangement_fee_pct = arr_fee,
    capitalized_fees    = cap_fees
  )

  cf_tab <- dcf_res$cashflows
  need_cf <- c("year","free_cash_flow","sale_proceeds","net_operating_income")
  checkmate::assert_subset(need_cf, names(cf_tab))

  ds <- debt_sched
  need_ds <- c("year","payment","interest","amortization","outstanding_debt")
  checkmate::assert_subset(need_ds, names(ds))

  years <- cf_tab$year
  serv  <- rep(0, length(years)); names(serv) <- years
  outst <- rep(NA_real_, length(years)); names(outst) <- years

  idx <- match(ds$year, years)
  serv[idx]  <- as.numeric(ds$payment)
  outst[idx] <- as.numeric(ds$outstanding_debt)

  # equity cash flows
  eq_cf <- as.numeric(cf_tab$free_cash_flow)
  eq_cf[1L] <- -eq0$equity_0
  Tn <- length(eq_cf)
  if (Tn >= 2L) {
    eq_cf[2:Tn] <- cf_tab$free_cash_flow[2:Tn] - serv[2:Tn]
    last <- Tn
    outN <- if (is.finite(outst[last])) outst[last] else 0
    eq_cf[last] <- eq_cf[last] + (cf_tab$sale_proceeds[last] %||% 0) - outN
  }

  # equity metrics
  irr <- irr_safe(eq_cf)
  disc_rate <- dcf_res$inputs$disc_rate %||% NA_real_
  df <- if (is.finite(disc_rate)) (1 + disc_rate)^(0:(length(eq_cf) - 1)) else NA_real_
  npv <- if (all(is.finite(df))) sum(eq_cf / df) else NA_real_

  # Full table enriched with credit ratios (uses the entry Discounted Cash Flow (DCF) exit_yield)
  full <- add_credit_ratios(cf_tab, debt_sched, exit_yield = dcf_res$inputs$exit_yield)

  list(
    equity_cf = eq_cf,
    metrics = list(
      irr_equity  = irr,
      npv_equity  = npv,
      equity_0    = eq0$equity_0,
      loan_draw_0 = eq0$loan_draw_0
    ),
    full = full
  )
}

#' Acquisition price from an entry capitalization rate
#' @description Converts a given \code{NOI_y1} and \code{entry_yield} into a net
#'   purchase price (HT) and an all-in price including acquisition costs
#'   (via \code{acq_cost_rate}).
#' @param noi_y1 numeric(1). Expected \eqn{NOI} for year 1.
#' @param entry_yield numeric(1) in \code{(0,1]}. Entry capitalization rate.
#' @param acq_cost_rate numeric(1) in \code{[0,1)}. Acquisition cost rate.
#' @return list(ht = net price, di = all-in price).
#' @examples
#' price_from_cap(500000, 0.05, acq_cost_rate = 0.07)
#' @export
price_from_cap <- function(noi_y1, entry_yield, acq_cost_rate = 0) {
  checkmate::assert_number(noi_y1, lower = 0)
  checkmate::assert_number(entry_yield, lower = .Machine$double.eps, upper = 1)
  checkmate::assert_number(acq_cost_rate, lower = 0, upper = 1 - 1e-12)
  price_ht <- noi_y1 / entry_yield
  price_di <- price_ht * (1 + acq_cost_rate)
  list(ht = price_ht, di = price_di)
}


#' Quick computation of year-1 NOI
#' @param rent_signed numeric(1). Face rent (€/m²/year).
#' @param lettable_area numeric(1). Lettable area (m²).
#' @param vac_rate numeric(1) in \code{[0,1)}. Average vacancy rate.
#' @return numeric(1) \eqn{NOI_{y1}} rounded to cents.
#' @examples
#' compute_noi_y1(400, 2500, vac_rate = 0.05)
#' @export
compute_noi_y1 <- function(rent_signed, lettable_area, vac_rate = 0) {
  checkmate::assert_number(rent_signed, lower = 0)
  checkmate::assert_number(lettable_area, lower = 0)
  checkmate::assert_number(vac_rate, lower = 0, upper = 0.999)
  round(rent_signed * lettable_area * (1 - vac_rate), 2)
}


#' Stylised rent table (lease cash-flow)
#' @description Builds a minimal \code{year}-\code{noi} table for \code{n_years}
#'   with optionally vectorised vacancy rates.
#' @param rent_signed numeric. Face rent (€/m²/year) (scalar or vector).
#' @param surface_m2 numeric. Floor area (m²) (scalar or vector).
#' @param n_years integer(1). Number of years.
#' @param vac_rate_vec numeric. Vacancy (scalar or vector), recycled to \code{n_years}.
#' @return \code{tibble(year, noi)}.
#' @examples
#' build_lease_table(400, 2500, n_years = 5, vac_rate_vec = c(0, .05, .1))
#' @export
build_lease_table <- function(rent_signed, surface_m2,
                              n_years, vac_rate_vec = 0) {
  checkmate::assert_integerish(n_years, lower = 1, len = 1)
  vac_rate_vec <- as.numeric(unlist(vac_rate_vec))
  rent_signed   <- rep(rent_signed,   length.out = n_years)
  surface_m2    <- rep(surface_m2,    length.out = n_years)
  vac_rate_vec  <- rep(vac_rate_vec,  length.out = n_years)
  checkmate::assert_numeric(vac_rate_vec, lower = 0, upper = 0.999, any.missing = FALSE)

  tibble::tibble(
    year = seq_len(n_years),
    noi  = rent_signed * surface_m2 * (1 - vac_rate_vec)
  )
}

#' Aggregate lease events into annual vectors aligned on base_year..base_year+horizon-1
#'
#' @description
#' Converts a list of lease events into annual vectors for rent, vacancy, free months,
#' tenant capex (€/sqm), and a new_lease flag. The `[start, end]` convention is used:
#' an event applies to years y with start <= y <= end. Overlaps within a unit resolve as:
#' rent/vac/new_lease: last event wins; capex_sqm/free_months: accumulated at start year.
#' Returned vectors are **non-indexed** (indexation is applied later in cfg_normalize()).
#'
#' @param ev list of events with fields: start, end, rent, vac, free_months,
#'   capex_sqm, new_lease.
#' @param horizon integer(1) >= 1, number of annual steps.
#' @param base_year integer(1), first absolute year of the horizon.
#'
#' @return list with numeric vectors of length `horizon`:
#'   `rent`, `vac`, `free`, `capex_sqm`, `new_lease`.
#' @export
leases_tbl_structuration <- function(ev, horizon, base_year) {
  checkmate::assert_list(ev, min.len = 1)
  checkmate::assert_integerish(horizon, lower = 1, len = 1)
  checkmate::assert_integerish(base_year, len = 1)

  `%||%` <- function(x, y) if (is.null(x)) y else x

  yrs_abs <- seq(base_year, length.out = horizon)

  # initialise
  rent_vec      <- numeric(horizon)  # €/sqm/year
  vac_vec       <- numeric(horizon)  # rate in [0,1]
  free_vec      <- numeric(horizon)  # fraction of year (0..1) applied at start year
  capex_sqm_vec <- numeric(horizon)  # €/sqm, distributed across event span
  new_lease_vec <- numeric(horizon)  # indicator (0/1) at start year

  for (e in ev) {
    # sanitize event fields
    e$start        <- as.integer(e$start)
    e$end          <- as.integer(e$end)
    e$rent         <- e$rent        %||% 0
    e$vac          <- e$vac         %||% 0
    e$free_months  <- e$free_months %||% 0
    e$capex_sqm    <- e$capex_sqm   %||% 0
    e$new_lease    <- e$new_lease   %||% 0

    # CLOSED interval: start <= year <= end
    idx <- which(yrs_abs >= e$start & yrs_abs <= e$end)
    if (length(idx) == 0L) next

    # last-wins for rent and vacancy over the active span
    rent_vec[idx] <- e$rent
    vac_vec[idx]  <- e$vac

    # one-off accumulators at the start year
    i0 <- idx[1]
    free_frac <- max(0, min(1, e$free_months / 12))

    # Free rent applies only for a new lease
    if (isTRUE(e$new_lease) || e$new_lease == 1) {
      free_vec[i0] <- free_vec[i0] + free_frac
    }

    # CAPEX distributed across the event span
    n_years      <- length(idx)
    annual_capex <- e$capex_sqm / n_years
    capex_sqm_vec[idx] <- capex_sqm_vec[idx] + annual_capex

    # new_lease flag: last event wins at start year
    new_lease_vec[i0] <- as.numeric(e$new_lease)
  }

  # clamp accidental over-accumulation of free months to [0,1]
  free_vec <- pmax(0, pmin(1, free_vec))

  list(
    rent      = rent_vec,
    vac       = vac_vec,
    free      = free_vec,
    capex_sqm = capex_sqm_vec,
    new_lease = new_lease_vec
  )
}



#' IRR decomposition between operations and resale
#'
#' @description
#' Approximates the relative contribution of:
#'   - operational cash flows (acquisition + NOI - capex - opex),
#'   - resale (net sale in year N),
#' to the total IRR, using NPV shares (`share`) and mapping them to
#' `irr_total` (`irr_contrib = irr_total * share`).
#'
#' @param cashflows data.frame. Must contain at least
#'   \code{year}, \code{free_cash_flow}, \code{discount_factor}.
#'   If \code{sale_proceeds} is missing, it is assumed to be zero.
#' @param tv_disc numeric(1). Terminal value already discounted (net sale),
#'   if available. If \code{NULL}, it is derived from \code{sale_proceeds}
#'   and \code{discount_factor}.
#' @param irr_total numeric(1). Total IRR (project or equity) for which the
#'   decomposition is sought (e.g. \code{dcf_res$irr_project} or an equity IRR).
#' @param initial_investment numeric(1). Not used here (kept for API compatibility).
#'
#' @return \code{tibble(component, share, irr_contrib)} with two rows:
#'   "Operations" and "Resale".
#' @export
irr_partition <- function(cashflows, tv_disc = NULL, irr_total, initial_investment = NULL) {
  checkmate::assert_data_frame(cashflows, min.rows = 1)
  need <- c("year", "free_cash_flow", "discount_factor")
  checkmate::assert_subset(need, names(cashflows))

  years <- cashflows$year
  df    <- cashflows$discount_factor

  # add sale_proceeds if missing
  if (!("sale_proceeds" %in% names(cashflows))) {
    cashflows$sale_proceeds <- 0
  }
  sale <- cashflows$sale_proceeds

  # index of last year (N)
  last <- which.max(years)

  # --- NPV of operations ----------------------------------------------------
  # free_cash_flow already includes resale at N:
  # remove resale component to isolate operations.
  ops_cf <- cashflows$free_cash_flow
  ops_cf[last] <- ops_cf[last] - sale[last]

  pv_ops <- sum(ops_cf / df, na.rm = TRUE)

  # --- NPV of resale --------------------------------------------------------
  pv_tv <- if (!is.null(tv_disc)) {
    checkmate::assert_number(tv_disc)
    tv_disc
  } else {
    sale[last] / df[last]
  }

  total_pv <- pv_ops + pv_tv

  if (!is.finite(total_pv) || abs(total_pv) < .Machine$double.eps) {
    return(tibble::tibble(
      component   = c("Operations", "Resale"),
      share       = c(NA_real_, NA_real_),
      irr_contrib = c(NA_real_, NA_real_)
    ))
  }

  s_ops <- pv_ops / total_pv
  s_tv  <- pv_tv  / total_pv

  tibble::tibble(
    component   = c("Operations", "Resale"),
    share       = c(s_ops, s_tv),
    irr_contrib = irr_total * c(s_ops, s_tv)
  )
}

#' Explicitly standardise GEI and NOI columns in a Discounted Cash Flow (DCF) cash-flow table
#'
#' @description
#' Guarantees the presence of numeric columns \code{gei} and \code{noi} in a
#' cash-flow table, to make explicit the income base used for the unlevered
#' project IRR. In this package, \code{gei} denotes gross effective income
#' (after vacancy and rent-free effects) and \code{noi} is computed as
#' \code{gei - opex}.
#'
#' The input may provide \code{gei} directly, or a legacy column
#' \code{net_operating_income} which is interpreted here as \code{gei}
#' (compatibility with earlier pipelines).
#'
#' @param cf_tab data.frame|tibble Cash-flow table for periods 0..N, typically
#'   produced by \code{dcf_calculate()}.
#'   Required columns: \code{opex} and either \code{gei} or
#'   \code{net_operating_income}.
#'
#' @return A \code{tibble} with guaranteed numeric columns \code{gei} and
#'   \code{noi}. Existing \code{noi} is preserved when present, but a warning is
#'   emitted if it differs from \code{gei - opex} beyond a small tolerance.
#'
#' @examples
#' # Minimal example with a legacy column name (net_operating_income interpreted as GEI)
#' cf_tab <- tibble::tibble(
#'   year = 0:2,
#'   net_operating_income = c(0, 120, 124),
#'   opex = c(0, 20, 21)
#' )
#' dcf_add_noi_columns(cf_tab)
#'
#' # Example where GEI is provided explicitly and NOI is already present
#' cf_tab2 <- tibble::tibble(
#'   year = 0:2,
#'   gei  = c(0, 120, 124),
#'   opex = c(0, 20, 21),
#'   noi  = c(0, 100, 103)
#' )
#' dcf_add_noi_columns(cf_tab2)
#' @export
dcf_add_noi_columns <- function(cf_tab) {
  checkmate::assert_data_frame(cf_tab, min.rows = 2)

  nms <- names(cf_tab)
  has <- function(nm) nm %in% nms

  gei_col <- dplyr::case_when(
    has("gei") ~ "gei",
    has("net_operating_income") ~ "net_operating_income",
    TRUE ~ NA_character_
  )

  if (is.na(gei_col)) {
    stop(
      "dcf_add_noi_columns(): missing income base. ",
      "Provide column 'gei' or 'net_operating_income'. ",
      "Available columns: ", paste(nms, collapse = ", ")
    )
  }

  if (!has("opex")) {
    stop(
      "dcf_add_noi_columns(): column 'opex' required. ",
      "Available columns: ", paste(nms, collapse = ", ")
    )
  }

  # Coerce to numeric and warn if coercion introduces new missing values
  to_num_checked <- function(x, col_name) {
    x_chr <- as.character(x)
    x_num <- suppressWarnings(as.numeric(x_chr))

    introduced_na <- is.na(x_num) & !is.na(x_chr) & trimws(x_chr) != ""
    if (any(introduced_na)) {
      bad_examples <- unique(utils::head(x_chr[introduced_na], 5))
      warning(
        "dcf_add_noi_columns(): numeric coercion introduced NA in '", col_name,
        "'. Examples: ", paste(bad_examples, collapse = " | ")
      )
    }
    x_num
  }

  gei_num  <- to_num_checked(cf_tab[[gei_col]], gei_col)
  opex_num <- to_num_checked(cf_tab[["opex"]], "opex")

  checkmate::assert_numeric(gei_num, any.missing = TRUE)
  checkmate::assert_numeric(opex_num, any.missing = TRUE)
  checkmate::assert_true(length(gei_num) == nrow(cf_tab))
  checkmate::assert_true(length(opex_num) == nrow(cf_tab))

  noi_calc <- gei_num - opex_num

  out <- dplyr::as_tibble(cf_tab)

  # Always standardise gei as numeric, regardless of original type
  out$gei <- gei_num

  # Add noi if missing; if present, keep it but warn if it contradicts gei - opex
  if (!has("noi")) {
    out$noi <- noi_calc
  } else {
    noi_num <- to_num_checked(out$noi, "noi")
    out$noi <- noi_num

    ok <- is.finite(noi_num) & is.finite(noi_calc)
    if (any(ok)) {
      # Tolerance scaled to magnitude to avoid spurious warnings on large flows
      tol <- 1e-8 + 1e-6 * pmax(1, abs(noi_calc[ok]))
      if (any(abs(noi_num[ok] - noi_calc[ok]) > tol, na.rm = TRUE)) {
        warning(
          "dcf_add_noi_columns(): existing 'noi' differs from 'gei - opex' for at least one row. ",
          "The existing 'noi' has been preserved."
        )
      }
    }
  }

  out
}

#' Robust selection of terminal NOI for resale valuation
#'
#' @description
#' Chooses a stabilised net operating income (NOI) for terminal value
#' calculation, using a hierarchical decision rule designed to mitigate
#' distortions driven by vacancy, capital expenditure, or atypical end-of-horizon
#' cash-flow patterns.
#'
#' The selection logic proceeds as follows:
#' \enumerate{
#'   \item If \code{NOI_N} is (numerically) zero and
#'     \code{force_theoretical_if_noi_n_zero} is \code{TRUE}, use
#'     \code{noi_theoretical} when provided.
#'   \item If year \code{N} is clean (zero vacancy, zero capex, and \code{NOI_N > 0}),
#'     use \code{NOI_N}.
#'   \item If year \code{N} is distorted but year \code{N-1} is clean and
#'     \code{NOI_{N-1} > 0}, use \code{NOI_{N-1}}.
#'   \item Otherwise, if \code{noi_theoretical} is provided, use it.
#'   \item As a last resort, fall back to \code{NOI_N}. A warning is emitted only
#'     when \code{NOI_N <= 0}.
#' }
#'
#' @param noi Numeric vector of length \code{N} containing annual NOI values
#'   for years 1..N.
#' @param vacancy Optional numeric vector of length \code{N} giving annual
#'   vacancy rates. When \code{NULL}, vacancy is assumed to be zero in all years.
#' @param capex Optional numeric vector of length \code{N} giving annual capital
#'   expenditures. When \code{NULL}, capex is assumed to be zero in all years.
#' @param noi_theoretical Optional numeric scalar giving a stabilised theoretical
#'   NOI (for example market rent multiplied by area).
#' @param force_theoretical_if_noi_n_zero Logical scalar. When \code{TRUE}, and
#'   \code{NOI_N} is numerically zero, \code{noi_theoretical} is used when
#'   available.
#'
#' @return Numeric scalar giving the NOI retained for capitalization.
#' @export
select_terminal_noi <- function(noi,
                                vacancy = NULL,
                                capex = NULL,
                                noi_theoretical = NULL,
                                force_theoretical_if_noi_n_zero = TRUE) {

  checkmate::assert_numeric(noi, any.missing = FALSE)
  N <- length(noi)
  if (N < 1L) stop("select_terminal_noi(): 'noi' must have length >= 1.")

  # standardise vacancy/capex
  vac <- if (is.null(vacancy)) {
    rep(0, N)
  } else {
    checkmate::assert_numeric(vacancy, len = N, lower = 0, upper = 1)
    vacancy
  }

  cap <- if (is.null(capex)) {
    rep(0, N)
  } else {
    checkmate::assert_numeric(capex, len = N, lower = 0)
    capex
  }

  # NOI_N == 0 is considered distorted by default
  noi_N_zero <- (abs(noi[N]) < .Machine$double.eps)

  # distorted N = vacancy > 0, capex > 0, or NOI_N == 0
  is_distorted_N <- (vac[N] > 0) || (cap[N] > 0) || noi_N_zero

  # 1) Explicit case: NOI_theoretical if NOI_N == 0 and flag = TRUE
  if (noi_N_zero && isTRUE(force_theoretical_if_noi_n_zero)) {
    if (!is.null(noi_theoretical)) {
      checkmate::assert_number(noi_theoretical, lower = 0)
      return(noi_theoretical)
    }
  }

  # 2) Simple case: clean year N --> NOI_N
  if (!is_distorted_N) {
    return(noi[N])
  }

  # 3) Fallback: distorted N but clean N-1
  if (N >= 2L && vac[N - 1L] == 0 && cap[N - 1L] == 0 && noi[N - 1L] > 0) {
    return(noi[N - 1L])
  }

  # 4) Reinforced fallback: use theoretical NOI if available
  if (!is.null(noi_theoretical)) {
    checkmate::assert_number(noi_theoretical, lower = 0)
    return(noi_theoretical)
  }

  # 5) Extreme fallback: use NOI_N; warn only when NOI_N <= 0
  if (noi[N] <= 0) {
    warning(
      "select_terminal_noi(): unable to determine a clean stabilised NOI. ",
      "Falling back to NOI_N = ", noi[N]
    )
  }
  noi[N]
}
