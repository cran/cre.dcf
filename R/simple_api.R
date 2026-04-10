#' Define simple debt terms for a CRE deal
#'
#' @description
#' Builds a compact financing specification intended for the simplified R API.
#' Use this helper together with [deal_spec()] and [analyze_deal()] instead of
#' manipulating the full YAML-like configuration directly.
#'
#' @param ltv Numeric scalar in \code{[0, 1]}. Initial loan-to-value ratio.
#' @param rate Numeric scalar in \code{[0, 1]}. Annual nominal interest rate.
#' @param type Character scalar. Either \code{"bullet"} or \code{"amort"}.
#' @param maturity Optional integer scalar greater than or equal to 1. When
#'   \code{NULL}, the deal horizon is used.
#' @param extra_amort_pct Numeric scalar in \code{[0, 1]}. Additional annual
#'   amortisation share used for bullet structures.
#' @param arrangement_fee_pct Numeric scalar in \code{[0, 1]}. Arrangement fee
#'   applied to the initial principal.
#' @param capitalized_fees Logical scalar. Whether arrangement fees are
#'   capitalized into the initial draw.
#'
#' @return An object of class \code{cre_debt_terms}.
#' @export
debt_terms <- function(ltv = 0.55,
                       rate = 0.045,
                       type = c("bullet", "amort"),
                       maturity = NULL,
                       extra_amort_pct = 0,
                       arrangement_fee_pct = 0,
                       capitalized_fees = FALSE) {
  type <- match.arg(type)

  checkmate::assert_number(ltv, lower = 0, upper = 1)
  checkmate::assert_number(rate, lower = 0, upper = 1)
  checkmate::assert_number(extra_amort_pct, lower = 0, upper = 1)
  checkmate::assert_number(arrangement_fee_pct, lower = 0, upper = 1)
  checkmate::assert_flag(capitalized_fees)
  if (!is.null(maturity)) {
    checkmate::assert_integerish(maturity, lower = 1, len = 1)
    maturity <- as.integer(maturity)
  }

  structure(
    list(
      ltv = ltv,
      rate = rate,
      type = type,
      maturity = maturity,
      extra_amort_pct = extra_amort_pct,
      arrangement_fee_pct = arrangement_fee_pct,
      capitalized_fees = capitalized_fees
    ),
    class = c("cre_debt_terms", "list")
  )
}

#' Define a lease event for the simplified lease-roll API
#'
#' @param start Integer scalar. First calendar year covered by the event.
#' @param end Integer scalar. Last calendar year covered by the event.
#' @param rent Numeric scalar greater than or equal to 0. Headline rent in
#'   annual currency per sqm.
#' @param vac Numeric scalar in \code{[0, 1]}. Vacancy share for the event.
#' @param free_months Numeric scalar greater than or equal to 0. Rent-free
#'   period applied at lease start when \code{new_lease = TRUE}.
#' @param capex_sqm Numeric scalar greater than or equal to 0. Capital
#'   expenditure per sqm allocated across the event span.
#' @param new_lease Logical or integer scalar. Whether the event corresponds to
#'   a new letting.
#'
#' @return An object of class \code{cre_lease_event}.
#' @examples
#' lease_event(start = 2025, end = 2027, rent = 220, vac = 0.05)
#' @export
lease_event <- function(start,
                        end,
                        rent,
                        vac = 0,
                        free_months = 0,
                        capex_sqm = 0,
                        new_lease = FALSE) {
  checkmate::assert_integerish(start, len = 1)
  checkmate::assert_integerish(end, len = 1)
  start <- as.integer(start)
  end <- as.integer(end)
  if (end < start) {
    stop("lease_event(): `end` must be greater than or equal to `start`.")
  }

  checkmate::assert_number(rent, lower = 0)
  checkmate::assert_number(vac, lower = 0, upper = 1)
  checkmate::assert_number(free_months, lower = 0)
  checkmate::assert_number(capex_sqm, lower = 0)
  new_lease_int <- if (is.logical(new_lease)) {
    as.integer(isTRUE(new_lease))
  } else {
    checkmate::assert_integerish(new_lease, len = 1)
    as.integer(new_lease)
  }
  if (!(new_lease_int %in% c(0L, 1L))) {
    stop("lease_event(): `new_lease` must resolve to 0/1 or FALSE/TRUE.")
  }

  structure(
    list(
      start = start,
      end = end,
      rent = rent,
      vac = vac,
      free_months = free_months,
      capex_sqm = capex_sqm,
      new_lease = new_lease_int
    ),
    class = c("cre_lease_event", "list")
  )
}

#' Define an explicit vacancy event
#'
#' @param start Integer scalar. First vacant calendar year.
#' @param end Integer scalar. Last vacant calendar year.
#' @param capex_sqm Numeric scalar greater than or equal to 0. Capital
#'   expenditure per sqm allocated across the vacancy span.
#'
#' @return An object of class \code{cre_lease_event}.
#' @examples
#' vacancy_event(start = 2028, end = 2028, capex_sqm = 40)
#' @export
vacancy_event <- function(start, end, capex_sqm = 0) {
  lease_event(
    start = start,
    end = end,
    rent = 0,
    vac = 1,
    free_months = 0,
    capex_sqm = capex_sqm,
    new_lease = FALSE
  )
}

#' Define a renewal or reletting event
#'
#' @param start Integer scalar. First calendar year covered by the renewed or
#'   relet lease event.
#' @param end Integer scalar. Last calendar year covered by the event.
#' @param rent Numeric scalar greater than or equal to 0. Headline rent in
#'   annual currency per sqm.
#' @param free_months Numeric scalar greater than or equal to 0. Rent-free
#'   period applied at the new letting date.
#' @param capex_sqm Numeric scalar greater than or equal to 0. Capital
#'   expenditure per sqm allocated across the event span.
#' @param vac Numeric scalar in \code{[0, 1]}. Residual vacancy share after the
#'   lease starts.
#'
#' @return An object of class \code{cre_lease_event}.
#' @examples
#' renewal_event(start = 2029, end = 2033, rent = 245, free_months = 3)
#' @export
renewal_event <- function(start,
                          end,
                          rent,
                          free_months = 0,
                          capex_sqm = 0,
                          vac = 0) {
  lease_event(
    start = start,
    end = end,
    rent = rent,
    vac = vac,
    free_months = free_months,
    capex_sqm = capex_sqm,
    new_lease = TRUE
  )
}

#' Define one lease unit for the simplified lease-roll API
#'
#' @param name Character scalar. Unit label used in messages and downstream
#'   engine structures.
#' @param area_sqm Numeric scalar greater than 0. Lettable area in sqm.
#' @param events List of objects returned by [lease_event()].
#'
#' @return An object of class \code{cre_lease_unit}.
#' @examples
#' u <- lease_unit(
#'   "North",
#'   area_sqm = 1200,
#'   events = list(lease_event(start = 2025, end = 2027, rent = 220))
#' )
#' u$unit
#' @export
lease_unit <- function(name, area_sqm, events) {
  checkmate::assert_string(name, min.chars = 1)
  checkmate::assert_number(area_sqm, lower = .Machine$double.eps)
  checkmate::assert_list(events, min.len = 1)
  if (!all(vapply(events, inherits, logical(1), what = "cre_lease_event"))) {
    stop("lease_unit(): `events` must be created with lease_event().")
  }

  starts <- vapply(events, function(e) e$start, integer(1))
  ends <- vapply(events, function(e) e$end, integer(1))
  ord <- order(starts, ends)
  events <- events[ord]

  structure(
    list(
      unit = name,
      area = area_sqm,
      events = lapply(events, unclass)
    ),
    class = c("cre_lease_unit", "list")
  )
}

#' Group lease units into a simplified lease roll
#'
#' @param units List of objects returned by [lease_unit()].
#'
#' @return An object of class \code{cre_lease_roll}.
#' @examples
#' roll <- lease_roll(list(
#'   lease_unit(
#'     "North",
#'     area_sqm = 1200,
#'     events = list(lease_event(start = 2025, end = 2027, rent = 220))
#'   )
#' ))
#' length(roll$units)
#' @export
lease_roll <- function(units) {
  checkmate::assert_list(units, min.len = 1)
  if (!all(vapply(units, inherits, logical(1), what = "cre_lease_unit"))) {
    stop("lease_roll(): `units` must be created with lease_unit().")
  }

  structure(
    list(units = lapply(units, unclass)),
    class = c("cre_lease_roll", "list")
  )
}

.lease_roll_units <- function(x) {
  if (!inherits(x, "cre_lease_roll")) {
    stop("Expected a `cre_lease_roll` object.")
  }
  x$units
}

.lease_unit_snapshot <- function(unit) {
  unit_name <- unit$unit %||% NA_character_
  unit_area <- as.numeric(unit$area %||% NA_real_)
  events <- unit$events %||% list()
  starts <- vapply(events, function(e) as.integer(e$start %||% NA_integer_), integer(1))
  ends <- vapply(events, function(e) as.integer(e$end %||% NA_integer_), integer(1))
  rents <- vapply(events, function(e) as.numeric(e$rent %||% NA_real_), numeric(1))
  vacs <- vapply(events, function(e) as.numeric(e$vac %||% 0), numeric(1))
  frees <- vapply(events, function(e) as.numeric(e$free_months %||% 0), numeric(1))
  capex <- vapply(events, function(e) as.numeric(e$capex_sqm %||% 0), numeric(1))
  new_lease <- vapply(events, function(e) as.numeric(e$new_lease %||% 0), numeric(1))

  tibble::tibble(
    unit = unit_name,
    area_sqm = unit_area,
    event_count = length(events),
    first_start = if (length(starts)) min(starts) else NA_integer_,
    last_end = if (length(ends)) max(ends) else NA_integer_,
    start_rent_sqm = if (length(rents)) rents[1] else NA_real_,
    end_rent_sqm = if (length(rents)) rents[length(rents)] else NA_real_,
    max_vacancy = if (length(vacs)) max(vacs) else NA_real_,
    free_months_total = sum(frees, na.rm = TRUE),
    capex_sqm_total = sum(capex, na.rm = TRUE),
    reletting_count = sum(new_lease, na.rm = TRUE)
  )
}

.lease_roll_metrics <- function(lease_roll,
                                purchase_year,
                                horizon_years,
                                opex_sqm = 0) {
  units <- .lease_roll_units(lease_roll)
  horizon_years <- as.integer(horizon_years)
  purchase_year <- as.integer(purchase_year)

  total_area <- sum(vapply(units, function(u) u$area %||% 0, numeric(1)))
  gross_potential_rent_y1 <- 0
  vacancy_loss_y1 <- 0

  for (unit in units) {
    st <- leases_tbl_structuration(
      ev = unit$events,
      horizon = max(1L, horizon_years),
      base_year = purchase_year
    )
    gross_u <- (st$rent[1L] %||% 0) * (unit$area %||% 0)
    gross_potential_rent_y1 <- gross_potential_rent_y1 + gross_u
    vacancy_loss_y1 <- vacancy_loss_y1 + gross_u * (st$vac[1L] %||% 0)
  }

  ops <- .engine_project_operations(list(
    purchase_year = purchase_year,
    horizon_years = max(1L, horizon_years),
    index_rate = 0,
    opex_inflation_rate = 0,
    capex_inflation_rate = 0,
    landlord_base_opex_sqm = 0,
    opex_sqm = opex_sqm %||% 0,
    leasing_cost_pct = 0,
    leases = units
  ))

  gei_y1 <- ops$noi_vec[1L] %||% NA_real_
  opex_y1 <- ops$opex_vec[1L] %||% NA_real_
  capex_y1 <- ops$capex_vec[1L] %||% NA_real_
  noi_y1 <- gei_y1 - opex_y1
  pbtcf_y1 <- noi_y1 - capex_y1
  vac_rate_y1 <- if (gross_potential_rent_y1 > 0) {
    vacancy_loss_y1 / gross_potential_rent_y1
  } else {
    NA_real_
  }

  list(
    area_sqm = total_area,
    rent_sqm = if (total_area > 0) gross_potential_rent_y1 / total_area else NA_real_,
    vacancy_rate = vac_rate_y1,
    gross_potential_rent_y1 = gross_potential_rent_y1,
    gei_y1 = gei_y1,
    noi_y1 = noi_y1,
    pbtcf_y1 = pbtcf_y1,
    capex_y1 = capex_y1
  )
}

#' Define a simplified CRE deal specification
#'
#' @description
#' Builds a beginner-friendly deal object that can be analyzed with
#' [analyze_deal()]. Exactly one income mode must be provided:
#' \itemize{
#'   \item \code{entry_yield},
#'   \item \code{noi_y1},
#'   \item \code{rent_sqm + area_sqm},
#'   \item \code{lease_roll}.
#' }
#'
#' @param price Numeric scalar greater than 0. All-in acquisition price
#'   (\code{price_di}) used by the simplified API.
#' @param horizon_years Integer scalar greater than or equal to 1.
#' @param entry_yield Optional numeric scalar in \code{(0, 1]}. Entry yield for
#'   the top-down mode.
#' @param noi_y1 Optional numeric scalar greater than 0. Year-1 NOI.
#' @param rent_sqm Optional numeric scalar greater than or equal to 0. Annual
#'   rent per sqm.
#' @param area_sqm Optional numeric scalar greater than 0. Lettable area in sqm.
#' @param lease_roll Optional object returned by [lease_roll()]. Use this mode
#'   to describe several units and lease events without writing YAML manually.
#' @param vacancy_rate Numeric scalar in \code{[0, 1)}. Used only in the
#'   rent/surface mode.
#' @param opex_sqm Numeric scalar greater than or equal to 0. Used only in the
#'   rent/surface and lease-roll modes.
#' @param purchase_year Integer scalar. Defaults to the current year.
#' @param index_rate Numeric scalar in \code{[0, 1]}. Annual growth/indexation.
#' @param acq_cost_rate Numeric scalar in \code{[0, 1)}. Acquisition cost rate.
#' @param discount_rate Numeric scalar in \code{[0, 1]}. Discount rate mapped to
#'   the \code{"risk_premium"} engine block.
#' @param capex Numeric scalar or numeric vector of length \code{horizon_years}.
#' @param exit_yield_spread_bps Numeric scalar. Exit-yield spread in basis
#'   points.
#' @param exit_cost Numeric scalar in \code{[0, 1)}. Exit cost rate.
#' @param debt Object returned by [debt_terms()].
#'
#' @return An object of class \code{cre_deal_spec}.
#' @export
deal_spec <- function(price,
                      horizon_years = 10L,
                      entry_yield = NULL,
                      noi_y1 = NULL,
                      rent_sqm = NULL,
                      area_sqm = NULL,
                      lease_roll = NULL,
                      vacancy_rate = 0,
                      opex_sqm = 0,
                      purchase_year = as.integer(format(Sys.Date(), "%Y")),
                      index_rate = 0.02,
                      acq_cost_rate = 0.06,
                      discount_rate = 0.08,
                      capex = 0,
                      exit_yield_spread_bps = 0,
                      exit_cost = 0.015,
                      debt = debt_terms()) {
  checkmate::assert_number(price, lower = .Machine$double.eps)
  checkmate::assert_integerish(horizon_years, lower = 1, len = 1)
  checkmate::assert_integerish(purchase_year, len = 1)
  checkmate::assert_number(index_rate, lower = 0, upper = 1)
  checkmate::assert_number(acq_cost_rate, lower = 0, upper = 1 - 1e-12)
  checkmate::assert_number(discount_rate, lower = 0, upper = 1)
  checkmate::assert_number(exit_cost, lower = 0, upper = 1 - 1e-12)
  checkmate::assert_number(vacancy_rate, lower = 0, upper = 0.999)
  checkmate::assert_number(opex_sqm, lower = 0)
  checkmate::assert_number(exit_yield_spread_bps, finite = TRUE)
  checkmate::assert_numeric(capex, any.missing = FALSE, min.len = 1)
  if (!(length(capex) %in% c(1L, as.integer(horizon_years)))) {
    stop("deal_spec(): `capex` must be a scalar or a vector of length `horizon_years`.")
  }
  if (!inherits(debt, "cre_debt_terms")) {
    stop("deal_spec(): `debt` must be created with debt_terms().")
  }

  has_entry <- !is.null(entry_yield)
  has_noi <- !is.null(noi_y1)
  has_rent <- !is.null(rent_sqm) || !is.null(area_sqm)
  has_leases <- !is.null(lease_roll)
  income_modes <- c(has_entry, has_noi, has_rent, has_leases)
  if (sum(income_modes) != 1L) {
    stop(
      paste(
        "deal_spec(): provide exactly one income mode:",
        "`entry_yield`, `noi_y1`, `rent_sqm` + `area_sqm`, or `lease_roll`."
      )
    )
  }

  gross_potential_rent_y1_effective <- NULL
  gei_y1_effective <- NULL
  pbtcf_y1_effective <- NULL

  if (has_entry) {
    checkmate::assert_number(entry_yield, lower = .Machine$double.eps, upper = 1)
    income_mode <- "entry_yield"
    noi_y1_effective <- entry_yield * price
    entry_yield_effective <- entry_yield
  } else if (has_noi) {
    checkmate::assert_number(noi_y1, lower = .Machine$double.eps)
    income_mode <- "noi_y1"
    noi_y1_effective <- noi_y1
    entry_yield_effective <- noi_y1 / price
    gei_y1_effective <- noi_y1_effective
    pbtcf_y1_effective <- noi_y1_effective - capex[1]
  } else if (has_leases) {
    if (!inherits(lease_roll, "cre_lease_roll")) {
      stop("deal_spec(): `lease_roll` must be created with lease_roll().")
    }
    lease_metrics <- .lease_roll_metrics(
      lease_roll = lease_roll,
      purchase_year = purchase_year,
      horizon_years = horizon_years,
      opex_sqm = opex_sqm
    )
    income_mode <- "lease_roll"
    area_sqm <- lease_metrics$area_sqm
    rent_sqm <- lease_metrics$rent_sqm
    vacancy_rate <- lease_metrics$vacancy_rate
    noi_y1_effective <- lease_metrics$noi_y1
    if (!is.finite(noi_y1_effective) || noi_y1_effective <= 0) {
      stop("deal_spec(): `lease_roll` implies a non-positive NOI_y1.")
    }
    entry_yield_effective <- noi_y1_effective / price
    gross_potential_rent_y1_effective <- lease_metrics$gross_potential_rent_y1
    gei_y1_effective <- lease_metrics$gei_y1
    pbtcf_y1_effective <- lease_metrics$pbtcf_y1
  } else {
    if (is.null(rent_sqm) || is.null(area_sqm)) {
      stop("deal_spec(): `rent_sqm` and `area_sqm` must be provided together.")
    }
    checkmate::assert_number(rent_sqm, lower = 0)
    checkmate::assert_number(area_sqm, lower = .Machine$double.eps)
    income_mode <- "rent_roll"
    noi_y1_effective <- rent_sqm * area_sqm * (1 - vacancy_rate) - (opex_sqm * area_sqm)
    if (!is.finite(noi_y1_effective) || noi_y1_effective <= 0) {
      stop("deal_spec(): rent/surface inputs imply a non-positive NOI_y1.")
    }
    entry_yield_effective <- noi_y1_effective / price
    gross_potential_rent_y1_effective <- rent_sqm * area_sqm
    gei_y1_effective <- gross_potential_rent_y1_effective * (1 - vacancy_rate)
    pbtcf_y1_effective <- noi_y1_effective - capex[1]
  }

  structure(
    list(
      price = price,
      horizon_years = as.integer(horizon_years),
      purchase_year = as.integer(purchase_year),
      income_mode = income_mode,
      entry_yield = entry_yield,
      noi_y1 = noi_y1,
      rent_sqm = rent_sqm,
      area_sqm = area_sqm,
      lease_roll = lease_roll,
      vacancy_rate = vacancy_rate,
      opex_sqm = opex_sqm,
      index_rate = index_rate,
      acq_cost_rate = acq_cost_rate,
      discount_rate = discount_rate,
      capex = as.numeric(capex),
      exit_yield_spread_bps = exit_yield_spread_bps,
      exit_cost = exit_cost,
      debt = debt,
      entry_yield_effective = entry_yield_effective,
      noi_y1_effective = noi_y1_effective,
      gross_potential_rent_y1_effective = gross_potential_rent_y1_effective,
      gei_y1_effective = gei_y1_effective,
      pbtcf_y1_effective = pbtcf_y1_effective
    ),
    class = c("cre_deal_spec", "list")
  )
}

#' Convert a simplified deal into an engine configuration
#'
#' @param deal Object returned by [deal_spec()].
#'
#' @return A configuration list compatible with [run_case()].
#' @export
deal_to_config <- function(deal) {
  if (!inherits(deal, "cre_deal_spec")) {
    stop("deal_to_config(): `deal` must be created with deal_spec().")
  }

  debt <- deal$debt
  cfg <- dcf_spec_template()
  cfg$purchase_year <- as.integer(deal$purchase_year)
  cfg$horizon_years <- as.integer(deal$horizon_years)
  cfg$index_rate <- deal$index_rate
  cfg$entry_yield <- deal$entry_yield_effective
  cfg$acq_cost_rate <- deal$acq_cost_rate
  cfg$exit_yield_spread_bps <- deal$exit_yield_spread_bps
  cfg$exit_cost <- deal$exit_cost
  cfg$disc_method <- "risk_premium"
  cfg$disc_rate_risk_premium <- list(rf = deal$discount_rate)
  cfg$ltv_init <- debt$ltv
  cfg$rate_annual <- debt$rate
  cfg$simple_debt_type <- debt$type
  cfg$extra_amort_pct <- debt$extra_amort_pct
  cfg$arrangement_fee_pct <- debt$arrangement_fee_pct
  cfg$capitalized_fees <- debt$capitalized_fees
  cfg$maturity <- debt$maturity %||% deal$horizon_years
  cfg$opex_sqm <- deal$opex_sqm
  cfg$acq_price_ht <- deal$price / (1 + deal$acq_cost_rate)
  cfg$ltv_base <- "price_di"
  cfg$simple_capex <- deal$capex
  cfg$simple_input_mode <- deal$income_mode
  cfg$vacancy_rate <- deal$vacancy_rate

  if (identical(deal$income_mode, "entry_yield")) {
    cfg$top_down_noi <- TRUE
    cfg$leases <- list()
  } else if (identical(deal$income_mode, "lease_roll")) {
    cfg$leases <- .lease_roll_units(deal$lease_roll)
  } else if (identical(deal$income_mode, "noi_y1")) {
    cfg$leases <- list(
      list(
        unit = "asset",
        area = 1,
        events = list(
          list(
            start = deal$purchase_year,
            end = deal$purchase_year + deal$horizon_years,
            rent = deal$noi_y1_effective,
            free_months = 0,
            capex_sqm = 0,
            vac = 0,
            new_lease = 0
          )
        )
      )
    )
  } else {
    cfg$leases <- list(
      list(
        unit = "asset",
        area = deal$area_sqm,
        events = list(
          list(
            start = deal$purchase_year,
            end = deal$purchase_year + deal$horizon_years,
            rent = deal$rent_sqm,
            free_months = 0,
            capex_sqm = 0,
            vac = deal$vacancy_rate,
            new_lease = 0
          )
        )
      )
    )
  }

  cfg
}

#' Analyze a simplified CRE deal
#'
#' @param deal Object returned by [deal_spec()].
#' @param debt_type Optional debt override passed to [run_case()].
#'
#' @return An object of class \code{cre_deal_result}.
#' @export
analyze_deal <- function(deal, debt_type = NULL) {
  if (!inherits(deal, "cre_deal_spec")) {
    stop("analyze_deal(): `deal` must be created with deal_spec().")
  }

  cfg <- deal_to_config(deal)
  out <- run_case(config = cfg, ltv_base = "price_di", debt_type = debt_type)
  out$deal <- deal
  out$simple_config <- cfg
  class(out) <- c("cre_deal_result", class(out))
  out
}

#' Extract standard cash-flow tables from a deal result
#'
#' @param x Object returned by [analyze_deal()] or [run_case()].
#' @param view One of \code{"full"}, \code{"operating"},
#'   \code{"all_equity"}, \code{"leveraged"}, or \code{"comparison"}.
#'
#' @return A data.frame or tibble.
#' @export
deal_cashflows <- function(x, view = c("full", "operating", "all_equity", "leveraged", "comparison")) {
  view <- match.arg(view)

  if (identical(view, "full")) {
    return(x$cashflows)
  }
  if (identical(view, "operating")) {
    cols <- intersect(
      c("year", "gei", "noi", "pbtcf", "opex", "capex", "asset_value", "sale_proceeds"),
      names(x$cashflows)
    )
    return(x$cashflows[, cols, drop = FALSE])
  }
  if (identical(view, "all_equity")) {
    return(x$all_equity$cashflows)
  }
  if (identical(view, "leveraged")) {
    return(x$leveraged$cashflows)
  }
  x$comparison$summary
}

.safe_num <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(x[[1]])
}

.fmt_num <- function(x, digits = 2) {
  if (!is.finite(x)) {
    return("NA")
  }
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

.fmt_pct <- function(x, digits = 2) {
  if (!is.finite(x)) {
    return("NA")
  }
  paste0(.fmt_num(100 * x, digits = digits), "%")
}

.asset_snapshot_from_deal <- function(x) {
  area <- .safe_num(x$area_sqm)
  rent <- .safe_num(x$rent_sqm)
  vacancy_rate <- .safe_num(x$vacancy_rate)
  opex_sqm <- .safe_num(x$opex_sqm)
  opex_y1 <- if (is.finite(area) && is.finite(opex_sqm)) area * opex_sqm else NA_real_
  gross_potential_rent_y1 <- if (!is.null(x$gross_potential_rent_y1_effective)) {
    .safe_num(x$gross_potential_rent_y1_effective)
  } else if (is.finite(area) && is.finite(rent)) {
    area * rent
  } else {
    NA_real_
  }

  gei_y1 <- if (!is.null(x$gei_y1_effective)) {
    .safe_num(x$gei_y1_effective)
  } else if (is.finite(gross_potential_rent_y1) && is.finite(vacancy_rate)) {
    gross_potential_rent_y1 * (1 - vacancy_rate)
  } else if (is.finite(x$noi_y1_effective) && is.finite(opex_y1)) {
    x$noi_y1_effective + opex_y1
  } else {
    NA_real_
  }

  tibble::tibble(
    income_mode = x$income_mode %||% NA_character_,
    purchase_year = .safe_num(x$purchase_year),
    price = .safe_num(x$price),
    horizon_years = .safe_num(x$horizon_years),
    area_sqm = area,
    price_per_sqm = if (is.finite(area) && area > 0) .safe_num(x$price) / area else NA_real_,
    rent_sqm = rent,
    vacancy_rate = vacancy_rate,
    opex_sqm = opex_sqm,
    gross_potential_rent_y1 = gross_potential_rent_y1,
    gei_y1 = gei_y1,
    noi_y1 = .safe_num(x$noi_y1_effective),
    pbtcf_y1 = if (!is.null(x$pbtcf_y1_effective)) .safe_num(x$pbtcf_y1_effective) else .safe_num(x$noi_y1_effective) - .safe_num(x$capex[1]),
    entry_yield = .safe_num(x$entry_yield_effective),
    index_rate = .safe_num(x$index_rate),
    discount_rate = .safe_num(x$discount_rate),
    debt_type = x$debt$type %||% NA_character_,
    debt_ltv = .safe_num(x$debt$ltv),
    debt_rate = .safe_num(x$debt$rate)
  )
}

.asset_snapshot_from_result <- function(x) {
  if (inherits(x$deal, "cre_deal_spec")) {
    out <- .asset_snapshot_from_deal(x$deal)
  } else {
    out <- tibble::tibble(
      income_mode = x$config$simple_input_mode %||% NA_character_,
      purchase_year = NA_real_,
      price = x$pricing$price_di %||% NA_real_,
      horizon_years = x$all_equity$cashflows$year[nrow(x$all_equity$cashflows)] %||% NA_real_,
      area_sqm = NA_real_,
      price_per_sqm = NA_real_,
      rent_sqm = NA_real_,
      vacancy_rate = x$config$vacancy_rate %||% NA_real_,
      opex_sqm = x$config$opex_sqm %||% NA_real_,
      gross_potential_rent_y1 = NA_real_,
      gei_y1 = NA_real_,
      noi_y1 = NA_real_,
      pbtcf_y1 = NA_real_,
      entry_yield = NA_real_,
      index_rate = NA_real_,
      discount_rate = x$config$disc_rate %||% NA_real_,
      debt_type = x$config$debt_type %||% NA_character_,
      debt_ltv = x$config$ltv_init %||% NA_real_,
      debt_rate = NA_real_
    )
  }

  year1 <- x$cashflows[x$cashflows$year == 1L, , drop = FALSE]
  if (nrow(year1) == 1L) {
    if ("gei" %in% names(year1)) out$gei_y1 <- year1$gei[[1]]
    if ("noi" %in% names(year1)) out$noi_y1 <- year1$noi[[1]]
    if ("pbtcf" %in% names(year1)) out$pbtcf_y1 <- year1$pbtcf[[1]]
  }

  out$price <- x$pricing$price_di %||% out$price
  if (!is.finite(out$price_per_sqm) && is.finite(out$area_sqm) && out$area_sqm > 0) {
    out$price_per_sqm <- out$price / out$area_sqm
  }
  out$horizon_years <- x$all_equity$cashflows$year[nrow(x$all_equity$cashflows)] %||% out$horizon_years
  out
}

#' Summarize a lease roll in analyst-friendly tabular form
#'
#' @param x Object returned by [lease_unit()] or [lease_roll()].
#'
#' @return A tibble with one row per lease unit.
#' @export
lease_roll_snapshot <- function(x) {
  if (inherits(x, "cre_lease_unit")) {
    return(.lease_unit_snapshot(x))
  }
  if (inherits(x, "cre_lease_roll")) {
    units <- .lease_roll_units(x)
    return(dplyr::bind_rows(lapply(units, function(u) .lease_unit_snapshot(u))))
  }
  stop("lease_roll_snapshot(): `x` must be created with lease_unit() or lease_roll().")
}

#' Summarize a simplified asset in one row
#'
#' @param x Object returned by [deal_spec()], [analyze_deal()], or [run_case()].
#'
#' @return A tibble with one row of asset, income, and financing assumptions.
#' @export
asset_snapshot <- function(x) {
  if (inherits(x, "cre_deal_spec")) {
    return(.asset_snapshot_from_deal(x))
  }
  if (inherits(x, "cre_deal_result") || is.list(x)) {
    return(.asset_snapshot_from_result(x))
  }
  stop("asset_snapshot(): `x` must be created with deal_spec(), analyze_deal(), or run_case().")
}

.deal_metrics_tbl <- function(x) {
  cmp <- x$comparison$summary
  bullet_row <- cmp[cmp$scenario == "debt_bullet", , drop = FALSE]
  if (nrow(bullet_row) == 0L) {
    bullet_row <- cmp[1L, , drop = FALSE]
  }

  snap <- asset_snapshot(x)

  tibble::as_tibble(
    cbind(
      snap,
      tibble::tibble(
        irr_project = x$all_equity$irr_project %||% NA_real_,
        irr_equity = x$leveraged$irr_equity %||% NA_real_,
        dscr_min = bullet_row$min_dscr %||% NA_real_,
        ltv_max_forward = bullet_row$max_ltv_forward %||% NA_real_,
        ops_share = x$all_equity$ops_share %||% NA_real_,
        tv_share = x$all_equity$tv_share %||% NA_real_
      )
    )
  )
}

#' @export
print.cre_deal_spec <- function(x, ...) {
  snap <- asset_snapshot(x)
  income_line <- switch(
    x$income_mode,
    entry_yield = sprintf("income mode: entry yield %.2f%%", 100 * x$entry_yield_effective),
    noi_y1 = sprintf("income mode: NOI_y1 %.2f", x$noi_y1_effective),
    lease_roll = sprintf(
      "income mode: lease roll with %d unit(s)",
      length(.lease_roll_units(x$lease_roll))
    ),
    rent_roll = sprintf(
      "income mode: %.2f sqm x %.2f rent/sqm",
      x$area_sqm %||% NA_real_,
      x$rent_sqm %||% NA_real_
    )
  )

  cat(
    "<cre_deal_spec>\n",
    sprintf("price_di: %.2f\n", x$price),
    if (is.finite(snap$area_sqm) && is.finite(snap$rent_sqm)) {
      sprintf(
        "asset: %s sqm | rent %s/sqm | vacancy %s | opex %s/sqm | price %s/sqm\n",
        .fmt_num(snap$area_sqm),
        .fmt_num(snap$rent_sqm),
        .fmt_pct(snap$vacancy_rate, digits = 1),
        .fmt_num(snap$opex_sqm),
        .fmt_num(snap$price_per_sqm)
      )
    } else {
      sprintf("asset: aggregated inputs (%s)\n", x$income_mode)
    },
    sprintf(
      "year-1: GEI %s | NOI %s | PBTCF %s | entry yield %s\n",
      .fmt_num(snap$gei_y1),
      .fmt_num(snap$noi_y1),
      .fmt_num(snap$pbtcf_y1),
      .fmt_pct(snap$entry_yield)
    ),
    sprintf("horizon: %d years\n", x$horizon_years),
    sprintf("%s\n", income_line),
    sprintf(
      "growth/discount: indexation %s | discount rate %s\n",
      .fmt_pct(x$index_rate),
      .fmt_pct(x$discount_rate)
    ),
    sprintf("debt: %s, LTV %.1f%%, rate %.2f%%\n", x$debt$type, 100 * x$debt$ltv, 100 * x$debt$rate),
    sep = ""
  )
  invisible(x)
}

#' @export
print.cre_deal_result <- function(x, ...) {
  cat("<cre_deal_result>\n")
  print(summary(x), ...)
  invisible(x)
}

#' @export
print.cre_lease_event <- function(x, ...) {
  cat(
    "<cre_lease_event>\n",
    sprintf("years: %d-%d\n", x$start, x$end),
    sprintf(
      "rent: %s/sqm | vacancy: %s | free months: %s | capex: %s/sqm | new lease: %s\n",
      .fmt_num(.safe_num(x$rent)),
      .fmt_pct(.safe_num(x$vac), digits = 1),
      .fmt_num(.safe_num(x$free_months), digits = 1),
      .fmt_num(.safe_num(x$capex_sqm)),
      if (isTRUE(as.logical(x$new_lease))) "yes" else "no"
    ),
    sep = ""
  )
  invisible(x)
}

#' @export
print.cre_lease_unit <- function(x, ...) {
  snap <- lease_roll_snapshot(x)
  cat(
    "<cre_lease_unit>\n",
    sprintf("unit: %s | area: %s sqm | events: %d\n",
            x$unit %||% "NA",
            .fmt_num(.safe_num(x$area), digits = 0),
            length(x$events %||% list())),
    sprintf(
      "timeline: %d-%d | start rent: %s/sqm | end rent: %s/sqm | relettings: %d\n",
      snap$first_start,
      snap$last_end,
      .fmt_num(snap$start_rent_sqm),
      .fmt_num(snap$end_rent_sqm),
      as.integer(snap$reletting_count)
    ),
    sep = ""
  )
  invisible(x)
}

#' @export
print.cre_lease_roll <- function(x, ...) {
  snap <- lease_roll_snapshot(x)
  cat(
    "<cre_lease_roll>\n",
    sprintf(
      "units: %d | area: %s sqm | timeline: %d-%d\n",
      nrow(snap),
      .fmt_num(sum(snap$area_sqm, na.rm = TRUE), digits = 0),
      min(snap$first_start, na.rm = TRUE),
      max(snap$last_end, na.rm = TRUE)
    ),
    sep = ""
  )
  print(snap, ...)
  invisible(x)
}

#' @export
summary.cre_lease_unit <- function(object, ...) {
  lease_roll_snapshot(object)
}

#' @export
summary.cre_lease_roll <- function(object, ...) {
  lease_roll_snapshot(object)
}

#' @export
summary.cre_deal_result <- function(object, ...) {
  .deal_metrics_tbl(object)
}
