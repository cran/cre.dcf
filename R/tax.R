#' Build a depreciation specification for a generic SPV tax engine
#'
#' @param acquisition_split Data frame describing the acquisition-price split.
#'   Required columns are \code{bucket}, \code{share}, \code{life_years},
#'   \code{method}, and \code{depreciable}. Shares should sum to 1.
#' @param capex_bucket Character scalar or \code{NULL}. Bucket used to assign
#'   recurring capital expenditures. If \code{NULL}, the first depreciable
#'   bucket is used.
#' @param start_rule Character scalar. Either \code{"full_year"} or
#'   \code{"next_year"}.
#'
#' @return A list of class \code{cre_tax_depreciation_spec}.
#'
#' @examples
#' dep <- depreciation_spec(
#'   acquisition_split = tibble::tribble(
#'     ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
#'     "land",      0.20,        NA,    "none",           FALSE,
#'     "building",  0.65,        30,    "straight_line",  TRUE,
#'     "fitout",    0.15,        10,    "straight_line",  TRUE
#'   ),
#'   capex_bucket = "fitout",
#'   start_rule = "full_year"
#' )
#' dep$capex_bucket
#' @export
depreciation_spec <- function(acquisition_split,
                              capex_bucket = NULL,
                              start_rule = c("full_year", "next_year")) {
  start_rule <- match.arg(start_rule)
  checkmate::assert_data_frame(acquisition_split, min.rows = 1)

  req <- c("bucket", "share", "life_years", "method", "depreciable")
  miss <- setdiff(req, names(acquisition_split))
  if (length(miss) > 0L) {
    stop(
      "depreciation_spec(): missing required columns: ",
      paste(miss, collapse = ", ")
    )
  }

  split_tbl <- tibble::as_tibble(acquisition_split)
  split_tbl$bucket <- as.character(split_tbl$bucket)
  split_tbl$share <- as.numeric(split_tbl$share)
  split_tbl$life_years <- as.numeric(split_tbl$life_years)
  split_tbl$method <- as.character(split_tbl$method)
  split_tbl$depreciable <- as.logical(split_tbl$depreciable)

  if (any(!is.finite(split_tbl$share)) || any(split_tbl$share < 0)) {
    stop("depreciation_spec(): `share` must be finite and non-negative.")
  }

  share_sum <- sum(split_tbl$share)
  if (abs(share_sum - 1) > 1e-8) {
    stop("depreciation_spec(): acquisition shares must sum to 1.")
  }

  dep_idx <- which(split_tbl$depreciable)
  if (length(dep_idx) == 0L) {
    stop("depreciation_spec(): at least one bucket must be depreciable.")
  }

  dep_methods <- unique(split_tbl$method[dep_idx])
  if (!all(dep_methods %in% "straight_line")) {
    stop("depreciation_spec(): only `straight_line` is supported for depreciable buckets.")
  }

  if (any(!is.finite(split_tbl$life_years[dep_idx])) || any(split_tbl$life_years[dep_idx] <= 0)) {
    stop("depreciation_spec(): depreciable buckets need positive `life_years`.")
  }

  non_dep_idx <- which(!split_tbl$depreciable)
  if (length(non_dep_idx) > 0L) {
    ok_non_dep <- is.na(split_tbl$life_years[non_dep_idx]) |
      (is.finite(split_tbl$life_years[non_dep_idx]) & split_tbl$life_years[non_dep_idx] > 0)
    if (!all(ok_non_dep)) {
      stop("depreciation_spec(): non-depreciable buckets need `life_years = NA` or a positive value.")
    }
  }

  if (is.null(capex_bucket)) {
    capex_bucket <- split_tbl$bucket[dep_idx][1L]
  }
  checkmate::assert_string(capex_bucket, min.chars = 1)

  if (!(capex_bucket %in% split_tbl$bucket)) {
    stop("depreciation_spec(): `capex_bucket` must match a bucket in `acquisition_split`.")
  }
  if (!isTRUE(split_tbl$depreciable[match(capex_bucket, split_tbl$bucket)])) {
    stop("depreciation_spec(): `capex_bucket` must refer to a depreciable bucket.")
  }

  structure(
    list(
      acquisition_split = split_tbl,
      capex_bucket = capex_bucket,
      start_rule = start_rule
    ),
    class = c("cre_tax_depreciation_spec", "list")
  )
}

#' Build an interest-deductibility rule for the generic SPV tax engine
#'
#' @param mode Character scalar. Currently only \code{"full"} is supported.
#'
#' @return A list of class \code{cre_tax_interest_rule}.
#'
#' @examples
#' interest_rule()
#' @export
interest_rule <- function(mode = c("full")) {
  mode <- match.arg(mode)
  structure(
    list(mode = mode),
    class = c("cre_tax_interest_rule", "list")
  )
}

#' Build a loss-carryforward rule for the generic SPV tax engine
#'
#' @param carryforward Logical scalar. Whether tax losses can be carried
#'   forward.
#' @param carryforward_years Numeric scalar. Number of future years for which a
#'   loss may be used. Use \code{Inf} for no expiry.
#' @param offset_cap_pct Numeric scalar in \code{[0, 1]}. Maximum fraction of a
#'   positive taxable base that can be offset by prior losses.
#'
#' @return A list of class \code{cre_tax_loss_rule}.
#'
#' @examples
#' loss_rule(carryforward = TRUE, carryforward_years = Inf, offset_cap_pct = 1)
#' @export
loss_rule <- function(carryforward = TRUE,
                      carryforward_years = Inf,
                      offset_cap_pct = 1) {
  checkmate::assert_flag(carryforward)
  if (is.infinite(carryforward_years)) {
    carryforward_years <- Inf
  } else {
    checkmate::assert_number(carryforward_years, lower = 0)
  }
  checkmate::assert_number(offset_cap_pct, lower = 0, upper = 1)

  structure(
    list(
      carryforward = carryforward,
      carryforward_years = carryforward_years,
      offset_cap_pct = offset_cap_pct
    ),
    class = c("cre_tax_loss_rule", "list")
  )
}

#' Build a generic SPV tax specification
#'
#' @param corp_tax_rate Numeric scalar in \code{[0, 1)}.
#' @param depreciation_spec Object returned by [depreciation_spec()].
#' @param interest_rule Object returned by [interest_rule()].
#' @param loss_rule Object returned by [loss_rule()].
#'
#' @return A list of class \code{cre_tax_spec_spv}.
#'
#' @examples
#' spec <- tax_spec_spv(corp_tax_rate = 0.25)
#' spec$corp_tax_rate
#' @export
tax_spec_spv <- function(
  corp_tax_rate = 0.25,
  depreciation_spec = NULL,
  interest_rule = NULL,
  loss_rule = NULL
) {
  checkmate::assert_number(corp_tax_rate, lower = 0, upper = 1)

  if (is.null(depreciation_spec)) {
    depreciation_spec <- get("depreciation_spec", mode = "function")(
      acquisition_split = tibble::tribble(
        ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
        "land",      0.20,        NA,    "none",           FALSE,
        "building",  0.80,        30,    "straight_line",  TRUE
      ),
      capex_bucket = "building",
      start_rule = "full_year"
    )
  }
  if (is.null(interest_rule)) {
    interest_rule <- get("interest_rule", mode = "function")()
  }
  if (is.null(loss_rule)) {
    loss_rule <- get("loss_rule", mode = "function")()
  }

  if (!inherits(depreciation_spec, "cre_tax_depreciation_spec")) {
    stop("tax_spec_spv(): `depreciation_spec` must be created with depreciation_spec().")
  }
  if (!inherits(interest_rule, "cre_tax_interest_rule")) {
    stop("tax_spec_spv(): `interest_rule` must be created with interest_rule().")
  }
  if (!inherits(loss_rule, "cre_tax_loss_rule")) {
    stop("tax_spec_spv(): `loss_rule` must be created with loss_rule().")
  }

  structure(
    list(
      corp_tax_rate = corp_tax_rate,
      depreciation_spec = depreciation_spec,
      interest_rule = interest_rule,
      loss_rule = loss_rule
    ),
    class = c("cre_tax_spec_spv", "list")
  )
}

#' Extract a tax basis from a pre-tax case
#'
#' @param x Object returned by [run_case()] or [analyze_deal()].
#' @param acquisition_basis Character scalar. Either \code{"price_ht"} or
#'   \code{"price_di"}.
#'
#' @return A tibble with the minimal fields consumed by [tax_run_spv()].
#'
#' @examples
#' deal <- deal_spec(
#'   price = 10e6,
#'   entry_yield = 0.055,
#'   horizon_years = 5,
#'   debt = debt_terms(ltv = 0.5, rate = 0.04, type = "bullet")
#' )
#' res <- analyze_deal(deal)
#' tax_basis_spv(res)
#' @export
tax_basis_spv <- function(x, acquisition_basis = c("price_ht", "price_di")) {
  acquisition_basis <- match.arg(acquisition_basis)

  if (!is.list(x) || is.null(x$cashflows) || is.null(x$pricing)) {
    stop("tax_basis_spv(): `x` must be a result returned by run_case() or analyze_deal().")
  }

  cf <- tibble::as_tibble(x$cashflows)
  req <- c("year", "noi", "capex", "interest", "sale_proceeds")
  miss <- setdiff(req, names(cf))
  if (length(miss) > 0L) {
    stop("tax_basis_spv(): missing columns in `x$cashflows`: ", paste(miss, collapse = ", "))
  }

  acq_price <- switch(
    acquisition_basis,
    price_ht = x$pricing$price_ht,
    price_di = x$pricing$price_di
  )
  checkmate::assert_number(acq_price, lower = 0)

  pre_tax_equity_cf <- if ("equity_flow" %in% names(cf)) {
    as.numeric(cf$equity_flow)
  } else if ("equity_cf" %in% names(cf)) {
    as.numeric(cf$equity_cf)
  } else {
    rep(NA_real_, nrow(cf))
  }

  tibble::tibble(
    year = as.integer(cf$year),
    noi = as.numeric(cf$noi),
    capex = as.numeric(cf$capex),
    interest = as.numeric(cf$interest),
    sale_proceeds = as.numeric(cf$sale_proceeds),
    taxable_sale_proceeds = as.numeric(cf$sale_proceeds),
    pre_tax_equity_cf = pre_tax_equity_cf,
    acquisition_price = rep(acq_price, nrow(cf))
  )
}

.tax_require_basis <- function(tax_basis) {
  checkmate::assert_data_frame(tax_basis, min.rows = 1)

  req <- c("year", "noi", "capex", "interest")
  miss <- setdiff(req, names(tax_basis))
  if (length(miss) > 0L) {
    stop("tax_run_spv(): missing required columns in `tax_basis`: ", paste(miss, collapse = ", "))
  }

  basis <- tibble::as_tibble(tax_basis)
  basis$year <- as.integer(basis$year)
  basis$noi <- as.numeric(basis$noi)
  basis$capex <- as.numeric(basis$capex)
  basis$interest <- as.numeric(basis$interest)

  if (anyDuplicated(basis$year)) {
    stop("tax_run_spv(): `tax_basis$year` must contain unique annual periods.")
  }

  basis <- basis[order(basis$year), , drop = FALSE]

  if (!("sale_proceeds" %in% names(basis))) {
    basis$sale_proceeds <- 0
  }
  if (!("taxable_sale_proceeds" %in% names(basis))) {
    basis$taxable_sale_proceeds <- basis$sale_proceeds
  }
  if (!("pre_tax_equity_cf" %in% names(basis))) {
    if ("equity_flow" %in% names(basis)) {
      basis$pre_tax_equity_cf <- basis$equity_flow
    } else if ("equity_cf" %in% names(basis)) {
      basis$pre_tax_equity_cf <- basis$equity_cf
    } else {
      basis$pre_tax_equity_cf <- NA_real_
    }
  }

  basis$sale_proceeds <- as.numeric(basis$sale_proceeds)
  basis$taxable_sale_proceeds <- as.numeric(basis$taxable_sale_proceeds)
  basis$pre_tax_equity_cf <- as.numeric(basis$pre_tax_equity_cf)

  if (any(!is.finite(basis$noi))) {
    stop("tax_run_spv(): `noi` must be finite.")
  }
  if (any(!is.finite(basis$capex)) || any(basis$capex < 0)) {
    stop("tax_run_spv(): `capex` must be finite and non-negative.")
  }
  if (any(!is.finite(basis$interest)) || any(basis$interest < 0)) {
    stop("tax_run_spv(): `interest` must be finite and non-negative.")
  }

  n_sales <- sum(abs(basis$sale_proceeds) > 1e-8, na.rm = TRUE)
  if (n_sales > 1L) {
    stop("tax_run_spv(): only one sale event is supported in version 1.")
  }

  basis
}

.tax_get_acquisition_price <- function(tax_basis, acquisition_price = NULL) {
  if (!is.null(acquisition_price)) {
    checkmate::assert_number(acquisition_price, lower = 0)
    return(as.numeric(acquisition_price))
  }

  candidates <- c("acquisition_price", "asset_cost", "price_ht", "price_di")
  avail <- intersect(candidates, names(tax_basis))
  if (length(avail) == 0L) {
    return(NA_real_)
  }

  vals <- unlist(lapply(avail, function(nm) tax_basis[[nm]]), use.names = FALSE)
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0L) {
    return(NA_real_)
  }

  unique_vals <- unique(as.numeric(vals))
  if (length(unique_vals) > 1L) {
    stop("tax_run_spv(): inconsistent acquisition-price values found in `tax_basis`.")
  }

  unique_vals[1L]
}

.tax_acquisition_start_year <- function(years) {
  years <- sort(unique(as.integer(years)))
  if (length(years) == 0L) {
    stop("tax_run_spv(): empty `year` vector.")
  }
  if (years[1L] == 0L && length(years) >= 2L) {
    return(years[2L])
  }
  years[1L]
}

.tax_vintage_depreciation <- function(years, basis, start_year, life_years, start_rule) {
  dep <- rep(0, length(years))
  if (!is.finite(basis) || basis <= 0 || !is.finite(life_years) || life_years <= 0) {
    return(dep)
  }

  annual_dep <- basis / life_years
  first_dep_year <- if (identical(start_rule, "next_year")) start_year + 1L else start_year

  active <- years >= first_dep_year & years < (first_dep_year + life_years)
  dep[active] <- annual_dep
  dep
}

.tax_build_depreciation_path <- function(years, tax_basis, tax_spec, acquisition_price) {
  dep_spec <- tax_spec$depreciation_spec
  split_tbl <- dep_spec$acquisition_split
  dep_path <- rep(0, length(years))

  dep_start_year <- .tax_acquisition_start_year(years)
  if (isTRUE(any(split_tbl$depreciable)) &&
      sum(split_tbl$share[split_tbl$depreciable]) > 0 &&
      !is.finite(acquisition_price)) {
    stop(
      "tax_run_spv(): an acquisition price is required to compute tax depreciation. ",
      "Provide `acquisition_price` or include an acquisition-price column in `tax_basis`."
    )
  }

  for (i in seq_len(nrow(split_tbl))) {
    row <- split_tbl[i, , drop = FALSE]
    if (!isTRUE(row$depreciable[[1]])) {
      next
    }
    dep_path <- dep_path + .tax_vintage_depreciation(
      years = years,
      basis = acquisition_price * row$share[[1]],
      start_year = dep_start_year,
      life_years = row$life_years[[1]],
      start_rule = dep_spec$start_rule
    )
  }

  capex_bucket_row <- split_tbl[match(dep_spec$capex_bucket, split_tbl$bucket), , drop = FALSE]
  for (i in seq_along(years)) {
    capex_i <- tax_basis$capex[i]
    year_i <- years[i]
    if (is.finite(capex_i) && capex_i > 0) {
      dep_path <- dep_path + .tax_vintage_depreciation(
        years = years,
        basis = capex_i,
        start_year = year_i,
        life_years = capex_bucket_row$life_years[[1]],
        start_rule = dep_spec$start_rule
      )
    }
  }

  dep_path
}

.tax_apply_losses <- function(taxable_income_pre_losses, years, loss_rule) {
  n <- length(years)
  loss_cf_open <- numeric(n)
  loss_cf_used <- numeric(n)
  loss_generated <- numeric(n)
  loss_cf_close <- numeric(n)
  taxable_income_post_losses <- numeric(n)

  pools_amount <- numeric(0)
  pools_expiry <- numeric(0)

  for (i in seq_len(n)) {
    year_i <- years[i]
    ti_pre <- taxable_income_pre_losses[i]

    if (length(pools_amount) > 0L) {
      keep <- pools_expiry >= year_i
      pools_amount <- pools_amount[keep]
      pools_expiry <- pools_expiry[keep]
    }

    loss_cf_open[i] <- sum(pools_amount)

    if (isTRUE(loss_rule$carryforward) && ti_pre > 0 && loss_cf_open[i] > 0) {
      max_offset <- ti_pre * loss_rule$offset_cap_pct
      remaining <- min(loss_cf_open[i], max_offset)
      if (remaining > 0) {
        for (j in seq_along(pools_amount)) {
          take <- min(pools_amount[j], remaining)
          pools_amount[j] <- pools_amount[j] - take
          remaining <- remaining - take
          loss_cf_used[i] <- loss_cf_used[i] + take
          if (remaining <= 1e-12) break
        }
        keep_after <- pools_amount > 1e-12
        pools_amount <- pools_amount[keep_after]
        pools_expiry <- pools_expiry[keep_after]
      }
    }

    taxable_income_post_losses[i] <- max(ti_pre - loss_cf_used[i], 0)

    if (isTRUE(loss_rule$carryforward) && ti_pre < 0) {
      loss_generated[i] <- -ti_pre
      expiry_year <- if (is.infinite(loss_rule$carryforward_years)) {
        Inf
      } else {
        year_i + loss_rule$carryforward_years
      }
      pools_amount <- c(pools_amount, loss_generated[i])
      pools_expiry <- c(pools_expiry, expiry_year)
    }

    loss_cf_close[i] <- sum(pools_amount)
  }

  list(
    loss_cf_open = loss_cf_open,
    loss_cf_used = loss_cf_used,
    loss_generated = loss_generated,
    loss_cf_close = loss_cf_close,
    taxable_income_post_losses = taxable_income_post_losses
  )
}

#' Run a generic SPV-level tax engine
#'
#' @param tax_basis Data frame with at least \code{year}, \code{noi},
#'   \code{capex}, and \code{interest}. Optional columns include
#'   \code{sale_proceeds}, \code{taxable_sale_proceeds},
#'   \code{pre_tax_equity_cf}, and \code{acquisition_price}.
#' @param tax_spec Object returned by [tax_spec_spv()].
#' @param acquisition_price Optional numeric scalar. Acquisition tax basis used
#'   for the initial asset split. If \code{NULL}, the function tries to infer it
#'   from \code{tax_basis}.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{tax_table}: yearly tax table,
#'     \item \code{summary}: one-row tibble with headline tax aggregates,
#'     \item \code{tax_spec}: the specification used for the run,
#'     \item \code{acquisition_price}: acquisition basis actually used.
#'   }
#'
#' @examples
#' basis <- tibble::tibble(
#'   year = 0:4,
#'   noi = c(0, 140, 150, 160, 170),
#'   capex = c(0, 0, 20, 0, 0),
#'   interest = c(0, 30, 25, 20, 0),
#'   sale_proceeds = c(0, 0, 0, 0, 900),
#'   pre_tax_equity_cf = c(-1000, 110, 105, 120, 950)
#' )
#'
#' spec <- tax_spec_spv(
#'   corp_tax_rate = 0.25,
#'   depreciation_spec = depreciation_spec(
#'     acquisition_split = tibble::tribble(
#'       ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
#'       "land",      0.20,        NA,    "none",           FALSE,
#'       "building",  0.80,         4,    "straight_line",  TRUE
#'     ),
#'     capex_bucket = "building",
#'     start_rule = "full_year"
#'   )
#' )
#'
#' out <- tax_run_spv(basis, spec, acquisition_price = 1000)
#' out$summary
#' @export
tax_run_spv <- function(tax_basis, tax_spec, acquisition_price = NULL) {
  if (!inherits(tax_spec, "cre_tax_spec_spv")) {
    stop("tax_run_spv(): `tax_spec` must be created with tax_spec_spv().")
  }
  if (!identical(tax_spec$interest_rule$mode, "full")) {
    stop("tax_run_spv(): only `interest_rule(mode = \"full\")` is supported in version 1.")
  }

  basis <- .tax_require_basis(tax_basis)
  years <- basis$year
  acq_price <- .tax_get_acquisition_price(basis, acquisition_price = acquisition_price)

  tax_depreciation <- .tax_build_depreciation_path(
    years = years,
    tax_basis = basis,
    tax_spec = tax_spec,
    acquisition_price = acq_price
  )

  deductible_interest <- basis$interest
  interest_disallowed <- basis$interest - deductible_interest

  book_value_open <- numeric(length(years))
  book_value_close_pre_exit <- numeric(length(years))
  book_value_close <- numeric(length(years))
  taxable_exit_gain_loss <- numeric(length(years))

  running_book_value <- if (is.finite(acq_price)) acq_price else 0
  for (i in seq_along(years)) {
    book_value_open[i] <- running_book_value
    book_value_close_pre_exit[i] <- running_book_value + basis$capex[i] - tax_depreciation[i]
    if (abs(basis$taxable_sale_proceeds[i]) > 1e-8) {
      taxable_exit_gain_loss[i] <- basis$taxable_sale_proceeds[i] - book_value_close_pre_exit[i]
      book_value_close[i] <- 0
      running_book_value <- 0
    } else {
      book_value_close[i] <- book_value_close_pre_exit[i]
      running_book_value <- book_value_close[i]
    }
  }

  taxable_income_pre_losses <- basis$noi - tax_depreciation - deductible_interest + taxable_exit_gain_loss

  loss_res <- .tax_apply_losses(
    taxable_income_pre_losses = taxable_income_pre_losses,
    years = years,
    loss_rule = tax_spec$loss_rule
  )

  cash_is <- loss_res$taxable_income_post_losses * tax_spec$corp_tax_rate
  after_tax_equity_cf <- if (all(is.na(basis$pre_tax_equity_cf))) {
    rep(NA_real_, length(years))
  } else {
    basis$pre_tax_equity_cf - cash_is
  }

  tax_table <- tibble::tibble(
    year = years,
    noi = basis$noi,
    capex = basis$capex,
    interest = basis$interest,
    sale_proceeds = basis$sale_proceeds,
    taxable_sale_proceeds = basis$taxable_sale_proceeds,
    pre_tax_equity_cf = basis$pre_tax_equity_cf,
    book_value_open = book_value_open,
    tax_depreciation = tax_depreciation,
    deductible_interest = deductible_interest,
    interest_disallowed = interest_disallowed,
    book_value_close_pre_exit = book_value_close_pre_exit,
    taxable_exit_gain_loss = taxable_exit_gain_loss,
    taxable_income_pre_losses = taxable_income_pre_losses,
    loss_cf_open = loss_res$loss_cf_open,
    loss_cf_used = loss_res$loss_cf_used,
    loss_generated = loss_res$loss_generated,
    loss_cf_close = loss_res$loss_cf_close,
    taxable_income_post_losses = loss_res$taxable_income_post_losses,
    cash_is = cash_is,
    after_tax_equity_cf = after_tax_equity_cf,
    book_value_close = book_value_close
  )

  summary_tbl <- tibble::tibble(
    corp_tax_rate = tax_spec$corp_tax_rate,
    acquisition_price = acq_price,
    total_tax_depreciation = sum(tax_table$tax_depreciation, na.rm = TRUE),
    total_cash_is = sum(tax_table$cash_is, na.rm = TRUE),
    total_loss_generated = sum(tax_table$loss_generated, na.rm = TRUE),
    final_loss_cf = utils::tail(tax_table$loss_cf_close, 1L),
    total_after_tax_equity_cf = if (all(is.na(tax_table$after_tax_equity_cf))) {
      NA_real_
    } else {
      sum(tax_table$after_tax_equity_cf, na.rm = TRUE)
    }
  )

  structure(
    list(
      tax_table = tax_table,
      summary = summary_tbl,
      tax_spec = tax_spec,
      acquisition_price = acq_price
    ),
    class = c("cre_tax_run_spv", "list")
  )
}
