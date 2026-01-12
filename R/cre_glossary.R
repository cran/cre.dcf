#' Glossary of CRE finance and modelling terms
#'
#' @description
#' Bilingual glossary (English/French) of the main commercial real estate
#' finance and discounted cash-flow modelling terms used in the package.
#' Definitions are intended to be short, operational and consistent with
#' the usage in vignettes and function documentation.
#'
#' @format A tibble with one row per term and the following columns:
#' \describe{
#'   \item{term_id}{Short, unique identifier used internally (e.g. "irr", "dscr").}
#'   \item{term_en}{Canonical English label.}
#'   \item{term_fr}{Canonical French label.}
#'   \item{definition_en}{Operational English definition (2–4 lines).}
#'   \item{definition_fr}{Operational French definition (2–4 lines).}
#'   \item{category}{High-level category (e.g. "discounted_cash_flow", "debt_metrics",
#'   "portfolio", "leasing").}
#'   \item{subcategory}{Optional subcategory (e.g. "return", "risk", "covenant").}
#'   \item{see_also}{Comma-separated list of related `term_id` values.}
#' }
#'
#' @seealso Vignette \code{vignette("glossary", package = "cre.dcf")}
"cre_glossary"
