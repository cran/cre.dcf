## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(cre.dcf)
library(dplyr)
library(tibble)

## -----------------------------------------------------------------------------
dcf <- dcf_calculate(
  acq_price = 10e6,
  entry_yield = 0.055,
  exit_yield = 0.0575,
  horizon_years = 5,
  disc_rate = 0.075,
  opex = c(50000, 51000, 52020, 53060, 54121),
  capex = c(15000, 15000, 20000, 15000, 0)
)

dcf$cashflows |>
  select(year, gei, noi, pbtcf, sale_proceeds, free_cash_flow)

## -----------------------------------------------------------------------------
metrics <- compute_unleveraged_metrics(dcf)

tibble(
  pv_operations = metrics$pv_operations,
  pv_terminal = metrics$pv_terminal,
  ops_share = metrics$ops_share,
  tv_share = metrics$tv_share
)

## -----------------------------------------------------------------------------
lease_compare <- bind_rows(
  concession = lease_effective_rent(
    cashflows = c(0, 100, 100, 100, 100),
    discount_rate = 0.08,
    area = 1,
    timing = "arrears",
    perspective = "landlord"
  ),
  flat = lease_effective_rent(
    cashflows = c(90, 90, 90, 90, 90),
    discount_rate = 0.08,
    area = 1,
    timing = "arrears",
    perspective = "landlord"
  ),
  .id = "lease"
)

lease_compare |>
  select(lease, pv, equivalent_annuity, effective_rent)

## -----------------------------------------------------------------------------
uw <- underwrite_loan(
  noi = 500000,
  value = 8e6,
  rate_annual = 0.045,
  maturity = 5,
  type = "bullet",
  dscr_min = 1.25,
  ltv_max = 0.65,
  debt_yield_min = 0.08
)

uw$constraints

tibble(
  binding_constraint = uw$binding_constraint,
  max_loan = uw$max_loan,
  payment_year1 = uw$payment_year1,
  implied_ltv = uw$implied_ltv,
  implied_dscr = uw$implied_dscr,
  implied_debt_yield = uw$implied_debt_yield
)

