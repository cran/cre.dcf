## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(cre.dcf)

deal <- deal_spec(
  price = 10e6,
  entry_yield = 0.055,
  horizon_years = 10,
  debt = debt_terms(
    ltv = 0.60,
    rate = 0.045,
    type = "bullet"
  )
)

res <- analyze_deal(deal)

summary(res)

## -----------------------------------------------------------------------------
deal <- deal_spec(
  price = 12e6,
  rent_sqm = 240,
  area_sqm = 4000,
  vacancy_rate = 0.08,
  opex_sqm = 15,
  horizon_years = 7,
  discount_rate = 0.08,
  debt = debt_terms(
    ltv = 0.55,
    rate = 0.043,
    type = "amort",
    maturity = 7
  )
)

deal

## -----------------------------------------------------------------------------
res <- analyze_deal(deal)
res

## -----------------------------------------------------------------------------
summary(res)

## -----------------------------------------------------------------------------
deal_cashflows(res, "comparison")
deal_cashflows(res, "full")

## -----------------------------------------------------------------------------
deal_spec(price = 10e6, entry_yield = 0.055)

## -----------------------------------------------------------------------------
deal_spec(price = 10e6, noi_y1 = 550000)

## -----------------------------------------------------------------------------
deal_spec(
  price = 10e6,
  rent_sqm = 220,
  area_sqm = 3000,
  vacancy_rate = 0.05,
  opex_sqm = 12
)

