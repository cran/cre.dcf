## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(cre.dcf)

north <- lease_unit(
  "North",
  area_sqm = 1800,
  events = list(
    lease_event(start = 2025, end = 2027, rent = 230, vac = 0.10),
    vacancy_event(start = 2028, end = 2028, capex_sqm = 35),
    renewal_event(start = 2029, end = 2034, rent = 245, free_months = 3, capex_sqm = 20)
  )
)

south <- lease_unit(
  "South",
  area_sqm = 1200,
  events = list(
    lease_event(start = 2025, end = 2034, rent = 210, vac = 0.03)
  )
)

roll <- lease_roll(list(north, south))

north
roll
lease_roll_snapshot(roll)

## -----------------------------------------------------------------------------
deal <- deal_spec(
  price = 10e6,
  purchase_year = 2025,
  lease_roll = roll,
  opex_sqm = 18,
  debt = debt_terms(ltv = 0.60, rate = 0.045, type = "bullet")
)

deal
asset_snapshot(deal)

## -----------------------------------------------------------------------------
res <- analyze_deal(deal)

asset_snapshot(res)
summary(res)

## -----------------------------------------------------------------------------
ops <- deal_cashflows(res, "operating")
cmp <- deal_cashflows(res, "comparison")

ops
cmp

