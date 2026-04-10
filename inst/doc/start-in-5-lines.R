## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(cre.dcf)

deal <- deal_spec(
  price = 10e6,
  rent_sqm = 220,
  area_sqm = 3000,
  vacancy_rate = 0.08,
  opex_sqm = 18,
  horizon_years = 10,
  debt = debt_terms(ltv = 0.6, rate = 0.045, type = "bullet")
)

deal

## -----------------------------------------------------------------------------
res <- analyze_deal(deal)

res
summary(res)

## -----------------------------------------------------------------------------
asset_snapshot(res)
deal_cashflows(res, "operating")

## -----------------------------------------------------------------------------
deal_cashflows(res, "full")
deal_cashflows(res, "comparison")

## -----------------------------------------------------------------------------
deal_spec(price = 10e6, noi_y1 = 550000)

## -----------------------------------------------------------------------------
deal_spec(price = 10e6, entry_yield = 0.055)

## -----------------------------------------------------------------------------
roll <- lease_roll(list(
  lease_unit(
    "North",
    area_sqm = 1800,
    events = list(
      lease_event(start = 2025, end = 2027, rent = 230, vac = 0.10),
      lease_event(start = 2028, end = 2034, rent = 245, vac = 0, new_lease = TRUE, free_months = 3)
    )
  ),
  lease_unit(
    "South",
    area_sqm = 1200,
    events = list(
      lease_event(start = 2025, end = 2034, rent = 210, vac = 0.03)
    )
  )
))

deal_spec(
  price = 10e6,
  purchase_year = 2025,
  lease_roll = roll,
  opex_sqm = 18
)

