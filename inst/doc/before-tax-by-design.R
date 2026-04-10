## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(cre.dcf)
library(dplyr)
library(tibble)

## -----------------------------------------------------------------------------
cfg_path <- system.file("extdata", "preset_core.yml", package = "cre.dcf")
cfg <- yaml::read_yaml(cfg_path)
case <- run_case(cfg)

tax_basis_preview <- case$cashflows |>
  select(year, gei, noi, pbtcf, capex, interest, sale_proceeds, equity_flow) |>
  filter(year <= 4 | year == max(year))

knitr::kable(
  tax_basis_preview,
  digits = 0,
  caption = "Current outputs that can feed a future SPV-level tax layer"
)

## -----------------------------------------------------------------------------
tpl <- dcf_spec_template()

tibble(
  KE = tpl$disc_rate_wacc$KE,
  KD = tpl$disc_rate_wacc$KD,
  tax_rate = tpl$disc_rate_wacc$tax_rate
)

## -----------------------------------------------------------------------------
future_tax_blocks <- tibble::tribble(
  ~block, ~consumes_from_core, ~adds_from_tax_spec, ~target_output,
  "Tax depreciation", "price, capex, holding period", "asset split, depreciation lives, start rule", "tax_depreciation",
  "Interest deductibility", "interest", "deductibility rule", "deductible_interest, interest_disallowed",
  "Simple corporate tax", "taxable base after adjustments", "statutory rate", "cash_is",
  "Loss carryforwards", "negative taxable income", "carryforward rule", "loss_cf_open, loss_cf_used, loss_cf_close"
)

knitr::kable(
  future_tax_blocks,
  caption = "Target blocks for a future SPV-level tax layer"
)

## -----------------------------------------------------------------------------
tax_basis <- tax_basis_spv(case)

tax_spec <- tax_spec_spv(
  corp_tax_rate = 0.25,
  depreciation_spec = depreciation_spec(
    acquisition_split = tibble::tribble(
      ~bucket,    ~share, ~life_years, ~method,          ~depreciable,
      "land",      0.20,        NA,    "none",           FALSE,
      "building",  0.65,        30,    "straight_line",  TRUE,
      "fitout",    0.15,        10,    "straight_line",  TRUE
    ),
    capex_bucket = "fitout",
    start_rule = "full_year"
  ),
  interest_rule = interest_rule(mode = "full"),
  loss_rule = loss_rule(carryforward = TRUE, carryforward_years = Inf)
)

tax_res <- tax_run_spv(tax_basis, tax_spec)

tax_res$tax_table |>
  select(
    year, noi, tax_depreciation, deductible_interest,
    taxable_income_pre_losses, loss_cf_open, loss_cf_used,
    cash_is, after_tax_equity_cf
  ) |>
  filter(year <= 4 | year == max(year))

## -----------------------------------------------------------------------------
tax_res$summary

