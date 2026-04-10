## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
echo    = FALSE,
message = FALSE,
warning = FALSE
)

library(cre.dcf)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(yaml)
library(tibble)

## -----------------------------------------------------------------------------

config_path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
stopifnot(nzchar(config_path), file.exists(config_path))

cfg_default <- yaml::read_yaml(config_path)

str(cfg_default, max.level = 1)

## -----------------------------------------------------------------------------
case <- run_case(cfg_default)

names(case)

## -----------------------------------------------------------------------------
pricing     <- case$pricing
all_equity  <- case$all_equity
leveraged   <- case$leveraged
comparison  <- case$comparison
cf_full     <- case$cashflows
cfg_finance <- case$config

## -----------------------------------------------------------------------------
pricing

## -----------------------------------------------------------------------------
cfg_finance

## -----------------------------------------------------------------------------
tab_capital_structure <- tibble::tibble(
item   = c("Acquisition price (DI)", "Initial debt", "Initial equity", "Initial LTV"),
amount = c(
pricing$price_di,
cfg_finance$debt_init,
cfg_finance$equity_init,
cfg_finance$ltv_init
)
)

tab_capital_structure

## -----------------------------------------------------------------------------
all_equity$cashflows

## -----------------------------------------------------------------------------
cf_ae <- all_equity$cashflows

ggplot(cf_ae, aes(x = factor(year))) +
geom_col(aes(y = free_cash_flow)) +
geom_point(aes(y = sale_proceeds)) +
labs(
x = "Year",
y = "Amount (EUR)",
title = "All-equity free cash-flows and sale proceeds"
)

## -----------------------------------------------------------------------------
all_equity[c("irr_project", "npv_project")]

## -----------------------------------------------------------------------------
all_equity[c("ops_share", "tv_share")]

## -----------------------------------------------------------------------------
comparison$summary

## -----------------------------------------------------------------------------
tab_summary <- comparison$summary %>%
mutate(
irr_equity    = percent(irr_equity, accuracy = 0.01),
irr_project   = percent(irr_project, accuracy = 0.01),
npv_equity    = comma(npv_equity, accuracy = 1),
npv_project   = comma(npv_project, accuracy = 1),
min_dscr      = round(min_dscr, 3),
max_ltv_fwd   = percent(max_ltv_forward, accuracy = 0.1)
)

tab_summary


## -----------------------------------------------------------------------------
sch_bullet <- comparison$details$debt_bullet$schedule
sch_amort  <- comparison$details$debt_amort$schedule

sch_bullet
sch_amort

## -----------------------------------------------------------------------------
rat_bullet <- comparison$details$debt_bullet$ratios
rat_amort  <- comparison$details$debt_amort$ratios

dplyr::select(rat_bullet, year, dscr, ltv_forward) %>% head()
dplyr::select(rat_amort,  year, dscr, ltv_forward) %>% head()


## -----------------------------------------------------------------------------
rat_long <- bind_rows(
rat_bullet  %>% mutate(structure = "Bullet"),
rat_amort   %>% mutate(structure = "Amortising")
) %>%
filter(year >= 1)

rat_long_dscr <- rat_long %>%
select(year, structure, dscr) %>%
filter(is.finite(dscr))

rat_long_ltv <- rat_long %>%
select(year, structure, ltv_forward) %>%
filter(is.finite(ltv_forward))


ggplot(rat_long_dscr, aes(x = year, y = dscr, group = structure)) +
geom_line() +
geom_point() +
facet_wrap(~ structure) +
geom_hline(yintercept = 1.25, linetype = "dashed") +
labs(
x = "Year",
y = "DSCR (x)",
title = "Debt service coverage ratio by structure",
subtitle = "Dashed line: illustrative DSCR guardrail at 1.25x"
)

## -----------------------------------------------------------------------------
ggplot(rat_long_ltv, aes(x = year, y = ltv_forward, group = structure)) +
geom_line() +
geom_point() +
facet_wrap(~ structure) +
geom_hline(yintercept = 0.65, linetype = "dashed") +
labs(
x = "Year",
y = "Forward LTV",
title = "Forward LTV by structure",
subtitle = "Dashed line: illustrative maximum forward LTV at 65%"
)

## -----------------------------------------------------------------------------
leveraged$cashflows

## -----------------------------------------------------------------------------
cf_lev <- leveraged$cashflows

ggplot(cf_lev, aes(x = factor(year), y = equity_cf)) +
geom_col() +
labs(
x = "Year",
y = "Equity cash-flow (EUR)",
title = "Leveraged equity cash-flow profile (default structure)"
)

## -----------------------------------------------------------------------------
leveraged[c("irr_equity", "npv_equity")]

## -----------------------------------------------------------------------------
em <- equity_multiple_safe(cf_lev$equity_cf)
em

## -----------------------------------------------------------------------------
# Normalised configuration (same logic as in run_case())
norm <- cfg_normalize(cfg_default)

# Acquisition base consistent with the case object
ltv_base_used <- case$config$ltv_base

acq_price_scen <- switch(
  ltv_base_used,
  "price_di" = norm$acq_price_di,
  "price_ht" = norm$acq_price_ht,
  "value"    = {
    stopifnot(!is.null(norm$noi_vec), length(norm$noi_vec) >= 1L)
    norm$noi_vec[1] / cfg_default$entry_yield
  }
)

# Rebuild the unlevered DCF result for scenario reuse
dcf_res_scen <- dcf_calculate(
  acq_price          = acq_price_scen,
  entry_yield        = cfg_default$entry_yield,
  exit_yield         = norm$exit_yield,
  horizon_years      = length(norm$noi_vec),
  disc_rate          = norm$disc_rate,
  exit_cost          = norm$exit_cost,
  capex              = norm$capex_vec,
  opex               = norm$opex_vec,
  noi                = norm$noi_vec
)

maturity_scen <- norm$maturity

## -----------------------------------------------------------------------------
library(tibble)

scenarios <- tibble(
scenario_id = c(
"eq_100",
"ltv30_bullet_2",
"ltv70_bullet_3",
"ltv70_amort_2_5"
),
label = c(
"100% equity (no leverage)",
"30% LTV, bullet, 2%",
"70% LTV, bullet, 3%",
"70% LTV, amortising, 2.5%"
),
ltv   = c(0.00, 0.30, 0.70, 0.70),
rate  = c(0.00, 0.02, 0.03, 0.025),
type  = c(NA_character_, "bullet", "bullet", "amort")
)

# Small helper to pick the relevant row from compare_financing_scenarios()

extract_row <- function(res, type) {
  if (is.na(type) || type == "all_equity") {
  dplyr::filter(res$summary, scenario == "all_equity")
  } else if (type == "bullet") {
  dplyr::filter(res$summary, scenario == "debt_bullet")
  } else {
  dplyr::filter(res$summary, scenario == "debt_amort")
  }
}

## -----------------------------------------------------------------------------
rows <- lapply(seq_len(nrow(scenarios)), function(i) {
sc <- scenarios[i, ]

if (sc$ltv == 0) {
# Pure all-equity case: reuse unlevered metrics
tibble(
scenario_id   = sc$scenario_id,
label         = sc$label,
ltv           = sc$ltv,
rate          = sc$rate,
structure     = "all_equity",
irr_equity    = all_equity$irr_project,
npv_equity    = all_equity$npv_project,
irr_project   = all_equity$irr_project,
npv_project   = all_equity$npv_project,
min_dscr      = NA_real_,
max_ltv_fwd   = NA_real_
)
} else {
comp <- compare_financing_scenarios(
dcf_res  = dcf_res_scen,
acq_price = acq_price_scen,
ltv       = sc$ltv,
rate      = sc$rate,
maturity  = maturity_scen
)

row_summary <- extract_row(comp, sc$type)

tibble(
  scenario_id   = sc$scenario_id,
  label         = sc$label,
  ltv           = sc$ltv,
  rate          = sc$rate,
  structure     = ifelse(is.na(sc$type), "all_equity", sc$type),
  irr_equity    = row_summary$irr_equity,
  npv_equity    = row_summary$npv_equity,
  irr_project   = row_summary$irr_project,
  npv_project   = row_summary$npv_project,
  min_dscr      = row_summary$min_dscr,
  max_ltv_fwd   = row_summary$max_ltv_forward
)

}
})

tab_financing_raw <- dplyr::bind_rows(rows)
tab_financing_raw

## -----------------------------------------------------------------------------
tab_financing <- tab_financing_raw %>%
dplyr::mutate(
ltv         = percent(ltv, accuracy = 1),
rate        = ifelse(rate > 0, percent(rate, accuracy = 0.1), "n/a"),
irr_equity  = percent(irr_equity, accuracy = 0.01),
irr_project = percent(irr_project, accuracy = 0.01),
npv_equity  = comma(npv_equity, accuracy = 1),
npv_project = comma(npv_project, accuracy = 1),
min_dscr    = round(min_dscr, 3),
max_ltv_fwd = dplyr::if_else(
is.finite(max_ltv_fwd),
percent(max_ltv_fwd, accuracy = 0.1),
NA_character_
)
)

tab_financing

## -----------------------------------------------------------------------------
# All-equity metrics

irr_proj_ae <- all_equity$irr_project
npv_proj_ae <- all_equity$npv_project

# Leveraged metrics (bullet and amortising structures)

irr_eq_lev_bullet <- leveraged$irr_equity
npv_eq_lev_bullet <- leveraged$npv_equity

# Add results for the 70% LTV scenarios (bullet and amortising) to the table

irr_eq_lev_bullet_70 <- comparison$details$debt_bullet$metrics$irr_equity[comparison$details$debt_bullet$metrics$scenario == "levered"]
npv_eq_lev_bullet_70 <- comparison$details$debt_bullet$metrics$npv_equity[comparison$details$debt_bullet$metrics$scenario == "levered"]

irr_eq_lev_amort_70 <- comparison$details$debt_amort$metrics$irr_equity[comparison$details$debt_amort$metrics$scenario == "levered"]
npv_eq_lev_amort_70 <- comparison$details$debt_amort$metrics$npv_equity[comparison$details$debt_amort$metrics$scenario == "levered"]

# Credit metrics for the bullet case and amortising case

rat_bullet <- rat_bullet
min_dscr_bullet <- suppressWarnings(min(rat_bullet$dscr[rat_bullet$year >= 1], na.rm = TRUE))
max_ltv_bullet  <- suppressWarnings(max(rat_bullet$ltv_forward[rat_bullet$year >= 1], na.rm = TRUE))

rat_amort <- rat_amort
min_dscr_amort <- suppressWarnings(min(rat_amort$dscr[rat_amort$year >= 1], na.rm = TRUE))
max_ltv_amort <- suppressWarnings(max(rat_amort$ltv_forward[rat_amort$year >= 1], na.rm = TRUE))

# Combine all metrics for all financing scenarios

tab_memo <- tibble::tibble(
item   = c(
"Acquisition price (DI)",
"Initial LTV",
"Unlevered project IRR",
"Unlevered project NPV",
"Leveraged equity IRR (30% LTV, bullet)",
"Leveraged equity NPV (30% LTV, bullet)",
"Leveraged equity IRR (70% LTV, bullet)",
"Leveraged equity NPV (70% LTV, bullet)",
"Leveraged equity IRR (70% LTV, amortising)",
"Leveraged equity NPV (70% LTV, amortising)",
"Minimum DSCR (bullet)",
"Maximum forward LTV (bullet)",
"Minimum DSCR (amortising)",
"Maximum forward LTV (amortising)",
"Equity multiple (bullet)"
),
value  = c(
scales::comma(pricing$price_di),
scales::percent(cfg_finance$ltv_init, accuracy = 0.1),
scales::percent(irr_proj_ae, accuracy = 0.01),
scales::comma(npv_proj_ae, accuracy = 1),
scales::percent(irr_eq_lev_bullet, accuracy = 0.01),
scales::comma(npv_eq_lev_bullet, accuracy = 1),
scales::percent(irr_eq_lev_bullet_70, accuracy = 0.01),
scales::comma(npv_eq_lev_bullet_70, accuracy = 1),
scales::percent(irr_eq_lev_amort_70, accuracy = 0.01),
scales::comma(npv_eq_lev_amort_70, accuracy = 1),
round(min_dscr_bullet, 3),
scales::percent(max_ltv_bullet, accuracy = 0.1),
round(min_dscr_amort, 3),
scales::percent(max_ltv_amort, accuracy = 0.1),
round(em, 2)  # This still refers to the initial equity multiple (bullet scenario)
)
)

tab_memo

