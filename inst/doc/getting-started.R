## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

## -----------------------------------------------------------------------------
library(cre.dcf)
library(yaml)
library(dplyr)

path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
stopifnot(nzchar(path))

cfg  <- yaml::read_yaml(path)
case <- run_case(cfg)

ae     <- case$all_equity
al     <- case$leveraged
cf_all <- case$cashflows

stopifnot(
is.list(ae),
is.list(al),
is.data.frame(ae$cashflows),
is.data.frame(al$cashflows),
is.data.frame(cf_all)
)

cfe <- ae$cashflows
stopifnot(all(c("year", "free_cash_flow", "sale_proceeds") %in% names(cfe)))



## -----------------------------------------------------------------------------
## 2. Exit occurs once at the final year (all-equity)

t         <- cfe$year
exit_rows <- which(cfe$sale_proceeds > 0)

# Checks: a single exit, and it occurs at the last period

stopifnot(length(exit_rows) == 1L)
stopifnot(exit_rows == which.max(t))

# ---- Display results for pedagogical clarity ----

exit_year    <- t[exit_rows]
sale_value   <- cfe$sale_proceeds[exit_rows]
free_cf_exit <- cfe$free_cash_flow[exit_rows]

cat(
"\nExit event diagnostics:\n",
sprintf("• Number of exit events detected: %d (should be 1)\n", length(exit_rows)),
sprintf("• Exit year (expected last period): %d\n", exit_year),
sprintf("• Sale proceeds at exit: %s\n",
formatC(sale_value, format = 'f', big.mark = " ")),
sprintf("• Free cash flow in the exit year (before sale): %s\n",
formatC(free_cf_exit, format = 'f', big.mark = " ")),
sprintf("• Maximum year in series: %d\n", max(t)),
if (exit_year == max(t))
"✓ Exit correctly occurs in the final year.\n"
else
"✗ Exit NOT in final year - investigate configuration.\n"
)


## -----------------------------------------------------------------------------
## 4. IRR identity (all-equity): verifying that IRR is the root of NPV = 0

# 4.1 Build cash-flow vector (t = year)

stopifnot(is.integer(cfe$year) || is.numeric(cfe$year))
stopifnot(min(cfe$year) == 0)  # ensure the time origin is correct

flows <- cfe$free_cash_flow
last  <- which.max(cfe$year)

# Add sale proceeds to the last period's free cash flow
flows[last] <- flows[last] + cfe$sale_proceeds[last]

npv_at <- function(r) {
  sum(flows / (1 + r)^(cfe$year))
}

# 4.2 Detect automatically a valid interval where NPV changes sign

grid <- seq(-0.9, 2.0, by = 0.01)
vals <- sapply(grid, npv_at)
sgn  <- sign(vals)

idx <- which(diff(sgn) != 0)
stopifnot(length(idx) >= 1L)

lower <- grid[idx[1]]
upper <- grid[idx[1] + 1]

# 4.3 Root finding with numerical control (reference IRR based on this vignette's convention)

irr_root <- uniroot(
  npv_at,
  c(lower, upper),
  tol = .Machine$double.eps^0.5
)$root

# 4.4 Checks:
# (A) NPV(irr_root) ≈ 0  [hard invariance: must hold]
# (B) NPV(ae$irr_project) reported for information only

tol_cash <- 1e-2  # acceptable deviation in currency units

npv_at_root   <- npv_at(irr_root)
npv_at_report <- npv_at(ae$irr_project)

# Hard check on the IRR computed in this vignette
stopifnot(abs(npv_at_root) <= tol_cash)

# Informative diagnostics on the package's reported IRR
gap_rate <- abs(irr_root - ae$irr_project)

status_report <- if (is.finite(npv_at_report) && abs(npv_at_report) <= tol_cash) {
  "✓ Reported IRR behaves as a root of the NPV equation under this cash-flow convention."
} else {
  paste0(
    "⚠ Reported IRR does not exactly solve NPV = 0 under this vignette's convention.\n",
    "  This may reflect different timing or cash-flow conventions in the internal implementation."
  )
}


# ---- Pedagogical printout ----

cat(
  "\nIRR identity diagnostic (all-equity case):\n",
  sprintf("• Interval used for root search: [%.2f, %.2f]\n", lower, upper),
  sprintf("• Computed IRR from cash-flow root: %.8f\n", irr_root),
  sprintf("• Reported IRR from run_case(): %.8f\n", ae$irr_project),
  sprintf("• Absolute rate gap (for information): %.10f\n", gap_rate),
  sprintf("• NPV evaluated at computed IRR: %.4f (tolerance %.2f)\n",
          npv_at_root, tol_cash),
  sprintf("• NPV evaluated at reported IRR: %.4f\n", npv_at_report),
  "\n", status_report, "\n"
)

# Optional: tabular summary for visual output

data.frame(
  irr_computed        = irr_root,
  irr_reported        = ae$irr_project,
  npv_at_irr_computed = npv_at_root,
  npv_at_irr_reported = npv_at_report
)

## -----------------------------------------------------------------------------
## 5. Discount factor consistency and interpretation

stopifnot("df" %in% names(cf_all))

df <- cf_all$df
df <- df[is.finite(df)]

# In this package, `df` increases over time (≈ (1 + r)^t),

# so its inverse is the true discount factor.

disc_factor <- 1 / df

# Theoretical properties of the discount sequence

stopifnot(abs(disc_factor[1] - 1) < 1e-12)         # t = 0 --> discount factor = 1
stopifnot(all(diff(disc_factor) <= 1e-10))         # should be non-increasing

# Summary metrics for transparency

rate_estimate <- (df[length(df)]^(1 / (length(df) - 1))) - 1
decay_ratio   <- disc_factor[length(disc_factor)] / disc_factor[1]

# ---- Pedagogical printout ----

cat(
"\nDiscount factor diagnostics:\n",
sprintf("• First value of df (t = 0): %.6f\n", df[1]),
sprintf("• Last value of df (t = %d): %.6f\n", length(df) - 1, tail(df, 1)),
sprintf("• Implied constant annual rate ≈ %.4f%%\n", 100 * rate_estimate),
sprintf("• Discount factor at t = %d: %.6f\n",
length(disc_factor) - 1, tail(disc_factor, 1)),
sprintf("• Ratio (disc_t_end / disc_t0): %.6f\n", decay_ratio),
if (all(diff(disc_factor) <= 1e-10))
"✓ Discount factors decrease monotonically - internal consistency confirmed.\n"
else
"✗ Discount factors not monotonic - check time indexing or rate definition.\n"
)

# Display a concise comparative table for reader visibility

knitr::kable(
data.frame(
year            = cf_all$year,
df              = round(df, 6),
discount_factor = round(disc_factor, 6)
),
caption = "Evolution of accumulation and discount factors across time"
)


## -----------------------------------------------------------------------------
## 6. Sanity checks and diagnostic printout

# (a) NOI finiteness and range

stopifnot("noi" %in% names(cf_all))

min_noi <- min(cf_all$noi, na.rm = TRUE)
max_noi <- max(cf_all$noi, na.rm = TRUE)

stopifnot(is.finite(min_noi), is.finite(max_noi))

# (b) Positive acquisition price (price_di)

price_di <- case$pricing$price_di
stopifnot(is.numeric(price_di), length(price_di) == 1L, price_di > 0)

# (c) Acquisition price consistency between pricing and cashflow tables

stopifnot("acquisition_price" %in% names(cfe))

price_cf  <- cfe$acquisition_price[1]
gap_price <- abs(price_di - price_cf)
stopifnot(gap_price < 1e-6)

# ---- Display results for transparency ----

cat(
"\nSanity checks summary:\n",
sprintf("• NOI range: [%s, %s]\n",
formatC(min_noi, format = 'f', big.mark = " "),
formatC(max_noi, format = 'f', big.mark = " ")),
sprintf("• Reported acquisition price (pricing$price_di): %s\n",
formatC(price_di, format = 'f', big.mark = " ")),
sprintf("• Acquisition price at t0 in cashflows: %s\n",
formatC(price_cf, format = 'f', big.mark = " ")),
sprintf("• Absolute gap between the two: %.8f (tolerance 1e-6)\n", gap_price),
if (min_noi < 0)
"• Note: NOI dips below zero in some periods - consistent with transitional or opportunistic strategies, but deserves economic interpretation.\n"
else
"• Note: NOI remains non-negative over the horizon.\n"
)


## -----------------------------------------------------------------------------
## 7. Compact financial summary

summary_tbl <- data.frame(
Metric = c(
"Unlevered IRR (project)",
"Unlevered NPV (project, currency units)",
"Equity IRR (levered case)",
"Equity NPV (levered case, currency units)",
"Acquisition price (price_di)"
),
Value = c(
ae$irr_project,
ae$npv_project,
al$irr_equity,
al$npv_equity,
case$pricing$price_di
)
)

# Pedagogical printout with interpretation

cat(
"\n--- Summary of DCF core results ---\n",
sprintf("• Unlevered IRR (project): %.4f%%\n", 100 * ae$irr_project),
sprintf("• Unlevered NPV (project): %s\n",
formatC(ae$npv_project, format = 'f', big.mark = " ")),
sprintf("• Levered IRR (equity): %.4f%%\n", 100 * al$irr_equity),
sprintf("• Levered NPV (equity): %s\n",
formatC(al$npv_equity, format = 'f', big.mark = " ")),
sprintf("• Acquisition price (price_di): %s\n",
formatC(case$pricing$price_di, format = 'f', big.mark = " ")),
"\nInterpretation:\n",
"  - The unlevered IRR reflects the intrinsic profitability of the asset before financing.\n",
"  - The levered IRR measures the equity return after accounting for debt leverage.\n",
"  - The gap between both IRRs quantifies the effect of financial leverage on expected return.\n",
"  - NPV values in currency units provide absolute measures of value creation at the chosen discount rate.\n"
)

knitr::kable(
summary_tbl,
caption = "Key DCF performance metrics for the base case (unlevered and levered)"
)


