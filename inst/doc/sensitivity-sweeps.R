## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(cre.dcf)
library(yaml)
library(dplyr)


## -----------------------------------------------------------------------------
# 2.1 Load baseline configuration (core preset)

cfg_path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")

stopifnot(nzchar(cfg_path))

cfg0 <- yaml::read_yaml(cfg_path)

# 2.2 Derive baseline exit yield from entry_yield and spread (in bps)

stopifnot(!is.null(cfg0$entry_yield))

spread_bps0 <- cfg0$exit_yield_spread_bps
if (is.null(spread_bps0)) spread_bps0 <- 0L

exit_yield_0 <- cfg0$entry_yield + as.numeric(spread_bps0) / 10000

# 2.3 Derive baseline discount rate as WACC when disc_method == "wacc"

stopifnot(!is.null(cfg0$disc_method))

if (cfg0$disc_method != "wacc") {
stop("This sensitivity skeleton assumes disc_method = 'wacc' in preset_core.yml.")
}

ltv0 <- cfg0$ltv_init
kd0  <- cfg0$rate_annual
scr0 <- if (is.null(cfg0$scr_ratio)) 0 else cfg0$scr_ratio

stopifnot(!is.null(ltv0), !is.null(kd0))

ke0 <- cfg0$disc_rate_wacc$KE
if (is.null(ke0)) {
stop("disc_rate_wacc$KE is missing in preset_core.yml; cannot compute baseline WACC.")
}

disc_rate_0 <- (1 - ltv0) * ke0 + ltv0 * kd0 * (1 - scr0)

# 2.4 Define a local grid around (exit_yield_0, disc_rate_0)

step_bps <- 50L     # 50 bps increments
span_bps <- 100L    # ±100 bps around baseline

seq_around <- function(x, span_bps = 100L, step_bps = 50L) {
x + seq(-span_bps, span_bps, by = step_bps) / 10000
}

exit_grid <- seq_around(exit_yield_0, span_bps, step_bps)
disc_grid <- seq_around(disc_rate_0,  span_bps, step_bps)

param_grid <- expand.grid(
exit_yield = exit_grid,
disc_rate  = disc_grid,
KEEP.OUT.ATTRS = FALSE,
stringsAsFactors = FALSE
)

head(param_grid)



## -----------------------------------------------------------------------------

# 3.1 Helper: invert WACC to obtain KE from target discount rate

# WACC(d) = (1 - LTV)*KE + LTV * KD * (1 - SCR)

wacc_invert_ke <- function(d, ltv, kd, scr) {
num <- d - ltv * kd * (1 - scr)
den <- 1 - ltv
ke  <- num / den

if (!is.finite(ke)) stop("Non-finite KE from WACC inversion; check inputs.")

# Soft clamp to [0, 1] with a warning in extreme cases

if (ke < 0 || ke > 1) {
warning(sprintf("Implied KE=%.4f outside [0,1]; clamped.", ke))
}
pmax(0, pmin(1, ke))
}

# 3.2 Helper: apply (exit_yield, disc_rate) to a copy of cfg0

cfg_with_params <- function(cfg_base, e, d) {
cfg_mod <- cfg_base

# 3.2.1 Adjust exit_yield via spread on entry_yield

if (is.null(cfg_mod$entry_yield)) {
stop("entry_yield missing in config; cannot derive exit_yield spread.")
}
spread_bps <- round((e - cfg_mod$entry_yield) * 10000)
cfg_mod$exit_yield_spread_bps <- as.integer(spread_bps)

# 3.2.2 Adjust cost of equity so that WACC equals target d

ltv <- cfg_mod$ltv_init
kd  <- cfg_mod$rate_annual
scr <- if (is.null(cfg_mod$scr_ratio)) 0 else cfg_mod$scr_ratio

ke_star <- wacc_invert_ke(d = d, ltv = ltv, kd = kd, scr = scr)

cfg_mod$disc_method <- "wacc"
if (is.null(cfg_mod$disc_rate_wacc) || !is.list(cfg_mod$disc_rate_wacc)) {
cfg_mod$disc_rate_wacc <- list(KE = ke_star, KD = kd, tax_rate = scr)
} else {
cfg_mod$disc_rate_wacc$KE <- ke_star
cfg_mod$disc_rate_wacc$KD <- kd
}

cfg_mod
}

# 3.3 One simulation at (exit_yield, disc_rate)

run_one <- function(e, d) {
cfg_i <- cfg_with_params(cfg0, e = e, d = d)
out   <- run_case(cfg_i)

data.frame(
exit_yield = e,
disc_rate  = d,
irr_equity = out$leveraged$irr_equity,
npv_equity = out$leveraged$npv_equity,
irr_proj   = out$all_equity$irr_project,
npv_proj   = out$all_equity$npv_project
)
}

# 3.4 Grid sweep

message("Running DCF grid sweep - number of simulations: ", nrow(param_grid))

res_list <- vector("list", nrow(param_grid))
for (i in seq_len(nrow(param_grid))) {
e <- param_grid$exit_yield[i]
d <- param_grid$disc_rate[i]
res_list[[i]] <- run_one(e, d)
}
res <- dplyr::bind_rows(res_list)

cat("\nSample of computed sensitivity grid (first 9 rows):\n")
print(dplyr::arrange(res, exit_yield, disc_rate)[1:min(9, nrow(res)), ])

cat("\nGrid coverage (raw values):\n")
cat(sprintf("• exit_yield range: [%.4f, %.4f]\n",
min(res$exit_yield), max(res$exit_yield)))
cat(sprintf("• disc_rate  range: [%.4f, %.4f]\n",
min(res$disc_rate),  max(res$disc_rate)))
cat(sprintf("• total simulations: %d\n", nrow(res)))



## -----------------------------------------------------------------------------
if (requireNamespace("ggplot2", quietly = TRUE)) {
ggplot2::ggplot(res, ggplot2::aes(x = disc_rate, y = exit_yield, fill = npv_equity)) +
ggplot2::geom_tile() +
ggplot2::geom_contour(ggplot2::aes(z = npv_equity),
bins = 10, alpha = 0.5) +
ggplot2::labs(
title = "Iso-NPV (equity) across (discount rate, exit yield)",
x = "Discount rate (target WACC, decimal)",
y = "Exit yield (decimal)",
fill = "Equity NPV"
)
}



## -----------------------------------------------------------------------------
cat("\n=== Theoretical diagnostics: invariants and monotonicities ===\n")

## 5.1 Invariance of IRR with respect to discount rate (within each exit_yield)

## ---------------------------------------------------------------------------

irr_sd_by_exit <- res |>
group_by(exit_yield) |>
summarise(
irr_sd_over_disc = sd(irr_equity, na.rm = TRUE),
.groups = "drop"
)

irr_sd_median <- median(irr_sd_by_exit$irr_sd_over_disc, na.rm = TRUE)

cat(
"\nIRR invariance diagnostics:\n",
sprintf("• Median SD of equity IRR across discount-rate variations (per exit_yield slice): %.3e\n",
irr_sd_median),
"  --> Near-zero dispersion indicates that IRR behaves as an internal rate of return,\n",
"    independent of the exogenous discount rate used for NPV computation.\n"
)

## 5.2 Monotonicity of equity NPV with respect to disc_rate

## --------------------------------------------------------

npv_monotone_disc <- res |>
group_by(exit_yield) |>
arrange(disc_rate, .by_group = TRUE) |>
summarise(
all_non_increasing = all(diff(npv_equity) <= 1e-8),
.groups = "drop"
)

share_monotone_disc <- mean(npv_monotone_disc$all_non_increasing, na.rm = TRUE)

cat(
"\nNPV monotonicity w.r.t discount rate:\n",
sprintf("• Share of exit_yield slices where equity NPV is non-increasing in disc_rate: %.1f%%\n",
100 * share_monotone_disc),
"  --> In a standard DCF, higher discount rates should decrease NPV. Deviations from\n",
"    strict monotonicity may indicate discrete changes in cash-flow structure or\n",
"    numerical tolerances.\n"
)

## 5.3 Monotonicity of equity NPV with respect to exit_yield

## ---------------------------------------------------------

npv_monotone_exit <- res |>
group_by(disc_rate) |>
arrange(exit_yield, .by_group = TRUE) |>
summarise(
all_non_increasing = all(diff(npv_equity) <= 1e-8),
.groups = "drop"
)

share_monotone_exit <- mean(npv_monotone_exit$all_non_increasing, na.rm = TRUE)

cat(
"\nNPV monotonicity w.r.t exit yield:\n",
sprintf("• Share of discount-rate slices where equity NPV is non-increasing in exit_yield: %.1f%%\n",
100 * share_monotone_exit),
"  --> Since a higher exit yield reduces terminal value, one expects lower NPV when\n",
"    exit_yield increases, all else equal. Non-monotonic patterns typically reflect\n",
"    changes in covenant status or other discrete thresholds in the model.\n"
)


