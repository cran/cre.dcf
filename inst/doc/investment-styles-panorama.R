## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(cre.dcf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(scales)


## -----------------------------------------------------------------------------
# Retrieve manifest
tbl_print <- styles_manifest()

# Ensure expected ordering
tbl_print <- tbl_print |>
  dplyr::filter(style %in% c("core", "core_plus", "value_added", "opportunistic")) |>
  dplyr::mutate(
    style = factor(
      style,
      levels = c("core", "core_plus", "value_added", "opportunistic")
    )
  ) |>
  dplyr::arrange(style) |>
  dplyr::select(
    style,
    irr_project,
    irr_equity,
    dscr_min_bul,
    ltv_max_fwd,
    ops_share,
    tv_share,
    npv_equity
  )

# Defensive: stop if table empty (should never happen if helpers/tests are correct)
if (nrow(tbl_print) == 0L) {
  stop("No style presets were found. Check inst/extdata and helper logic.")
}

# Render table
knitr::kable(
  tbl_print,
  digits = c(0, 0, 4, 4, 3, 3, 3, 3, 0),
  caption = "Style presets: returns, credit profile, and value composition"
)

## -----------------------------------------------------------------------------
tbl_rr <- styles_manifest() |>
  dplyr::filter(style %in% c("core", "core_plus", "value_added", "opportunistic")) |>
  dplyr::mutate(
    style = factor(
      style,
      levels = c("core", "core_plus", "value_added", "opportunistic")
    ),
    irr_uplift = irr_equity - irr_project
  ) |>
  dplyr::arrange(style)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(
    tbl_rr,
    ggplot2::aes(x = irr_project, y = irr_equity, label = style, colour = style)
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 3) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(nudge_y = 0.002, size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title  = "Risk–return cloud (project vs equity IRR)",
      x      = "IRR project (unlevered)",
      y      = "IRR equity (levered)"
    )
}

## -----------------------------------------------------------------------------
tbl_cov <- styles_manifest() |>
  dplyr::filter(style %in% c("core", "core_plus", "value_added", "opportunistic")) |>
  dplyr::mutate(
    style = factor(
      style,
      levels = c("core", "core_plus", "value_added", "opportunistic")
    )
  ) |>
  dplyr::arrange(style) |>
  dplyr::select(
    style,
    irr_project,
    irr_equity,
    dscr_min_bul,
    ltv_init,      # structural leverage at origination
    ltv_max_fwd,   # worst forward LTV under the business plan
    npv_equity
  )

if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggplot(
    tbl_cov,
    ggplot2::aes(
      x     = ltv_init,
      y     = dscr_min_bul,
      label = style,
      colour = style
    )
  ) +
    ggplot2::geom_hline(yintercept = 1.2, linetype = 3) +  # illustrative DSCR guardrail
    ggplot2::geom_vline(xintercept = 0.65, linetype = 3) + # illustrative initial-LTV guardrail
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(nudge_y = 0.05, size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title  = "Leverage–coverage map",
      x      = "Initial LTV (bullet)",
      y      = "Min DSCR (bullet)"
    )
}

## -----------------------------------------------------------------------------
guard <- list(min_dscr = 1.50, max_ltv = 0.60)

breach_tbl <- styles_breach_counts(
  styles         = c("core", "core_plus", "value_added", "opportunistic"),
  min_dscr_guard = guard$min_dscr,
  max_ltv_guard  = guard$max_ltv
)

knitr::kable(
  breach_tbl,
  caption = "Covenant-breach counts by style (bullet)"
)

## -----------------------------------------------------------------------------
styles_vec <- c("core", "core_plus", "value_added", "opportunistic")

# Baseline (WACC) equity metrics from the manifest
base_tbl <- styles_manifest(styles_vec) |>
  dplyr::select(style, irr_equity, npv_equity)

# Re-evaluation under the yield+growth rule
yg_tbl <- styles_revalue_yield_plus_growth(styles_vec)

rob_tbl <- dplyr::left_join(base_tbl, yg_tbl, by = "style") |>
  dplyr::mutate(
    delta_npv = npv_equity_y - npv_equity
  )

knitr::kable(
  rob_tbl,
  digits  = 4,
  caption = "Robustness: equity IRR (invariant) and NPV under WACC vs yield+growth"
)

## -----------------------------------------------------------------------------
styles_vec <- c("core", "core_plus", "value_added", "opportunistic")

# 1) Equity cash flows and horizons ----------------------------------------

eq_tbl <- styles_equity_cashflows(styles_vec) |>
  dplyr::group_by(style) |>
  dplyr::arrange(style, year)

horizon_tbl <- eq_tbl |>
  dplyr::group_by(style) |>
  dplyr::summarise(
    horizon_years = max(year),
    .groups       = "drop"
  )

eq_with_h <- dplyr::left_join(eq_tbl, horizon_tbl, by = "style")

# 2) Share of total positive equity CF received before the final year ------

timing_tbl <- eq_with_h |>
  dplyr::group_by(style) |>
  dplyr::summarise(
    total_pos_equity  = sum(pmax(equity_cf, 0), na.rm = TRUE),
    early_pos_equity  = sum(
      pmax(equity_cf, 0) * (year < horizon_years),
      na.rm = TRUE
    ),
    share_early_equity = dplyr::if_else(
      total_pos_equity > 0,
      early_pos_equity / total_pos_equity,
      NA_real_
    ),
    .groups = "drop"
  )

knitr::kable(
  timing_tbl |>
    dplyr::select(style, share_early_equity),
  digits  = 3,
  caption = "Share of total positive equity distributions received before the final year"
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  eq_cum_tbl <- eq_with_h |>
    dplyr::group_by(style) |>
    dplyr::mutate(cum_equity = cumsum(equity_cf))

  ggplot2::ggplot(
    eq_cum_tbl,
    ggplot2::aes(x = year, y = cum_equity, colour = style)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::geom_line() +
    ggplot2::labs(
      title = "Cumulative leveraged equity cash flows by style",
      x     = "Year",
      y     = "Cumulative equity CF"
    )
}

## -----------------------------------------------------------------------------
styles_vec <- c("core", "core_plus", "value_added", "opportunistic")

pv_tbl <- styles_manifest(styles_vec) |>
  dplyr::mutate(style = factor(style, levels = styles_vec))

knitr::kable(
  pv_tbl |>
    dplyr::select(style, ops_share, tv_share),
  digits = 3,
  caption = "Present-value split between operations and terminal value by style"
)

## -----------------------------------------------------------------------------
## Sensitivity to +/- 50 bps on exit yield ----------------------------------

styles_vec <- c("core", "core_plus", "value_added", "opportunistic")

exit_sens <- styles_exit_sensitivity(
  styles    = styles_vec,
  delta_bps = c(-50, 0, 50)
)

knitr::kable(
  exit_sens |>
    tidyr::pivot_wider(
      names_from  = shock_bps,
      values_from = irr_equity
    ),
  digits  = 4,
  caption = "Equity IRR sensitivity to +/- 50 bps exit-yield shock by style"
)

## -----------------------------------------------------------------------------
## Sensitivity to rental-growth shocks --------------------------------------

growth_sens <- styles_growth_sensitivity(
  styles = styles_vec,
  delta  = c(-0.01, 0, 0.01)
)

knitr::kable(
  growth_sens |>
    tidyr::pivot_wider(
      names_from  = shock_growth,
      values_from = irr_equity
    ),
  digits  = 4,
  caption = "Equity IRR sensitivity to rental-growth shocks by style"
)

## -----------------------------------------------------------------------------
target_irr <- 0.10  # 10% equity IRR as illustrative hurdle

be_tbl <- styles_break_even_exit_yield(
  styles     = c("core", "core_plus", "value_added", "opportunistic"),
  target_irr = target_irr
)

baseline_irr_tbl <- styles_manifest(
  c("core", "core_plus", "value_added", "opportunistic")
) |>
  dplyr::select(style, irr_equity)

knitr::kable(
  be_tbl,
  digits  = 4,
  caption = sprintf("Break-even exit yield to hit %.1f%% equity IRR by style", 100 * target_irr)
)

## -----------------------------------------------------------------------------
## Distressed exit across regimes --------------------------------

# Covenant regimes: strict / baseline / flexible
regimes <- tibble::tibble(
  regime   = c("strict", "baseline", "flexible"),
  min_dscr = c(1.20,      1.15,       1.10),
  max_ltv  = c(0.65,      0.70,       0.75)
)

distress_tbl <- styles_distressed_exit(
  styles               = c("core", "core_plus", "value_added", "opportunistic"),
  regimes              = regimes,
  fire_sale_bps        = 100,   # +100 bps exit-yield penalty
  refi_min_year        = 3L,    # refinancing window opens in year 3
  allow_year1_distress = FALSE, # breaches before year 3 --> exit at year 3
  underwriting_mode    = "transition"
)

# For compact display in the vignette, focus on the baseline regime
distress_baseline <- distress_tbl |>
  dplyr::filter(regime == "baseline") |>
  dplyr::select(
    style,
    underwriting_mode,
    covenant_start_year,
    breach_year,
    breach_type,
    irr_equity_base,
    irr_equity_distress,
    distress_undefined,
    equity_multiple_base,
    equity_multiple_distress,
    equity_loss_pct_distress
  ) |>
  dplyr::arrange(style)

knitr::kable(
  distress_baseline,
  digits  = c(0, 0, 0, 0, 0, 4, 4, 0, 2, 2, 2),
  caption = paste(
    "Baseline distressed-exit summary by style (bullet debt scenario,",
    "+100 bps fire-sale penalty; breaches before year 3 shifted to year 3)."
  )
)

## -----------------------------------------------------------------------------
# Export results and breaches (CSV) to facilitate off-notebook auditing
out_dir <- tempfile("cre_dcf_styles_")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(tbl_print,  file.path(out_dir, "styles_summary.csv"))
readr::write_csv(breach_tbl, file.path(out_dir, "covenant_breaches.csv"))

cat(sprintf("\nArtifacts written to: %s\n", out_dir))

