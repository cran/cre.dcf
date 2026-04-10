## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(cre.dcf)
library(dplyr)


## -----------------------------------------------------------------------------
cfg_path <- system.file("extdata", "preset_core.yml", package = "cre.dcf")
stopifnot(nzchar(cfg_path))

cfg  <- yaml::read_yaml(cfg_path)
case <- run_case(cfg)

cmp <- case$comparison
stopifnot(is.list(cmp), is.data.frame(cmp$summary))

# Ensure expected fields are present

required_fields <- c("scenario","irr_equity","npv_equity","min_dscr","max_ltv_forward")
stopifnot(all(required_fields %in% names(cmp$summary)))

knitr::kable(cmp$summary, caption = "Summary comparison of bullet vs amortizing structures")


## -----------------------------------------------------------------------------
# Extract scenario rows --------------------------------------------------

rows <- split(cmp$summary, cmp$summary$scenario)
stopifnot(all(c("debt_bullet", "debt_amort") %in% names(rows)))

bullet <- rows$debt_bullet
amort  <- rows$debt_amort

# readable summary --------------------------------------------

cat("\nQualitative comparison of debt structures:\n")
cat(sprintf(
  "• IRR equity : bullet = %.4f%% | amort. = %.4f%%\n",
  100 * bullet$irr_equity,
  100 * amort$irr_equity
))
cat(sprintf(
  "• Min DSCR   : bullet = %.3f  | amort. = %.3f\n",
  bullet$min_dscr,
  amort$min_dscr
))
cat(sprintf(
  "• Max LTV f. : bullet = %.3f  | amort. = %.3f\n",
  bullet$max_ltv_forward,
  amort$max_ltv_forward
))

# Expected financial ordering (sanity checks) ----------------------------

## (a) Leverage effect on IRR - bullet should give a higher equity IRR
stopifnot(bullet$irr_equity > amort$irr_equity)

## (b) DSCR - the ordering is not universal and depends on where the NOI trough
##     occurs relative to the amortization profile. Here we only check that the
##     summary exposes interpretable finite values.
stopifnot(is.finite(bullet$min_dscr))
stopifnot(is.finite(amort$min_dscr))

## (c) Forward LTV - amortizing structure should deleverage over time
stopifnot(bullet$max_ltv_forward > amort$max_ltv_forward)


## -----------------------------------------------------------------------------
# Extract interest-cover paths ------------------------------------------

rat_bul <- case$comparison$details$debt_bullet$ratios
rat_amo <- case$comparison$details$debt_amort$ratios

required_ratio_fields <- c("year", "interest_cover_ratio", "interest")
stopifnot(all(required_ratio_fields %in% names(rat_bul)))
stopifnot(all(required_ratio_fields %in% names(rat_amo)))

# Restrict to operating years (exclude t = 0)

icr_bul <- rat_bul$interest_cover_ratio[rat_bul$year >= 1]
icr_amo <- rat_amo$interest_cover_ratio[rat_amo$year >= 1]

icr_min_bul  <- min(icr_bul, na.rm = TRUE)
icr_min_amo  <- min(icr_amo, na.rm = TRUE)
icr_mean_bul <- mean(icr_bul, na.rm = TRUE)
icr_mean_amo <- mean(icr_amo, na.rm = TRUE)

last_year_bul <- max(rat_bul$year[rat_bul$year >= 1])
last_year_amo <- max(rat_amo$year[rat_amo$year >= 1])

# Last-year ICR among operating years

icr_last_bul <- tail(icr_bul, 1L)
icr_last_amo <- tail(icr_amo, 1L)

cat(
"\nInterest cover summary:\n",
sprintf("• Min ICR    : bullet = %.3f | amort. = %.3f\n", icr_min_bul, icr_min_amo),
sprintf("• Mean ICR   : bullet = %.3f | amort. = %.3f\n", icr_mean_bul, icr_mean_amo),
sprintf(
"• Last-year ICR (t = %d / %d) : bullet = %.3f | amort. = %.3f\n",
last_year_bul, last_year_amo, icr_last_bul, icr_last_amo
),
"  • Read together with DSCR, debt yield and forward LTV.\n"
)

# Internal sanity check: ICR must be finite whenever interest > 0 and NOI > 0 --

stopifnot(all(is.finite(rat_bul$interest_cover_ratio[rat_bul$interest > 0 & rat_bul$noi > 0])))
stopifnot(all(is.finite(rat_amo$interest_cover_ratio[rat_amo$interest > 0 & rat_amo$noi > 0])))



## -----------------------------------------------------------------------------
# DSCR availability when debt service is positive and NOI is positive -----

stopifnot("dscr" %in% names(rat_bul))
stopifnot("dscr" %in% names(rat_amo))

bul_idx <- rat_bul$payment > 0 & rat_bul$noi > 0
amo_idx <- rat_amo$payment > 0 & rat_amo$noi > 0

stopifnot(all(is.finite(rat_bul$dscr[bul_idx])))
stopifnot(all(is.finite(rat_amo$dscr[amo_idx])))

# Read the sign of DSCR --------------------------------------------------

neg_share_bul <- mean(rat_bul$dscr[bul_idx] < 0, na.rm = TRUE)
neg_share_amo <- mean(rat_amo$dscr[amo_idx] < 0, na.rm = TRUE)

cat(
"\nDSCR sign summary:\n",
sprintf(
"• Bullet   – min DSCR = %.3f, share of negative DSCR (interest > 0): %.1f%%\n",
min(rat_bul$dscr[bul_idx], na.rm = TRUE),
100 * neg_share_bul
),
sprintf(
"• Amort.   – min DSCR = %.3f, share of negative DSCR (interest > 0): %.1f%%\n",
min(rat_amo$dscr[amo_idx], na.rm = TRUE),
100 * neg_share_amo
),
"  • Negative values can appear in transitional years.\n"
)


## -----------------------------------------------------------------------------
# Global sum of discounted equity flows in the consolidated table --------

cf_all <- case$cashflows
stopifnot("equity_disc" %in% names(cf_all))

npv_equity_sum <- sum(cf_all$equity_disc, na.rm = TRUE)
stopifnot(is.finite(npv_equity_sum))

# 5.2 Scenario-level equity NPVs from the comparison summary -----------------

npv_equity_bullet <- cmp$summary$npv_equity[cmp$summary$scenario == "debt_bullet"]
npv_equity_amort  <- cmp$summary$npv_equity[cmp$summary$scenario == "debt_amort"]

stopifnot(
length(npv_equity_bullet) == 1L,
length(npv_equity_amort)  == 1L
)

# Leveraged NPV reported in the main case object -------------------------

npv_equity_lev <- case$leveraged$npv_equity
stopifnot(is.finite(npv_equity_lev))

# Read the relationship between these quantities -------------------------

gap_bullet_global <- npv_equity_sum - npv_equity_bullet
gap_amort_global  <- npv_equity_sum - npv_equity_amort

cat(
"\nEquity NPV comparison:\n",
sprintf(
"• Global sum of discounted equity flows (cf_all$equity_disc): %s\n",
formatC(npv_equity_sum, format = 'f', big.mark = " ")
),
sprintf(
"• Bullet scenario equity NPV (comparison summary)        : %s\n",
formatC(npv_equity_bullet, format = 'f', big.mark = " ")
),
sprintf(
"• Amort. scenario equity NPV (comparison summary)        : %s\n",
formatC(npv_equity_amort, format = 'f', big.mark = " ")
),
sprintf(
"• Leveraged equity NPV reported in case$leveraged        : %s\n",
formatC(npv_equity_lev, format = 'f', big.mark = " ")
),
sprintf(
"• Global – bullet NPV gap                               : %s\n",
formatC(gap_bullet_global, format = 'f', big.mark = " ")
),
sprintf(
"• Global – amort. NPV gap                              : %s\n",
formatC(gap_amort_global,  format = 'f', big.mark = " ")
),
"\n",
"The consolidated column `equity_disc` comes from the main merged table.\n",
"Scenario NPVs in `comparison$summary` and `case$leveraged` come from their own\n",
"scenario-specific equity cash-flow streams, so they should be compared, not\n",
"forced into a single algebraic identity.\n"
)


