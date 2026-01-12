## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(cre.dcf)
library(yaml)


## -----------------------------------------------------------------------------
# 1) Load a canonical configuration

cfg_path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
stopifnot(nzchar(cfg_path))

cfg <- yaml::read_yaml(cfg_path)
stopifnot(is.list(cfg), length(cfg) > 0)

# Optional but recommended: validate before use

cfg <- cre.dcf::cfg_validate(cfg)

cat("✓ Configuration loaded and validated.\n")


## -----------------------------------------------------------------------------
# 2) Run twice under identical conditions

case1 <- run_case(cfg)
case2 <- run_case(cfg)

# 3) Build compact metric vectors (named for clarity)

m1 <- c(
irr_project = case1$all_equity$irr_project,
npv_project = case1$all_equity$npv_project,
irr_equity  = case1$leveraged$irr_equity,
npv_equity  = case1$leveraged$npv_equity
)

m2 <- c(
irr_project = case2$all_equity$irr_project,
npv_project = case2$all_equity$npv_project,
irr_equity  = case2$leveraged$irr_equity,
npv_equity  = case2$leveraged$npv_equity
)

# 4) Byte-for-byte identity on metrics (primary assertion)

bytes_equal_metrics <- identical(serialize(m1, NULL), serialize(m2, NULL))
stopifnot(bytes_equal_metrics)

# 5) Byte-for-byte identity on key tables (secondary assertions)

# (a) all-equity cashflows

ae1 <- case1$all_equity$cashflows
ae2 <- case2$all_equity$cashflows

# (b) leveraged cashflows

lv1 <- case1$leveraged$cashflows
lv2 <- case2$leveraged$cashflows

# (c) comparison summary (if present)

sm1 <- case1$comparison$summary
sm2 <- case2$comparison$summary

stopifnot(
identical(serialize(ae1, NULL), serialize(ae2, NULL)),
identical(serialize(lv1, NULL), serialize(lv2, NULL)),
identical(serialize(sm1, NULL), serialize(sm2, NULL))
)

cat(
"\nReproducibility diagnostics (byte-level):\n",
"  • Core metrics: byte-level identity confirmed.\n",
"  • All-equity cashflows: byte-level identity confirmed.\n",
"  • Leveraged cashflows: byte-level identity confirmed.\n",
"  • Comparison summary: byte-level identity confirmed.\n"
)


## -----------------------------------------------------------------------------
max_abs_diff <- function(x, y) {

# Vector case

if (is.null(dim(x)) && is.null(dim(y))) {
if (is.numeric(x) && is.numeric(y) && length(x) == length(y)) {
return(max(abs(x - y), na.rm = TRUE))
} else {
return(NA_real_)
}
}

# Data frame case

if (is.data.frame(x) && is.data.frame(y)) {
common <- intersect(names(x), names(y))
if (length(common) == 0L) return(NA_real_)

numeric_cols <- common[
  vapply(x[common], is.numeric, TRUE) &
  vapply(y[common], is.numeric, TRUE)
]

if (length(numeric_cols) == 0L) return(NA_real_)

mx <- 0
for (nm in numeric_cols) {
  d <- max(abs(x[[nm]] - y[[nm]]), na.rm = TRUE)
  if (is.finite(d) && d > mx) mx <- d
}
return(mx)


}

# Fallback

NA_real_
}

cat(sprintf("  • Max |Δ| metrics (numeric drift): %s\n",
formatC(max_abs_diff(m1, m2), format = "g")))
cat(sprintf("  • Max |Δ| AE cashflows: %s\n",
formatC(max_abs_diff(ae1, ae2), format = "g")))
cat(sprintf("  • Max |Δ| LV cashflows: %s\n",
formatC(max_abs_diff(lv1, lv2), format = "g")))
cat(sprintf("  • Max |Δ| comparison summary: %s\n",
formatC(max_abs_diff(sm1, sm2), format = "g")), "\n")


