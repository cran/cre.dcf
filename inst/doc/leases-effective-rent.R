## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(cre.dcf)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
# 1.1 Load a preset configuration including explicit lease events

cfg_path <- system.file("extdata", "preset_default.yml", package = "cre.dcf")
stopifnot(nzchar(cfg_path))

cfg  <- yaml::read_yaml(cfg_path)
case <- run_case(cfg)

cf <- case$cashflows

# 1.2 Verify that all required variables are present

required_cols <- c("year", "gei", "opex", "capex", "noi", "pbtcf")
stopifnot(all(required_cols %in% names(cf)))


## -----------------------------------------------------------------------------
## 2. Analytical structure of the income chain

# 2.1 NOI as implemented in the engine: GEI - OPEX
cf <- cf |>
  mutate(
    noi_from_gei_opex   = gei - opex,
    resid_noi_core      = noi_from_gei_opex - noi,
    pbtcf_from_noi_capex = noi - capex,
    resid_pbtcf         = pbtcf_from_noi_capex - pbtcf
  )

gei_min <- min(cf$gei, na.rm = TRUE)
gei_max <- max(cf$gei, na.rm = TRUE)
noi_min <- min(cf$noi, na.rm = TRUE)
noi_max <- max(cf$noi, na.rm = TRUE)

max_abs_resid_core <- max(abs(cf$resid_noi_core), na.rm = TRUE)

cat(
  "\nIncome chain check (NOI identity):\n",
  sprintf("• Minimum GEI: %s\n", formatC(gei_min, format = 'f', big.mark = " ")),
  sprintf("• Maximum GEI: %s\n", formatC(gei_max, format = 'f', big.mark = " ")),
  sprintf("• Minimum NOI: %s\n", formatC(noi_min, format = 'f', big.mark = " ")),
  sprintf("• Maximum NOI: %s\n", formatC(noi_max, format = 'f', big.mark = " ")),
  sprintf("• Max |(GEI - OPEX) - NOI|: %s\n",
          formatC(max_abs_resid_core, format = 'f', big.mark = " ")),
  sprintf("• Max |(NOI - CAPEX) - PBTCF|: %s\n",
          formatC(max(abs(cf$resid_pbtcf), na.rm = TRUE), format = 'f', big.mark = " "))
)


## -----------------------------------------------------------------------------
## 3. Logical and accounting consistency checks

# 3.1 Finiteness
stopifnot(all(is.finite(cf$gei)))
stopifnot(all(is.finite(cf$opex)))
stopifnot(all(is.finite(cf$capex)))
stopifnot(all(is.finite(cf$noi)))

# 3.2 Non-negative OPEX / CAPEX
stopifnot(min(cf$opex,  na.rm = TRUE)  >= -1e-8)
stopifnot(min(cf$capex, na.rm = TRUE)  >= -1e-8)

# 3.3 NOI never exceeds GEI when costs are non-negative
stopifnot(all(cf$noi <= cf$gei + 1e-8))

# 3.4 NOI core identity: GEI - OPEX == NOI
stopifnot(all(abs(cf$resid_noi_core) < 1e-6))

# 3.5 PBTCF identity: NOI - CAPEX == PBTCF
stopifnot(all(abs(cf$resid_pbtcf) < 1e-6))

cat(
  "\n✓ Accounting checks passed:\n",
  "  • NOI in the engine is equal to GEI minus OPEX.\n",
  "  • PBTCF is equal to NOI minus CAPEX.\n",
  "  • OPEX and CAPEX remain non-negative, and NOI never exceeds GEI.\n"
)


## -----------------------------------------------------------------------------
## 4.1 Share of periods with negative NOI

neg_noi_share <- mean(cf$noi < 0, na.rm = TRUE)

cat(
"\nNOI sign check:\n",
sprintf("• Share of periods with NOI < 0: %.1f%%\n", 100 * neg_noi_share),
if (neg_noi_share > 0)
"  --> Indicates at least one transitional year with negative operating result (vacancy, works, etc.).\n"
else
"  --> All periods exhibit non-negative operating result in this configuration.\n"
)



## -----------------------------------------------------------------------------
cf |>
  select(year, gei, opex, capex, noi, pbtcf,
         noi_from_gei_opex, pbtcf_from_noi_capex, resid_noi_core, resid_pbtcf) |>
  head(10) |>
  knitr::kable(
    digits  = 2,
    caption = "GEI -> NOI -> PBTCF identities (first 10 years)"
  )


