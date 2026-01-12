## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)

library(cre.dcf)
library(yaml)


## -----------------------------------------------------------------------------
## 1. Load and validate a correct YAML configuration

# 1.1 Locate and parse a reference configuration file

path <- system.file("extdata", "preset_core.yml", package = "cre.dcf")
stopifnot(nzchar(path))

cfg <- yaml::read_yaml(path)
stopifnot(is.list(cfg), length(cfg) > 0)

# 1.2 Validate structure and types

# cfg_validate() throws an error if invalid; otherwise it returns (optionally invisibly)

# a configuration list that is deemed structurally consistent.

validated_cfg <- cre.dcf::cfg_validate(cfg)

cat("✓ Validation successful: configuration passed all structural and type checks.\n")

# 1.3 For illustration, display a compact excerpt of the validated configuration.

# Some implementations of cfg_validate() return cfg invisibly; others may return NULL.

# We therefore fall back to the original cfg if needed.

cfg_to_show <- if (is.list(validated_cfg) && length(validated_cfg) > 0L) {
validated_cfg
} else {
cfg
}

cat("\nExcerpt of (validated) configuration structure:\n")
utils::str(cfg_to_show[1:10], max.level = 1)


## -----------------------------------------------------------------------------
## 2. Deliberate type violation and controlled failure

# 2.1 Clone configuration and introduce a deliberate type error:

# purchase_year must be integer-like; we turn it into a character string.

bad_cfg <- cfg
bad_cfg$purchase_year <- "2020"  # invalid type: character instead of integer/numeric

# 2.2 Attempt validation and capture the expected error

caught <- FALSE
err_msg <- NULL

tryCatch(
{
cre.dcf::cfg_validate(bad_cfg)
},
error = function(e) {
caught  <<- TRUE
err_msg <<- e$message
}
)

# 2.3 Assert that the failure mechanism was triggered

stopifnot(caught)

cat("\nExpected validation failure caught:\n")
cat(err_msg, "\n")

cat("\n✓ Error successfully detected: invalid type for 'purchase_year' was blocked at validation stage.\n")


