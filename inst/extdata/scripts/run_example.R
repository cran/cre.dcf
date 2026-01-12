devtools::load_all(".")
library(cre.dcf)

out <- run_case()
print(out$comparison)

# ── Save result as JSON ─────────────────────────────────────────────────────
library(jsonlite)

# Save full result (if desired)
write_json(out, "output/output_full.json", pretty = TRUE, auto_unbox = TRUE)

# Save only the comparison table
write_json(out$comparison, "output/output_comparison.json", pretty = TRUE, auto_unbox = TRUE)

