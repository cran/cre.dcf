## cre.dcf 0.0.5 (2026-04-10)

- Corrected terminal-value estimation so that reversion is capitalized from a
  forwardized terminal NOI, consistent with standard CRE DCF textbook
  conventions.
- Standardized the operating cash-flow chain around `GEI -> NOI -> PBTCF` in
  the main DCF outputs while preserving legacy compatibility.
- Improved the simplified R API so that asset-level assumptions are surfaced
  more explicitly through richer printing, summaries, and `asset_snapshot()`.
- Added an analyst-friendly lease-roll API with `lease_event()`,
  `lease_unit()`, `lease_roll()`, `vacancy_event()`, `renewal_event()`, and
  `lease_roll_snapshot()` to build lease-driven cases without manual YAML
  authoring.
- Added an `operating` view to `deal_cashflows()` and a dedicated vignette on
  the lease-roll to DCF workflow.
- Added `lease_effective_rent()` and `underwrite_loan()` to cover annuitized
  lease comparison and constraint-based debt sizing.
- Added a simplified SPV tax layer with `tax_spec_spv()`, `tax_basis_spv()`,
  and `tax_run_spv()`.
- Expanded methodological documentation with dedicated vignettes on DCF
  foundations, before-tax package design, and a stylized French investment
  example.
- Recalibrated the style and credit documentation so that structural leverage
  and transition stress are interpreted separately.
- Expanded tests around DCF conventions, methodological helpers, and preset
  consistency.

## cre.dcf 0.0.4

- Fixed terminal sale-proceeds handling in leveraged cash flows and reporting
  tables.
- Aligned `run_case()`, `run_from_config()`, and financing comparisons on debt
  type resolution and debt-fee treatment.
- Added a simplified R API with `deal_spec()`, `debt_terms()`,
  `deal_to_config()`, `analyze_deal()`, and `deal_cashflows()`.
- Added user-facing print and summary methods for simplified deal objects.
- Expanded tests for cash-flow consistency, financing comparisons, and the
  simplified API.
- Simplified vignette prose and added shorter onboarding material for R users.
- Added a methodology vignette explaining the package's before-tax scope and
  the intended design of a future SPV-level tax layer.
- Added a stylized French investment vignette showing how `tax_run_spv()` can
  be used on a French-like office SPV case.
