# data-raw/cre_glossary.R

library(tibble)
library(dplyr)

# Optional helper for readability (otherwise use plain paste0())
`%+%` <- function(a, b) paste0(a, b)

cre_glossary <- tibble::tribble(
  ~term_id,       ~term_en,                            ~term_fr,
  ~definition_en,                                       ~definition_fr,
  ~category,                ~subcategory,              ~see_also,

  # ----- Discounted cash flow / valuation -----------------------------------

  "dcf",
  "Discounted cash flow (DCF)",
  "Actualisation des flux de trésorerie (DCF)",
  "Valuation method that prices a commercial real estate asset as the present "
    %+% "value of expected future cash flows, discounted at a rate reflecting time "
    %+% "value of money and project risk.",
  "Méthode de valorisation qui estime la valeur d’un actif immobilier d’entreprise "
    %+% "comme la somme actualisée des flux futurs attendus, à un taux reflétant la "
    %+% "valeur temps de l’argent et le risque du projet.",
  "discounted_cash_flow",   "valuation",
  "npv,irr,discount_rate,wacc",

  "npv",
  "Net present value (NPV)",
  "Valeur actuelle nette (VAN)",
  "Sum of all cash flows discounted at a given rate. A positive NPV indicates that "
    %+% "the project creates value above the required return, while a negative NPV "
    %+% "indicates value destruction.",
  "Somme de tous les flux de trésorerie actualisés à un taux donné. Une VAN positive "
    %+% "indique que le projet crée de la valeur au-delà du rendement exigé, une VAN "
    %+% "négative qu’il en détruit.",
  "discounted_cash_flow",   "valuation",
  "dcf,irr,discount_rate",

  "irr",
  "Internal rate of return (IRR)",
  "Taux de rendement interne (TRI)",
  "Discount rate that sets the net present value of all cash flows equal to zero. "
    %+% "Used in the package as a summary indicator of project IRR (all-equity) and "
    %+% "levered equity IRR.",
  "Taux d’actualisation qui annule la valeur actuelle nette de tous les flux (VAN = 0). "
    %+% "Utilisé dans le package comme indicateur synthétique de performance du projet "
    %+% "(TRI projet) et des fonds propres (TRI equity).",
  "discounted_cash_flow",   "return",
  "dcf,npv,discount_rate",

  "discount_rate",
  "Discount rate",
  "Taux d’actualisation",
  "Rate used to discount future cash flows to present value in a DCF model. In "
    %+% "the package it can be specified directly or derived from a WACC.",
  "Taux appliqué pour actualiser les flux de trésorerie futurs dans un modèle DCF. "
    %+% "Dans le package, il peut être fixé directement ou calculé à partir d’un "
    %+% "WACC.",
  "discounted_cash_flow",   "parameter",
  "dcf,npv,irr,wacc",

  "wacc",
  "Weighted average cost of capital (WACC)",
  "Coût moyen pondéré du capital (CMPC / WACC)",
  "Blended discount rate combining cost of equity and after-tax cost of debt, "
    %+% "weighted by their target proportions in the capital structure. When "
    %+% "disc_method = \"wacc\", project cash flows are discounted at this rate.",
  "Taux d’actualisation combinant le coût des fonds propres et le coût après impôt "
    %+% "de la dette, pondérés par leurs poids cibles dans la structure de capital. "
    %+% "Lorsque disc_method = \"wacc\", les flux du projet sont actualisés à ce taux.",
  "discounted_cash_flow",   "parameter",
  "dcf,discount_rate,irr",

  # ----- Cash-flow components -----------------------------------------------

  "noi",
  "Net operating income (NOI)",
  "Résultat net d’exploitation (NOI)",
  "Property-level income after rental revenues, vacancy and operating expenses, "
    %+% "before capital expenditure, financing costs and taxes. In the package it "
    %+% "drives both cash flows and DSCR.",
  "Revenu d’exploitation de l’actif après loyers, vacance et charges d’exploitation, "
    %+% "avant capex, frais financiers et impôts. Dans le package, il alimente les flux "
    %+% "de trésorerie et le calcul du DSCR.",
  "cash_flow",              "operating",
  "opex,capex,free_cash_flow,dscr",

  "opex",
  "Operating expenses (Opex)",
  "Charges d’exploitation (Opex)",
  "Recurring expenses required to operate the property (taxes, maintenance, "
    %+% "property management, utilities, etc.), usually modelled as a deduction "
    %+% "from gross income to arrive at NOI.",
  "Dépenses récurrentes nécessaires au fonctionnement de l’actif (taxes, entretien, "
    %+% "gestion, charges communes, énergie, etc.), généralement déduites du revenu "
    %+% "brut pour obtenir le NOI.",
  "cash_flow",              "operating",
  "noi,free_cash_flow",

  "capex",
  "Capital expenditure (Capex)",
  "Dépenses d’investissement (Capex)",
  "Non-recurring investment outlays dedicated to improving, refurbishing or "
    %+% "repositioning the property. In value-added and opportunistic scenarios, "
    %+% "capex profiles are often front-loaded.",
  "Dépenses d’investissement non récurrentes consacrées à l’amélioration, la "
    %+% "réhabilitation ou le repositionnement de l’actif. Dans les scénarios "
    %+% "value-added et opportunistes, les profils de capex sont souvent concentrés "
    %+% "en début de période.",
  "cash_flow",              "investment",
  "noi,free_cash_flow",

  "free_cash_flow",
  "Free cash flow (FCF)",
  "Flux de trésorerie libre (FCF)",
  "Cash flow available to equity or to the project after operating income, "
    %+% "operating expenses, capital expenditure and, when relevant, debt service. "
    %+% "In the package, FCF is the basis for IRR and NPV calculations.",
  "Flux de trésorerie disponible pour les fonds propres ou pour le projet après "
    %+% "revenu d’exploitation, charges, capex et, le cas échéant, service de la dette. "
    %+% "Dans le package, le FCF sert de base aux calculs de VAN et TRI.",
  "cash_flow",              "summary",
  "noi,opex,capex,dcf,npv,irr",

  "sale_proceeds",
  "Sale proceeds",
  "Produit de cession",
  "Net cash inflow received when the property is sold at the end of the horizon, "
    %+% "typically computed from an exit yield applied to stabilised NOI minus "
    %+% "transaction costs and debt repayment.",
  "Encaissement net perçu lors de la vente de l’actif en fin d’horizon, généralement "
    %+% "calculé à partir d’un rendement de sortie appliqué au NOI stabilisé, "
    %+% "diminué des frais de transaction et du remboursement de la dette.",
  "cash_flow",              "terminal",
  "exit_yield,dcf,npv,irr,free_cash_flow",

  # ----- Yields and pricing -------------------------------------------------

  "cap_rate",
  "Capitalisation rate (cap rate)",
  "Taux de capitalisation (cap rate)",
  "Yield used to convert a single-year NOI into a capital value by division "
    %+% "(Value = NOI / cap rate). In practice, entry and exit yields in the package "
    %+% "play a similar role.",
  "Rendement utilisé pour convertir un NOI annuel en valeur de capital par "
    %+% "division (Valeur = NOI / cap rate). En pratique, les taux d’entrée et de "
    %+% "sortie du package remplissent un rôle similaire.",
  "yield_pricing",          "valuation",
  "entry_yield,exit_yield,noi",

  "entry_yield",
  "Entry yield",
  "Rendement à l’acquisition (entry yield)",
  "Initial yield implied by the relationship between acquisition price and forward "
    %+% "or current NOI. In the package it is an input used, with costs, to "
    %+% "anchor the initial pricing of the asset.",
  "Rendement initial déduit du rapport entre le prix d’acquisition et le NOI "
    %+% "courant ou anticipé. Dans le package, c’est un paramètre d’entrée qui, avec "
    %+% "les coûts, ancre la valorisation initiale de l’actif.",
  "yield_pricing",          "entry",
  "cap_rate,noi,exit_yield",

  "exit_yield",
  "Exit yield",
  "Rendement à la sortie (exit yield)",
  "Yield applied to stabilised NOI in the terminal year to determine the exit "
    %+% "value of the property (sale price before transaction costs). Exit yield "
    %+% "levels and spreads are central in the package scenarios.",
  "Taux appliqué au NOI stabilisé de la dernière année pour déterminer la valeur "
    %+% "de sortie de l’actif (prix de cession avant frais). Le niveau et l’écart de "
    %+% "rendement de sortie sont centraux dans les scénarios du package.",
  "yield_pricing",          "exit",
  "cap_rate,noi,sale_proceeds",

  # ----- Debt structure and metrics -----------------------------------------

  "ltv",
  "Loan-to-value ratio (LTV)",
  "Ratio prêt-valeur (LTV)",
  "Outstanding loan balance divided by the market value or acquisition price of the "
    %+% "property. Higher LTVs mean higher leverage and thinner equity buffers.",
  "Encours de dette rapporté à la valeur de marché ou au prix d’acquisition de "
    %+% "l’actif. Un LTV élevé traduit un effet de levier plus important et un "
    %+% "coussin de fonds propres plus réduit.",
  "debt_metrics",           "leverage",
  "dscr,debt_yield",

  "ltv_forward",
  "Forward LTV",
  "LTV prospectif (forward LTV)",
  "Loan-to-value ratio projected over time in the model, for example after amortisation "
    %+% "and changes in property value. The package reports a maximum forward LTV over "
    %+% "the horizon.",
  "Ratio prêt-valeur projeté dans le temps, par exemple après amortissement et "
    %+% "évolution de la valeur de l’actif. Le package rapporte notamment un LTV "
    %+% "prospectif maximal sur l’horizon.",
  "debt_metrics",           "leverage",
  "ltv,dscr",

  "dscr",
  "Debt service coverage ratio (DSCR)",
  "Taux de couverture du service de la dette (DSCR)",
  "Ratio of net operating income to debt service (interest plus amortisation). Values "
    %+% "significantly above 1.0 indicate that property cash flows cover contractual "
    %+% "debt payments with margin. The package tracks DSCR paths and minima.",
  "Rapport entre le résultat net d’exploitation (NOI) et le service de la dette "
    %+% "(intérêts + amortissement). Des valeurs nettement supérieures à 1,0 indiquent "
    %+% "que les flux de trésorerie de l’actif couvrent les échéances de dette avec "
    %+% "marge. Le package suit les trajectoires de DSCR et leurs minima.",
  "debt_metrics",           "covenant",
  "ltv,debt_yield,noi",

  "debt_yield",
  "Debt yield",
  "Rendement de la dette (debt yield)",
  "Ratio of NOI to outstanding loan balance, independent of interest rate and "
    %+% "amortisation profile. Used as a more conservative leverage metric in some "
    %+% "lending practices.",
  "Rapport entre le NOI et l’encours de dette, indépendant du taux d’intérêt et "
    %+% "du profil d’amortissement. Utilisé comme métrique de levier plus prudente "
    %+% "dans certaines pratiques de financement.",
  "debt_metrics",           "leverage",
  "ltv,dscr,noi",

  "bullet_loan",
  "Bullet loan",
  "Prêt in fine (bullet)",
  "Debt structure where only interest is paid during the life of the loan and the "
    %+% "principal is repaid in full at maturity. In the package, this structure "
    %+% "maximises leverage and refinancing risk.",
  "Structure de dette dans laquelle seuls les intérêts sont payés pendant la durée "
    %+% "du prêt et le principal est remboursé en une fois à l’échéance. Dans le "
    %+% "package, cette structure maximise le levier et le risque de refinancement.",
  "debt_structure",         "amortisation_profile",
  "amortizing_loan,ltv,dscr",

  "amortizing_loan",
  "Amortizing loan",
  "Prêt amortissable",
  "Debt structure where each period includes both interest and principal repayment, "
    %+% "reducing the outstanding balance over time. In the package it typically "
    %+% "lowers forward LTV and improves DSCR compared to a bullet.",
  "Structure de dette dans laquelle chaque échéance comprend intérêts et remboursement "
    %+% "de principal, ce qui réduit l’encours au fil du temps. Dans le package, elle "
    %+% "tend à diminuer le LTV prospectif et à améliorer le DSCR par rapport "
    %+% "à un prêt in fine.",
  "debt_structure",         "amortisation_profile",
  "bullet_loan,ltv,dscr",

  "maturity",
  "Loan maturity",
  "Maturité du prêt",
  "Time between loan drawdown and final contractual repayment. In the package it is "
    %+% "expressed in years and interacts with the investment horizon and amortisation "
    %+% "profile.",
  "Durée entre le déblocage du prêt et son remboursement contractuel final. Dans le "
    %+% "package, elle est exprimée en années et interagit avec l’horizon d’investissement "
    %+% "et le profil d’amortissement.",
  "debt_structure",         "tenor",
  "bullet_loan,amortizing_loan,ltv,dscr",

  "covenant",
  "Financial covenant",
  "Covenant financier",
  "Contractual test attached to a loan, typically based on leverage (e.g. LTV) or "
    %+% "coverage (e.g. DSCR), that must remain within predefined thresholds. The "
    %+% "package tracks covenant breaches along the debt path.",
  "Test contractuel associé à un prêt, généralement fondé sur le levier (par exemple "
    %+% "LTV) ou la couverture (par exemple DSCR), qui doit rester dans des "
    %+% "bornes prédéfinies. Le package comptabilise les franchissements de seuils le "
    %+% "long de la trajectoire de dette.",
  "debt_metrics",           "constraint",
  "ltv,dscr"

) |>
  dplyr::arrange(term_id)

usethis::use_data(cre_glossary, overwrite = TRUE)
