## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE)
library(cre.dcf)
library(dplyr)

## -----------------------------------------------------------------------------
cre_glossary |>
arrange(term_en) |>
select(term_en, term_fr, definition_en, definition_fr, category, subcategory) |>
knitr::kable()


## -----------------------------------------------------------------------------
cre_glossary |>
filter(category == "debt_metrics") |>
select(term_en, term_fr, definition_en, definition_fr) |>
knitr::kable()


## -----------------------------------------------------------------------------
head(cre_glossary)


## -----------------------------------------------------------------------------
cre_glossary |> filter(term_id == "irr")

