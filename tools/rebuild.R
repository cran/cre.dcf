rm(list = ls(all.names = TRUE))
if ("cre.dcf" %in% loadedNamespaces()) pkgload::unload("cre.dcf")

#source("inst/extdata/cre_glossary.R") # glossary update

renv::status()
devtools::document()
devtools::load_all(quiet = TRUE)
#devtools::test()
#devtools::build_vignettes()
rcmdcheck::rcmdcheck(
  args = c("--as-cran","--run-donttest","--no-manual"),
  error_on = "error"
)

message("\n✓ Package reconstruit, tests et vignettes OK.\n")
