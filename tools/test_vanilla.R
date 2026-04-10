#!/usr/bin/env Rscript

Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "FALSE")

args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grepl(file_arg, args)][1])
project <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

suppressPackageStartupMessages({
  library(pkgload)
  library(testthat)
})

pkgload::load_all(project, quiet = TRUE)
testthat::test_dir(file.path(project, "tests", "testthat"), reporter = "summary")
