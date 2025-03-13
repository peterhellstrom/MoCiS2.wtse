# https://r-pkgs.org
# devtools::install_github("r-lib/devtools")
# devtools::install_github("r-lib/usethis")

library(devtools)

# Create project ----
# p <- "W:/projects/data/contaminants/MoCiS2.wtse"
# usethis::create_package(p, check_name = FALSE)

# License ----
# usethis::use_mit_license()

# Creat GitHub repository ----
# use_git_config(
#   user.name = "peterhellstrom",
#   user.email = "peter.hellstrom@nrm.se"
# )

# usethis::use_git()
# usethis::use_github()
# usethis::create_github_token()

# Load all ----
load_all()

# Documentation / NAMESPACE ----
document()

# Check ----
chk_pkg <- check()
dplyr::glimpse(chk_pkg)
names(chk_pkg)

chk_pkg$checkdir
utils::browseURL(chk_pkg$checkdir)

# Test ----
test()

# ReadMe ----
use_readme_rmd()
build_readme()

# Imports ----
# How to deal with this?
# Imports includes {n} non-default packages.
# Importing from so many packages makes the package vulnerable to any of
# them becoming unavailable.  Move as many as possible to Suggests and
# use conditionally.

# https://stackoverflow.com/questions/63345284/r-package-cran-note-for-package-dependencies-and-warnings-in-tests
# Bad practice to import entire package?
# Use some functions "conditionally"?

usethis::use_package("DBI", min_version = TRUE)
usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("fuzzyjoin", min_version = TRUE)
usethis::use_package("glue", min_version = TRUE)
usethis::use_package("lubridate", min_version = TRUE)
usethis::use_package("openxlsx2", min_version = TRUE)
usethis::use_package("purrr", min_version = TRUE)
usethis::use_package("readr", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("rlang", min_version = TRUE)
usethis::use_package("RSQLite", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("tibble", min_version = TRUE)
usethis::use_package("tidyr", min_version = TRUE)
usethis::use_package("tidyselect", min_version = TRUE)

# Important: Must use development version of openxlsx
# usethis::use_dev_package("openxlsx", remote = "github::ycphs/openxlsx")

# Suggests ----
usethis::use_package("ggplot2", "Suggests")
usethis::use_package("knitr", "Suggests")
usethis::use_package("rmarkdown", "Suggests")
usethis::use_package("sf", "Suggests")
usethis::use_package("units", "Suggests")
usethis::use_package("writexl", "Suggests")

usethis::use_dev_package("eagles", "Suggests", remote = "github::peterhellstrom/eagles")
usethis::use_dev_package("eaglesEggs", "Suggests", remote = "github::peterhellstrom/eaglesEggs")
usethis::use_dev_package("swecoords", "Suggests", remote = "github::peterhellstrom/swecoords")

requireNamespace("ggplot2", quietly = TRUE)

usethis::use_tidy_description()

# Install ----
install()

## Install from GitHub ----
# install_github("peterhellstrom/MoCiS2.wtse")

# Ignore ----
usethis::use_build_ignore(
  c("backup", "data-raw", "development", "examples")
)

# Document data ----
# https://r-pkgs.org/data.html

## Load package ----
library(MoCiS2.wtse)
utils::sessionInfo()
sessioninfo::session_info()

## Data sets ----
usethis::use_data_raw()
