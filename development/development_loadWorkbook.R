# remotes::install_github("ycphs/openxlsx")
library(openxlsx)

# "PROBLEM" with long term release of openxlsx:
# loadWorkbook() adds NA strings to empty cells!

# I therefore edited this source:
# https://github.com/ycphs/openxlsx/blob/master/R/loadWorkbook.R
# by adding the na.convert argument
# and changing at line 267:
# if (na.convert == TRUE) {
#   vals[vals == "<si><t/></si>"] <- "<si><t>NA</t></si>"
# }

# Edit: 2024-08-26 it seems like the above edit is now incorporated in
# the package DEVELOPMENT source!
# But not in the long-term stable release on CRAN!
# We thus need to install the development version:
# remotes::install_github("ycphs/openxlsx")
args(loadWorkbook)

# Note that default value of na.convert in development version
# is TRUE, but FALSE in my version. If we use
# development version on github, do not forget to set na.convert argument to FALSE.
