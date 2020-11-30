usethis::use_package("magrittr")
usethis::use_package("dplyr")
usethis::use_package("base")
usethis::use_package("purrr")
usethis::use_package("tidyr")
usethis::use_package("xlsx")
usethis::use_package("readODS")
usethis::use_package("stringr")
usethis::use_package("lubridate")
# Run once to configure your package to use pkgdown
# usethis::use_pkgdown()
# # Run to build the website
pkgdown::build_site()
devtools::document() # mettre à jour le namespace
