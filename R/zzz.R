.onAttach <- function(libname, pkgname)
  {
    packageStartupMessage("
Thank you for loading geodeterminants!
Use `?geodeterminants` to see available functions and information.

These packages must be installed for proper functionality:
install.packages(c(\"tidyverse\", \"tidygeocoder\", \"tibble\", \"tidycensus\", \"tigris\", \"sf\", \"readxl\"))
- tidyverse
- tidygeocoder
- tibble
- tidycensus (Individual API key required)
- tigris
- sf
- readxl

library(remotes)
install_github(\"wchan05/geodeterminants\", build_vignettes = (TRUE or FALSE))

Load with packages with: library(package_name)

***You MUST download csv and excel files from:***
https://wchan05.github.io/data_access/
")
}
