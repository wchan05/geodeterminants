.onAttach <- function(libname, pkgname)
  {
    packageStartupMessage("
Thank you for loading geodeterminants!
Use `?geodeterminants` to see available functions and information.

These packages must be installed for proper functionality (use `install.packages(\"package_name\")`):
- tidyverse
- tidygeocoder
- tibble
- tidycensus (Individual API key required)
- tigris
- sf
- readxl

Load with packages with: library(package_name)

***You MUST download csv and excel files from:***
https://wchan05.github.io/data_access/
")
}
