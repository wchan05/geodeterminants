.onAttach <- function(libname, pkgname)
  {
    packageStartupMessage("
    Thank you for loading geodeterminants!
    Use `?geodeterminants` to see available functions and information.
    These packages must be installed for proper functionality:
    **tidyverse**, **tidygeocoder**, **tibble**, **tidycensus**(API KEY),
    **tigris**, **sf**, **readxl**.
    ***You MUST download csv and excel files from [Github](https://wchan05.github.io/data_access/).***")
  }
