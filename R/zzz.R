.onAttach <- function(libname, pkgname)
  {
    packageStartupMessage("
    Thank you for loading geodeterminants!
    Use `?geodeterminants` to see available functions and information.
    These packages must be installed for proper functionality:
    **tidyverse**, **tidygeocoder**, **tibble**, **tidycensus**(API KEY),
    **tigris**, **sf**, **readxl**.
    ***Download csv and excel files from Github.***")
  }
