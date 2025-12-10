# year mods. --------------------------------------------------------------
#' Preserves year of diagnosis from the input data
#'
#' The `keep_original_year()` function will save the **original diagnosis year**
#' from the input data in a separate column, so that when the years are modified
#' to assure available and accurate data, the actual year is **NOT OVERWRITTEN**.
#'
#' @param tib A tibble of data containing addresses as `address` in characters
#'   and any corresponding data. The tibble should at least include addresses in
#'   proper format and year of diagnosis as `year`(until function
#'   `keep_original_year()` is run which then `actual_year` and `year` exist)
#'   for the most accurate data
#'
#' @returns A tibble of the original data, except the *year* column(which
#'   represents the year of diagnosis) is copied and stored as the column
#'   *actual_year*
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' keep_original_year(test_tib)
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
keep_original_year <- function(tib)
{
  if(!"year" %in% names(tib))
  {
    stop("Data must contain column 'year'")
  }
  tib <- tib %>%
    mutate(actual_year = year)
  return(tib)
}

#' Backup geocoding method
#'
#' The function `geo_fallback()` is responsible for finding the latitude and
#' longitude values of addresses that were not traceable by the census method
#' through the function `geocode()`.
#'
#' @param census_geo A tibble that has been geocoded but still has NA values
#' @param fallback The method used to find coordinates of the missing values. By default this is "arcgis" because it showed to cover the most area compared to other methods
#'
#' @returns A tibble with latitude and longitude values using arcgis to catch any missed values
#'
#' @examples
#' some_tibble <- some_tibble %>%
#' geocode(address, method = "census", lat = lat, long = long)
#'
#' geo_fallback(some_tibble)
#'
#' @importFrom dplyr filter select bind_rows
#' @importFrom tidygeocoder geocode
#' @importFrom magrittr %>%
#'
#' @export
geo_fallback <- function(census_geo, fallback = "arcgis")
{
  failed <- census_geo %>%
    filter(is.na(lat) | is.na(long)) %>%
    select(-lat, -long)
  success <- census_geo %>%
    filter(!is.na(lat) & !is.na(long))

  if(nrow(failed))
  {
    message("Re-geocoding ", nrow(failed), " address(es) using ", fallback, "...")
    geo_add <- failed %>%
      geocode(address = address, lat = "lat", long = "long", method = fallback)
    geo_max <- success %>%
      bind_rows(geo_add)
  }
  else
  {
    return(census_geo)
  }

  return(geo_max)
}

#' Checks for Tidy Census Key
#'
#' The function `check_tidycensus_key()` is used to check if the user has
#' obtained and entered a API key that would allow them to use the Tidy Census
#' package functions
#'
#' @returns TRUE if a Tidy Census Key was found and sends a message if otherwise
#'
#' @examples
#' check_tidycensus_key()
#'
#' @importFrom tidycensus census_api_key
#' @export
check_tidycensus_key <- function()
{
  key <- Sys.getenv("CENSUS_API_KEY")

  if(is.null(key) || key == "" || key == FALSE)
  {
    stop("No Census API key found. Please set one with census_api_key().")
  }
  else
  {
    message("Census API key is available.")
    return(TRUE)
  }
}

#' Modifies years to provide attainable and accurate data
#'
#' @description ⚠️ **Note:** It is important to run `keep_original_year()`
#'   before this function so that year data is not overwritten.
#'
#' The `modify_year` function takes the year of diagnosis column and adjusts the
#' years to assure that attainable and accurate data is collect. The reason for
#' this is because not every year is supported by ACS dataset and other files.
#' 2012 is the earliest data we are using because that is when full datasets
#' were released and the beginning of the more accurate ones. ACS data tends to
#' have full data up to 2 years before the current year(Ex. current year is 2025
#' so 2023 will be the most recent full data). Using this method that is how we
#' set the default year when year of diagnosis is NA or invalid.
#'
#' @inheritParams keep_original_year
#' @param current_year A 4 digit value that can represent the current year data
#'   is being used with this package or the year data was finalized
#'
#' @returns A tibble with the *address*, *actual_year*, and *year* columns as
#'   well as any other columns that were in the input tibble.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' modify_year(keep_original_year(test_tib))
#'
#' @importFrom dplyr mutate if_else
#' @importFrom magrittr %>%
#' @export
modify_year <- function(tib, current_year)
{
  if(!"year" %in% names(tib) | !"actual_year" %in% names(tib))
  {
    stop("Data must contain columns 'year' and 'actual_year' so that data is not overwritten")
  }

  tib <- tib %>%
    mutate(year = if_else(is.na(year), current_year - 2, year))
  tib <- tib %>%
    mutate(year = if_else(year >= current_year - 1, current_year - 2, year))
  tib <- tib %>%
    mutate(year = if_else(year < 2012, 2012, year))

  return(tib)
}

# function checkers -------------------------------------------------------
#' Validate Required Columns in a Tibble(including county info)
#'
#'@description ⚠️ **Note:** Similar to `checker()`, except **accounts** for
#'county data
#'
#' The `checker_with_county()` function checks that the input tibble contains
#' all required columns needed for downstream processing in the parent function.
#' This function is typically used internally to validate input data before
#' applying functions that rely on geographic and temporal metadata.
#'
#' *Checks for: address, GEOID, state, GEOID_county, county, and year columns*
#'
#' @inheritParams keep_original_year
#'
#' @returns TRUE if all conditions are met and the input tibble has information
#'   for address, GEOID, state, GEOID for county, county, and year. This does
#'   not need to be done by hand and will be there automatically as long as
#'   `SDOH_reverse()`,`add_info_cols()`,`get_GEOID()`, and `get_county_geo()`
#'   functions are run prior. This function will return FALSE if the above
#'   criteria is not met.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' checker_with_county(test_tib)
#'
#' @export
checker_with_county <- function(tib)
{
  fn_name <- as.character(sys.call(sys.parent())[[1]])
  req_cols <- c("address", "GEOID", "state", "GEOID_county", "county", "year")
  if (!all(req_cols %in% names(tib)))
  {
    stop(paste("Input tibble must have columns 'address', 'GEOID', 'state', 'GEOID_county', 'county', and 'year' for the function:", fn_name))
  }
}
#' Validate Required Columns in a Tibble
#'
#'@description ⚠️ **Note:** Similar to `checker_with_county()`, except **does
#'  not account** for county data
#'
#' The `checker()` function checks that the input tibble contains all required
#' columns needed for downstream processing in the parent function. This
#' function is typically used internally to validate input data before applying
#' functions that rely on geographic and temporal metadata.
#'
#' *Checks for: address, GEOID, state, and year columns*
#'
#' @inheritParams keep_original_year
#'
#' @returns TRUE if all conditions are met and the input tibble has information
#'   for address, GEOID, state, and year. This does
#'   not need to be done by hand and will be there automatically as long as
#'   `SDOH_reverse()`,`add_info_cols()`, and `get_GEOID()`
#'   functions are run prior. This function will return FALSE if the above
#'   criteria is not met.
#'
#' @examples
#' #' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#'
#' checker_with_county(test_tib)
#' @export
checker <- function(tib)
{
  fn_name <- as.character(sys.call(sys.parent())[[1]])
  req_cols <- c("address", "GEOID", "state", "year")
  if (!all(req_cols %in% names(tib)))
  {
    stop(paste("Input tibble must have columns 'address', 'GEOID', 'state', and 'year' for the function:", fn_name))
  }
}

# vector input ------------------------------------------------------------
#' Vector to Tibble with Information
#'
#' The `vector_to_tib()` function is a function that takes the address vector
#' and makes it into a tibble format, collecting information such as setting
#' default dates, geocoding & reverse geocoding information, and geoids for
#' tract and county levels. The function finishes by tying all the data together
#' into a tibble that originated from the vector of addresses and returns it.
#' The purpose of transferring to a tibble is because it is required for
#' obtaining the social determinants of health and other functions.
#'
#' @param addresses A vector of addresses
#' @param current_year The current year
#'
#' @returns A tibble with the address and corresponding data about that location(geographical divisions and geoids)
#'
#' @examples
#' #' addresses <- c(
#' "1600 Pennsylvania Ave NW, Washington, DC 20500",
#' "1 Infinite Loop, Cupertino, CA 95014",
#' "350 5th Ave, New York, NY 10118",
#' "233 S Wacker Dr, Chicago, IL 60606",
#' "15 Main Street Flemington, NJ 08822",
#' "Non existent Ave")
#'
#' geocoded <- vector_to_tib(addresses)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select rename left_join filter distinct bind_rows
#' @importFrom tidygeocoder geocode reverse_geocode
#' @importFrom sf st_as_sf st_transform st_join st_within st_drop_geometry
#' @importFrom tigris tracts counties
#'
#' @export
vector_to_tib <- function(addresses, current_year)
{
  if(length(addresses) == 0)
  {
    stop("address vector is empty")
  }
  if(!is.character(addresses))
  {
    stop("address vector type is invalid")
  }
  if(any(is.na(addresses)))
  {
    warning("address vector contains na values")
  }
  suppressMessages({
  geocoded <- tibble(address = addresses)
  geocoded <- geocoded %>%
    mutate(actual_year = NA) %>%
    mutate(year = current_year - 2) %>%
    geocode(address, method = "census", lat = lat, long = long) %>%
    geo_fallback() %>%
    mutate(valid_coords = (!is.na(lat) & !is.na(long))) %>%
    reverse_geocode(lat = lat, long = long, method = "osm", full_results = TRUE) %>%
    select(address = address...1, actual_year, year, lat, long, city, state, county, country)})

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR", "GU", "VI", "AS", "MP"),
    state = c(state.name, "District of Columbia", "Puerto Rico", "Guam", "U.S. Virgin Islands", "American Samoa", "Northern Mariana Islands"))

  geocoded <- geocoded %>%
    left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr)

  coords_sf <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  results_tract <- tibble()
  results_county <- tibble()

  pairs <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    select(state, year) %>%
    distinct() %>%
    split(1:nrow(.))

  for(pair in pairs)
  {
    state_code <- pair$state
    year_val <- pair$year

    subset_sf <- coords_sf %>%
      filter(state == state_code, year == year_val)

    if (nrow(subset_sf) == 0)
    {
      next
    }
    #tract
    suppressMessages({
    tracts_sf <- tracts(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    joined <- st_join(subset_sf, tracts_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID, year)

    results_tract <- bind_rows(results_tract, joined)
    #county
    suppressMessages({
    counties_sf <- counties(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    joined <- st_join(subset_sf, counties_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID_county = GEOID, year)

    results_county <- bind_rows(results_county, joined)
  }
  results_tract <- results_tract %>% distinct()
  results_county <- results_county %>% distinct()
  geocoded <- geocoded %>%
    left_join(results_tract, by = c("address", "year")) %>%
    left_join(results_county, by = c("address", "year"))

  return(geocoded)
}


# tib input ---------------------------------------------------------------
#' Current Tibble Receives the Necessary Data
#'
#' The `geo_info()` function is for when the input data is in tibble format
#' already. The function collects data such as geocoding & reverse geocoding
#' information and geoids for tract and county levels. The function finishes by
#' returning this newly updated tibble that is ready for the analysis in
#' following functions.
#'
#' @param tib A tibble that contains at least *address* column, plus any supplementary data
#'
#' @returns A tibble with the address and corresponding data about that location(geographical divisions and geoids)
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#'
#' test_tib <- geo_info(test_tib)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr select rename left_join filter distinct bind_rows
#' @importFrom tidygeocoder geocode reverse_geocode
#' @importFrom sf st_as_sf st_transform st_join st_within st_drop_geometry
#' @importFrom tigris tracts counties
#'
#' @export
geo_info <- function(tib)
{
  required_cols <- c("address")
  if (!all(required_cols %in% names(tib)))
  {
    stop("Input tibble must contain 'address' column")
  }

  suppressMessages({
  geocoded <- tib %>% select(address, actual_year, year) %>%
    geocode(address = address, method = "census", long = long, lat = lat) %>%
    geo_fallback() %>%
    reverse_geocode(lat = lat, long = long, method = "osm", full_results = TRUE) %>%
    select(address = address...1, actual_year, year, lat, long, city, state, county, country)})

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR", "GU", "VI", "AS", "MP"),
    state = c(state.name, "District of Columbia", "Puerto Rico", "Guam", "U.S. Virgin Islands", "American Samoa", "Northern Mariana Islands"))

  geocoded <- geocoded %>%
    left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr)

  coords_sf <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  results_tract <- tibble()
  results_county <- tibble()

  pairs <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    select(state, year) %>%
    distinct() %>%
    split(1:nrow(.))

  for(pair in pairs)
  {
    state_code <- pair$state
    year_val <- pair$year

    subset_sf <- coords_sf %>%
      filter(state == state_code, year == year_val)

    if (nrow(subset_sf) == 0)
    {
      next
    }
    #tracts
    suppressMessages({
    tracts_sf <- tracts(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    joined <- st_join(subset_sf, tracts_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID, year)

    results_tract <- bind_rows(results_tract, joined)
    #county
    suppressMessages({
    counties_sf <- counties(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    joined <- st_join(subset_sf, counties_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID_county = GEOID, year)

    results_county <- bind_rows(results_county, joined)
  }
  results_tract <- results_tract %>% distinct()
  results_county <- results_county %>% distinct()

  look_out <- c("lat", "long", "city", "state", "county", "country", "GEOID", "GEOID_county")
  tib <- tib %>%
    select(-any_of(look_out))

  geocoded <- geocoded %>%
    left_join(results_tract, by = c("address", "year")) %>%
    left_join(results_county, by = c("address", "year"))

  geocoded <- tib %>%
    left_join(geocoded, by = c("address", "actual_year", "year"))

  return(geocoded)
}

# Geocode function --------------------------------------------------------
#' Convert vector of addresses to compatible tibble
#'
#' The use of this function is for when only a vector of addresses is provided.
#' The `vector_geocode_to_tib()` function first validates that the vector is in
#' proper format. It checks if the vector is empty, if all string values are
#' present, and warns the user about NA values. The addresses are then put into
#' a tibble, geocoded, and returned in tibble data.
#'
#' @param addresses A vector of addresses
#'
#' @returns A tibble that has the addresses from the original vector and
#'   geocoded information for those that it is available.
#'
#' @examples
#' addresses <- c(
#' "1600 Pennsylvania Ave NW, Washington, DC 20500",
#' "1 Infinite Loop, Cupertino, CA 95014",
#' "350 5th Ave, New York, NY 10118",
#' "233 S Wacker Dr, Chicago, IL 60606",
#' "15 Main Street Flemington, NJ 08822",
#' "Non existent Ave")
#'
#' vector_geocode_to_tib(addresses)
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @export
vector_geocode_to_tib <- function(addresses)
{
  # Check if empty
  if(length(addresses) == 0)
  {
    stop("address vector is empty")
  }
  # Check if strings
  if(!is.character(addresses))
  {
    stop("address vector type is invalid")
  }
  # Warn about NA values
  if(any(is.na(addresses)))
  {
    warning("address vector contains na values")
  }

  # Makes tibble of addresses and their coordinates or NA value
  geocoded <- tibble(address = addresses)
  suppressMessages({
  geocoded <- geocoded %>%
    tidygeocoder::geocode(address, method = "census", lat = lat, long = long) %>%
    geo_fallback() %>%
    mutate(valid_coords = (!is.na(lat) & !is.na(long)))})

  return(geocoded)
}

# Getting important basic information about address -----------------------
#' Appends geographical categorical information
#'
#' The `SDOH_reverse()` function is used to fill in missing information about an
#' *address* and for joining data later on. The intent is to be used after
#' `vector_geocode_to_tib()`. Information like *city*, *state*, *county*, and
#' *country* for a location will be appended.
#'
#' @param geo_tib A tibble containing addresses and geocoding information,
#'   returned from `vector_geocode_to_tib()`
#'
#' @returns A tibble of addresses, their geocoded information, and general
#'   information about their location including: `city`, `state`, `county`, and
#'   `country`
#'
#' @examples
#' addresses <- c(
#' "1600 Pennsylvania Ave NW, Washington, DC 20500",
#' "1 Infinite Loop, Cupertino, CA 95014",
#' "350 5th Ave, New York, NY 10118",
#' "233 S Wacker Dr, Chicago, IL 60606",
#' "15 Main Street Flemington, NJ 08822",
#' "Non existent Ave")
#'
#' geocoded <- vector_geocode_to_tib(addresses)
#'
#' SDOH_reverse(geocoded)
#' @importFrom tibble tibble
#' @importFrom dplyr select left_join rename
#' @importFrom tidygeocoder reverse_geocode
#' @importFrom magrittr %>%
#'
#' @export
SDOH_reverse <- function(geo_tib)
{
  fn_name <- as.character(sys.call()[[1]])
  req_cols <- c("address", "lat", "long")
  if (!all(req_cols %in% names(geo_tib)))
  {
    stop(paste("Input tibble must have columns 'address', 'lat', and 'long' for the function:", fn_name))
  }
  suppressMessages({
  updated_tib <- geo_tib %>%
    reverse_geocode(lat = lat, long = long, method = "osm", full_results = TRUE) %>%
    select(address = address...1, valid_coords, city, state, county, country)})

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC"),
    state = c(state.name, "District of Columbia"))

  updated_tib <- updated_tib %>%
    left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr)

  return(updated_tib)
}

# state ---------------------------------------------------------------
#' Appends geographical categorical information(no starting coords)
#'
#' The `add_info_cols()` function takes the *addresses* and *years*, and matches
#' **coordinate data** from geocoding with **geographical categorical data**.
#' All data that was previous loaded into the input tibble is preserved and
#' included in the return tibble of this function and all function moving
#' forward.
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with columns for address, year, `city`, `county`, `country`, and
#'   any prior loaded data columns
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#'
#' add_info_cols(test_tib)
#'
#' @importFrom dplyr select left_join
#' @importFrom tidygeocoder geocode reverse_geocode
#' @importFrom magrittr %>%
#'
#' @export
add_info_cols <- function(tib)
{
  suppressMessages({
  new_data <- tib %>%
    tidygeocoder::geocode(address, method = "census") %>%
    geo_fallback()
  new_data <- new_data %>%
    tidygeocoder::reverse_geocode(lat = lat, long = long, method = "osm", full_results = TRUE) %>%
    select(address = address...1, city, county, country, year)})
    tib <- left_join(tib, new_data, by = c("address", "year"))
  return(tib)
}

# GEOID -------------------------------------------------------------------
#' Collects tract level GEOIDs for addresses
#'
#' The `get_GEOID()` function uses geocoding and spatial features in order to
#' obtain the GEOIDs for specific addresses. Using the **Coordinate Reference
#' System(CRS) 4326**, *cordinates* and *state-year pairs* are created as **sf
#' objects** and they are joined **within** to get the results.
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with prior data from the input tibble along with `GEOID`
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#'
#' get_GEOID(test_tib)
#'
#' @importFrom tidygeocoder geocode
#' @importFrom dplyr filter select distinct bind_rows left_join
#' @importFrom tibble tibble
#' @importFrom sf st_as_sf st_transform st_join st_within st_drop_geometry
#' @importFrom tigris tracts
#' @importFrom magrittr %>%
#' @export
get_GEOID <- function(tib)
{
  required_cols <- c("address")
  if (!all(required_cols %in% names(tib)))
  {
    stop("Input tibble must contain 'address' column")
  }

  suppressMessages({
  geocoded <- tib %>%
    geocode(address = address, method = "census", long = long, lat = lat)}) %>%
    geo_fallback()

  failed_geocodes <- geocoded %>%
    filter(is.na(long) | is.na(lat))

  if (nrow(failed_geocodes) > 0)
  {
    message("The following addresses could not be geocoded and will be returned with NA GEOID:\n",
            paste0("- ", failed_geocodes$address, collapse = "\n"))
  }

  # Convert successful geocodes to sf
  coords_sf <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  results <- tibble()

  pairs <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    select(state, year) %>%
    distinct() %>%
    split(1:nrow(.))

  for(pair in pairs)
  {
    state_code <- pair$state
    year_val <- pair$year

    subset_sf <- coords_sf %>%
      filter(state == state_code, year == year_val)

    if (nrow(subset_sf) == 0)
    {
      next
    }

    # Get tract shapefiles
    suppressMessages({
    tracts_sf <- tracts(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    # Spatial join
    joined <- st_join(subset_sf, tracts_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID, year)

    results <- bind_rows(results, joined)
  }
  tib_with_geoid <- geocoded %>%
    left_join(results, by = c("address", "year")) %>%
    select(-long, -lat)

  return(tib_with_geoid)
}

# County GEOID ------------------------------------------------------------
#' Collects county level GEOIDS for addresses
#'
#' The `get_county_geo()` function uses geocoding and spatial features in order to
#' obtain the GEOIDs for specific addresses. Using the **Coordinate Reference
#' System(CRS) 4326**, *cordinates* and *state-year pairs* are created as **sf
#' objects** and they are joined **within** to get the results.
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with prior data from the input tibble along with `GEOID_county`
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#'
#' get_county_geo(test_tib)
#'
#' @importFrom magrittr %>%
#' @importFrom tidygeocoder geocode
#' @importFrom dplyr filter select distinct bind_rows left_join
#' @importFrom tibble tibble
#' @importFrom sf st_as_sf st_transform st_join st_within st_drop_geometry
#' @importFrom tigris counties
#'
#' @export
get_county_geo <- function(tib)
{
  required_cols <- c("address")
  if (!all(required_cols %in% names(tib)))
  {
    stop("Input tibble must contain 'address' column.")
  }

  suppressMessages({
  geocoded <- tib %>%
    select(address, year, state) %>%
    geocode(address = address, method = "census", long = long, lat = lat)}) %>%
    geo_fallback()

  failed_geocodes <- geocoded %>%
    filter(is.na(long) | is.na(lat))

  if (nrow(failed_geocodes) > 0)
  {
    message("The following addresses could not be geocoded and will be returned with NA GEOID:\n",
            paste0("- ", failed_geocodes$address, collapse = "\n"))
  }

  # Convert successful geocodes to sf
  coords_sf <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

  results <- tibble()

  pairs <- geocoded %>%
    filter(!is.na(long) & !is.na(lat)) %>%
    select(state, year) %>%
    distinct() %>%
    split(1:nrow(.))

  for (pair in pairs)
  {
    state_code <- pair$state
    year_val <- pair$year

    subset_sf <- coords_sf %>%
      filter(state == state_code, year == year_val)

    if (nrow(subset_sf) == 0)
    {
      next
    }

    # Get tract shapefiles
    suppressMessages({
    counties_sf <- counties(state = state_code, year = year_val, class = "sf") %>%
      st_transform(crs = 4326)})

    # Spatial join
    joined <- st_join(subset_sf, counties_sf, join = st_within) %>%
      st_drop_geometry() %>%
      select(address, GEOID_county = GEOID, year)

    results <- bind_rows(results, joined)
  }
  tib_with_geoid <- geocoded %>%
    left_join(results, by = c("address", "year")) %>%
    select(-long, -lat)
  tib <- left_join(tib, tib_with_geoid, by = c("address", "state", "year"))

  return(tib)
}
# Education Attainment ----------------------------------------------------
#' Joins a Column for Education Attainment
#'
#' This function extracts and processes educational attainment data from the
#' U.S. Census Bureau’s American Community Survey (ACS) to assess the
#' educational environment of a respondent's geographic area. The measure is
#' based on the percentage of individuals with a bachelor’s degree or higher in
#' a given census tract(community based). The data are sourced from ACS 5-year estimates and
#' updated annually. The function requires a geocoded address to determine the
#' respondent's location and linka it to the appropriate ACS statistics. Users
#' can access ACS data from the U.S. Census Bureau via [Data.census.gov](https://data.census.gov/table/ACSDT5Y2020.B15003).
#'
#' @details Variables are extracted from ACS Table B15003 - Education Attainment
#'   for population 25 years old +
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with column `education_attainment` added to the data of the
#' input tibble
#'
#' @references
#' Source:
#'
#' U.S. Census Bureau. (n.d.). Data.census.gov. [Retrieved from](http://data.census.gov)
#' U.S. Census Bureau. (n.d.). American Community Survey (ACS) products (specifically, the 5-year estimates). [Retrieved from](http://www.census.gov/programs-surveys/acs)
#'
#' General References:
#'
#' Agency for Healthcare Research and Quality (AHRQ). (2015). Understanding the relationship between education and health: A review of the evidence and an examination of community perspectives. [Retrieved from](http://www.ahrq.gov/professionals/education/curriculum-tools/population-health/zimmerman.html)
#' Arcaya, M., Tucker-Seeley, R., Kim, R., Schnake-Mahl, A., So, M., & Subramanian, S.V. (2016). Research on neighborhood effects on health in the United States: A systematic review of study characteristics. Social Science & Medicine, 168, 16–29.
#' Cohen, S. S., Mumma, M. T., Dupree, E. E., & Boice, J. D., Jr. (2018). Validating the use of census data on education as a measure of socioeconomic status in an occupational cohort. International Journal of Radiation Biology, 19, 1–10.
#' Connelly, R., Gayle, V., & Lambert, P. S. (2016). A review of educational attainment measures for social survey research. Methodological Innovations, 9.
#' Friedman, J., Graetz, N., & Gakidou, E. (2018). Improving the estimation of educational attainment: New methods for assessing average years of schooling from binned data. PLoS One, 13(11), e0208019.
#' Geronimus, A. T., & Bound, J. (1998). Use of census-based aggregate variables to proxy for socioeconomic group: Evidence from national samples. American Journal of Epidemiology, 148(5), 475–486.
#' Krueger, P. M., Dehry, I. A., & Chang, V. W. (2019). The economic value of education for longer lives and reduced disability. Milbank Quarterly, 97(1), 48–73.
#' McElroy, J. A., Remington, P. L., Trentham-Dietz, A., Robert, S. A., & Newcomb, P. A. (2003). Geocoding addresses from a large population-based study: Lessons learned. Epidemiology, 14, 399–407.
#' Pardo-Crespo, M. R., Narla, N. P., Williams, A. R., Beebe, T. J., Sloan, J., Yawn, B. P., … Juhn, Y. J. (2013). Comparison of individual-level versus area-level socioeconomic measures in assessing health outcomes of children in Olmsted County, Minnesota. Journal of Epidemiology and Community Health, 67(4), 305–310. PMID: 23322850
#' Ryan, C. L., & Bauman, K. (2016). Educational attainment in the United States: 2015. P20-578, 1–11. [Retrieved from](https://www.census.gov/library/publications.html)
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_education_attainment(test_tib)
#'
#' @importFrom dplyr filter select distinct mutate bind_rows left_join group_by summarize
#' @importFrom tibble tibble
#' @importFrom tidycensus get_acs
#' @importFrom scales percent
#' @importFrom magrittr %>%
#'
#' @export
add_education_attainment <- function(tib)
{
  checker(tib)

  tib_valid <- tib %>% filter(!is.na(GEOID))
  tib_invalid <- tib %>% filter(is.na(GEOID)) %>%
    mutate(education_attainment = NA_character_)

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()

  denominator_all <- tibble()
  numerator_all <- tibble()

  for (i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]
    # Get total pop count for 25 yrs old +
    suppressMessages({
    denom <- get_acs(
      geography = "tract",
      state = st,
      variables = c(twenty_five_and_older = "B15003_001"),
      year = yr,
      survey = "acs5",
      geometry = FALSE) %>%
      select(GEOID, twenty_five_and_older = estimate) %>%
      mutate(state = st, year = yr)
    # Cleaned and combined
    denominator_all <- bind_rows(denominator_all, denom)
    # Get college degree holders
    edu <- get_acs(
      geography = "tract",
      state = st,
      variables = c(
        Bachelor = "B15003_022",
        Master = "B15003_023",
        Professional = "B15003_024",
        Doctorate = "B15003_025"),
      year = yr,
      survey = "acs5",
      geometry = FALSE) %>%
      select(GEOID, variable, estimate) %>%
      group_by(GEOID) %>%
      summarize(college_degree = sum(estimate, na.rm = TRUE), .groups = "drop") %>%
      mutate(state = st, year = yr)
    # Cleaned and Combined
    numerator_all <- bind_rows(numerator_all, edu)})
  }
  edu_data <- left_join(denominator_all, numerator_all, by = c("GEOID", "state", "year"))

  tib_valid <- left_join(tib_valid, edu_data, by = c("GEOID", "state", "year")) %>%
    mutate(
      education_attainment_community = ifelse(twenty_five_and_older > 0,
                                    (college_degree / twenty_five_and_older) * 100,
                                    NA_real_),
      education_attainment_community = scales::percent(education_attainment_community / 100, accuracy = 0.01)
    ) %>%
    select(-twenty_five_and_older, -college_degree)

  result <- bind_rows(tib_valid, tib_invalid)
  return(result)
}

# Dissimilarity Index --------------------------------------------------------
  # For larger population says to use metropolitan area
  # Not Hispanic or Latino White alone (suggested reference group do to compatibility with other measures and studies)
#' Joins a Column for Residential Segregation: Dissimilarity Index
#'
#' @description
#' The `add_dissimilarity_index()` function calculates the `dissimilarity_index`
#' to assess residential segregation between two population groups (typically a
#' **minority group** and a **reference group**, non-Hispanic whites in most
#' studies) using U.S. Census Bureau data. Residential segregation reflects the
#' distribution of different race/ethnic groups across *census tracts* within a
#' larger area, *County* in this case. The Dissimilarity Index is a commonly
#' used measure of segregation that quantifies how evenly two groups are
#' distributed across geographic areas.
#'
#' This implementation uses ACS 5-year estimates for race/ethnic population
#' counts extracted from Table B03002: "Hispanic or Latino by Race." A higher
#' index value indicates greater segregation, ranging from 0 (complete
#' integration) to 1 (complete segregation).
#' [Data.census.gov](https://data.census.gov/table/ACSDT1Y2022.B03002).
#'
#' @details
#' The Dissimilarity Index (D) is calculated as:
#' \deqn{D = \frac{1}{2} \sum_{i=1}^n \left| \frac{x_i}{X} - \frac{y_i}{Y} \right|}
#' where:
#' \itemize{
#'   \item \eqn{x_i} is the minority group population in tract i
#'   \item \eqn{X} is the total minority population across all tracts
#'   \item \eqn{y_i} is the reference group population in tract i
#'   \item \eqn{Y} is the total reference group population across all tracts
#' }
#'
#' @inheritParams keep_original_year
#' @param minority_group_code The B03002 table code of the minority group of
#'   interest. If the code isn't known leave null and select from the menu
#'
#' @returns A tibble with column `dissimilarity_index` added to the data of the
#' input tibble
#'
#' @references
#' Source:
#'
#' Recommended data sources include the following:
#'
#' U.S. Census Bureau American Community Survey (ACS) data products (5-year estimates). [Source](http://www.census.gov/programs-surveys/acs)
#' [American Factfinder website](http://factfinder.census.gov)
#'
#' Note that several online sources provide Dissimilarity Index scores for selected metropolitan statistical areas, counties, and school districts (and across Census years). See, for example, the [American Communities Project and the School Segregation Project at the Brown and Lewis Mumford Center at Albany](http://www.s4.brown.edu/cen2000/index.html: http://www.s4.brown.edu/schoolsegregation/index.htm) as well as the [Spatial Impact Factor Web Data at RTI International](http://rtispatialdata.rti.org/).
#'
#' General References:
#'
#' Iceland, J., & Douzet, F. (2006). Measuring racial and ethnic segregation. Hrodote, 122(3): 25-43.
#' Iceland, J., Weinberg, D. H., & Steinmetz, E. (2002). Racial and ethnic residential segregation in the United States: 1980-2000 (U.S. Census Bureau, Series CENSR‑3). Washington DC: U.S. Government Printing Office.
#' Massey, D. S., & Denton, N. A. (1988). The dimensions of residential segregation. Social Forces, 67, 281-315.
#' Morgan, P. M., Murphy, R. F., Willis, R. A., Hubbard, D. W., & Norton, J. M. (1975). Dental health of Louisiana residents based on the ten-state nutrition survey. Public Health Reports, 90(2), 173-178.
#' Morrill, R. L. (1995). Aging in place, age specific migration and natural decrease. Annals of Regional Science, 29(1), 41-66.
#' Reardon, S. F. (2006). A conceptual framework for measuring segregation and its associations with population outcomes. In J. M. Oakes & J. S. Kaufman (Eds.), Methods in social epidemiology (pp. 169-192). San Francisco, CA: Wiley and Sons/Jossey-Bass.
#' Reardon, S. F., & Firebaugh, G. (2002). Measures of multi-group segregation. Sociological Methodology, 32, 33-67.
#' Reardon, S. F., Matthews, S. A., O’Sullivan, D., Lee, B. A., Firebaugh, G., Farrell, C. R., & Bischoff, K. (2008). The geographic scale of metropolitan racial segregation. Demography, 45(3), 489-514.
#' Reardon, S. F., & O’Sullivan, D. (2004). Measures of spatial segregation. Sociological Methodology, 34, 121-162.
#' Sakoda, J. M. (1981). A generalized index of dissimilarity. Demography, 18(2), 245-250.
#' Taeuber, K. E., & Taeuber, A. F. (1965). Negroes in cities: Residential segregation and neighborhood change. Chicago, IL: Aldine.
#' Theil, H. (1972). Statistical decomposition analysis (Vol. 14). Amsterdam, Netherlands: North-Holland.
#' White, M. J. (1983). The measurement of spatial segregation. American Journal of Sociology, 88, 1008-1018.
#' White, M. J. (1986). Segregation and diversity measures in population distribution. Population Index, 52, 198-221.
#' Wong, D. S. (1993). Spatial indices of segregation. Urban Studies, 30, 559-572.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_dissimilarity_index(test_tib)
#'
#' @importFrom dplyr filter select distinct mutate case_when group_by ungroup left_join bind_rows rename
#' @importFrom stringr str_detect str_split_fixed str_trim
#' @importFrom tibble tibble
#' @importFrom tidycensus get_acs
#' @importFrom utils menu
#' @importFrom magrittr %>%
#'
#' @export
add_dissimilarity_index <- function(tib, minority_group_code = NULL)
{
  checker_with_county(tib)

  if(is.null(minority_group_code))
  {
    minority_groups <- c(
      "Not Hispanic or Latino" = "B03002_002",
      "Not Hispanic or Latino White alone" = "B03002_003",
      "Not Hispanic or Latino Black or African American alone" = "B03002_004",
      "Not Hispanic or Latino American Indian and Alaska Native alone" = "B03002_005",
      "Not Hispanic or Latino Asian alone" = "B03002_006",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_007",
      "Not Hispanic or Latino Some other race alone" = "B03002_008",
      "Not Hispanic or Latino Two or more races" = "B03002_009",
      "Not Hispanic or Latino Two races including Some other race" = "B03002_010",
      "Not Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_011",
      "Hispanic or Latino" = "B03002_012",
      "Hispanic or Latino White alone" = "B03002_013",
      "Hispanic or Latino Black or African American alone" = "B03002_014",
      "Hispanic or Latino American Indian and Alaska Native alone" = "B03002_015",
      "Hispanic or Latino Asian alone" = "B03002_016",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_017",
      "Hispanic or Latino Some other race alone" = "B03002_018",
      "Hispanic or Latino Two or more races" = "B03002_019",
      "Hispanic or Latino Two races including Some other race" = "B03002_020",
      "Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_021"
    )
    choice <- menu(names(minority_groups), title = "Select the group you want the dissimilarity index for:")
    minority_group <- minority_groups[choice]

    cat("You selected:", names(minority_group), "-", minority_group, "\n")
  }
  else
  {
    minority_group <- minority_group_code
  }

  # most compatible with other data/studies
  reference_group <- "B03002_003"

  tib_valid <- tib %>% filter(!is.na(GEOID) & !is.na(GEOID_county))

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()

  reference_num <- tibble()
  reference_denom <- tibble()
  subject_num <- tibble()
  subject_denom <- tibble()

  for(i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]
    suppressMessages({
    tot_data_reference <- get_acs(
      geography = "county",
      variables = c(tot_pop_reference = reference_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      select(GEOID_county = GEOID, reference_group = estimate) %>%
      mutate(year = yr)
    reference_denom <- bind_rows(reference_denom, tot_data_reference) %>%
      distinct()

    tot_data_subject <- get_acs(
      geography = "county",
      variables = c(tot_pop_subject = minority_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      select(GEOID_county = GEOID, subject_group = estimate) %>%
      mutate(year = yr)
    subject_denom <- bind_rows(subject_denom, tot_data_subject) %>%
      distinct()

    tract_data_reference <- get_acs(
      geography = "tract",
      variables = c(reference_pops = reference_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      select(GEOID, NAME, reference_sum = estimate) %>%
      mutate(year = yr)
    reference_num <- bind_rows(reference_num, tract_data_reference) %>%
      distinct()

    tract_data_subject <- get_acs(
      geography = "tract",
      variables = c(subject_pops = minority_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      select(GEOID, NAME, subject_sum = estimate) %>%
      mutate(year = yr)
    subject_num <- bind_rows(subject_num, tract_data_subject) %>%
      distinct()})
  }
  reference_num <- reference_num %>%
    mutate(
      county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
      state = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3])))
  subject_num <- subject_num %>%
    mutate(
      county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
      state = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3])))

  tib_valid <- tib_valid %>%
    left_join(reference_denom, by = c("GEOID_county", "year"))
  tib_valid <- tib_valid %>%
    left_join(subject_denom, by = c("GEOID_county", "year"))

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR"),
    state = c(state.name, "District of Columbia", "Puerto Rico"))

  reference_num <- reference_num %>%
    left_join(state_lookup, by = "state") %>%
    select(-state, -NAME)
  subject_num <- subject_num %>%
    left_join(state_lookup, by = "state") %>%
    select(-state, -NAME)

  sums <- left_join(reference_num, subject_num, by = c("GEOID", "county", "state_abbr", "year")) %>%
    rename(state = state_abbr) %>% distinct() %>%
    mutate(county = county %>%
             str_replace("Municipio", "") %>%
             str_squish())

  tib_valid <- tib_valid %>%
    left_join(sums, by = c("GEOID", "state", "year"))

  tib_valid <- tib_valid %>%
    filter(subject_group > 0, reference_group > 0)

  tib_valid <- tib_valid %>%
    mutate(difference = abs((subject_sum / subject_group) - (reference_sum / reference_group))) %>%
    group_by(address, year) %>%
    mutate(summation = sum(difference)) %>%
    ungroup() %>%
    select(-reference_sum, -subject_sum, -reference_group, -subject_group, -difference) %>%
    mutate(dissimilarity_index = summation * 0.5) %>%
    select(-summation) %>%
    distinct()
  tib <- tib %>%
    left_join(tib_valid %>%
                select(address, year, dissimilarity_index), by = c("address", "year"))

  return(tib)
}
# Separation Index --------------------------------------------------------
# Not Hispanic or Latino White alone (suggested reference group do to compatibility with other measures and studies)
#' Joins a Column for Residential Segregation: Separation Index
#'
#' @description
#' The `add_separation_index()` function extracts race/ethnicity data from the
#' U.S. Census Bureau's American Community Survey (ACS) using the `tidycensus`
#' package and calculates Separation Index (S) for measuring residential
#' segregation at the *census tract* level within a given *County*.
#'
#' The indices are calculated using Table B03002: "Hispanic or Latino by Race"
#' to reflect the evenness of race/ethnic group distribution across smaller
#' geographic units relative to a larger area. [Data.census.gov](https://data.census.gov/table/ACSDT1Y2022.B03002).
#'
#' @details \deqn{p_i = \frac{n_{1i}}{n_{1i} + n_{2i}}}
#'
#' where \eqn{n_{1i}} is the tract-level estimate of the reference group and
#' \eqn{n_{2i}} is that of the comparison group.
#'
#' Similarly, the proportion of the reference group in the county is calculated
#' as:
#'
#' \deqn{P = \frac{N_1}{N_1 + N_2}}
#'
#' where \eqn{N_1} and \eqn{N_2} are the county-level totals of the reference
#' and comparison groups, respectively.
#'
#' The separation index is then defined as the absolute difference between the
#' tract-level and county-level proportions:
#'
#' \deqn{S_i = |p_i - P|} The separation index ranges from 0 to 1: 0 being no
#' separation and 1 being complete separation.
#'
#' @inheritParams keep_original_year
#' @param comparison_group_code The B03002 table code of the comparison group of
#'   interest. If the code isn't known leave null and select from the menu
#'
#' @returns A tibble with column `separation_index` added to the data of the
#' input tibble
#'
#' @references
#' Source:
#'
#' Brown University. (2019). Spatial structures in social sciences. Retrieved May 28, 2019, [from](https://www.brown.edu/academics/spatial-structures-in-social-sciences/)
#'   Fossett, M. (2017). New methods for measuring and analyzing segregation. Springer.
#' U.S. Census Bureau. (2019). American Community Survey (ACS) data products (5-year estimates). Retrieved May 28, 2019, [from](http://www.census.gov/programs-surveys/acs)
#' U.S. Census Bureau. (2019). American FactFinder. Retrieved May 28, 2019, [from](http://factfinder.census.gov)
#'
#' General References:
#'
#' Carrington, W. J., & Troske, K. R. (1997). On measuring segregation in samples with small units. Journal of Business & Economic Statistics, 15(4), 402–409.
#' Fossett, M. (2017). New methods for measuring and analyzing segregation. Springer.
#' Iceland, J., & Douzet, F. (2006). Measuring racial and ethnic segregation. Herodote, 122(3), 25–43.
#' Iceland, J., Weinberg, D. H., & Steinmetz, E. (2002). Racial and ethnic residential segregation in the United States: 1980-2000 (U.S. Census Bureau, Series CENSR-3). Washington, DC: U.S. Government Printing Office.
#' James, D. R., & Taeuber, K. E. (1985). Measures of segregation. Sociological Methodology, 15, 1–32.
#' Massey, D. S., & Denton, N. A. (1988). The dimensions of residential segregation. Social Forces, 67, 281–315.
#' Morgan, P. M., Murphy, R. F., Willis, R. A., Hubbard, D. W., & Norton, J. M. (1975). Dental health of Louisiana residents based on the ten-state nutrition survey. Public Health Reports, 90(2), 173–178.
#' Morrill, R. L. (1995). Aging in place, age specific migration and natural decrease. Annals of Regional Science, 29(1), 41–66.
#' Reardon, S. F. (2006). A conceptual framework for measuring segregation and its associations with population outcomes. In J. M. Oakes & J. S. Kaufman (Eds.), Methods in social epidemiology (pp. 169–192). San Francisco, CA: Wiley and Sons/Jossey-Bass.
#' Reardon, S. F., & Firebaugh, G. (2002). Measures of multi-group segregation. Sociological Methodology, 32, 33–67.
#' Reardon, S. F., Matthews, S. A., O’Sullivan, D., Lee, B. A., Firebaugh, G., Farrell, C. R., & Bischoff, K. (2008). The geographic scale of metropolitan racial segregation. Demography, 45(3), 489–514.
#' Reardon, S. F., & O’Sullivan, D. (2004). Measures of spatial segregation. Sociological Methodology, 34, 121–162.
#' Sakoda, J. M. (1981). A generalized index of dissimilarity. Demography, 18(2), 245–250.
#' Taeuber, K. E., & Taeuber, A. F. (1965). Negroes in cities: Residential segregation and neighborhood change. Chicago, IL: Aldine.
#' Theil, H. (1972). Statistical decomposition analysis (vol. 14). Amsterdam, Netherlands: North-Holland.
#' White, M. J. (1983). The measurement of spatial segregation. American Journal of Sociology, 88, 1008–1018.
#' White, M. J. (1986). Segregation and diversity measures in population distribution. Population Index, 52, 198–221.
#' Winship, C. (1977). A revaluation of indexes of residential segregation. Social Forces, 55(4), 1058–1066.
#' Wong, D. S. (1993). Spatial indices of segregation. Urban Studies, 30, 559–572.
#' Zoloth, B. S. (1976). Alternative measures of school segregation. Land Economics, 52(3), 278–298.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_separation_index(test_tib)
#'
#' @importFrom dplyr filter select distinct mutate case_when left_join bind_rows group_by ungroup
#' @importFrom tibble tibble
#' @importFrom tidycensus get_acs
#' @importFrom stringr str_detect str_split_fixed str_trim
#' @importFrom utils menu
#' @importFrom magrittr %>%
#'
#' @export
add_separation_index <- function(tib, comparison_group_code = NULL)
{
  checker_with_county(tib)
  if(is.null(comparison_group_code))
  {
    comparison_groups <- c(
      "Not Hispanic or Latino" = "B03002_002",
      "Not Hispanic or Latino White alone" = "B03002_003",
      "Not Hispanic or Latino Black or African American alone" = "B03002_004",
      "Not Hispanic or Latino American Indian and Alaska Native alone" = "B03002_005",
      "Not Hispanic or Latino Asian alone" = "B03002_006",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_007",
      "Not Hispanic or Latino Some other race alone" = "B03002_008",
      "Not Hispanic or Latino Two or more races" = "B03002_009",
      "Not Hispanic or Latino Two races including Some other race" = "B03002_010",
      "Not Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_011",
      "Hispanic or Latino" = "B03002_012",
      "Hispanic or Latino White alone" = "B03002_013",
      "Hispanic or Latino Black or African American alone" = "B03002_014",
      "Hispanic or Latino American Indian and Alaska Native alone" = "B03002_015",
      "Hispanic or Latino Asian alone" = "B03002_016",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_017",
      "Hispanic or Latino Some other race alone" = "B03002_018",
      "Hispanic or Latino Two or more races" = "B03002_019",
      "Hispanic or Latino Two races including Some other race" = "B03002_020",
      "Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_021"
    )
    choice <- menu(names(comparison_groups), title = "Select the group you want the separation index for:")
    comparison_group <- comparison_groups[choice]

    cat("You selected:", names(comparison_group), "-", comparison_group, "\n")
  }
  else
  {
    comparison_group <- comparison_group_code
  }
  # most compatible with other data/studies
  reference_group <- "B03002_003"

  tib_valid <- tib %>% filter(!is.na(GEOID) & !is.na(GEOID_county))
  tib_geos <- tib_valid %>%
    select(GEOID, year)

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()

  reference_tib  <- tibble()
  comparison_tib <- tibble()
  county_ref_tib <- tibble()
  county_comp_tib <- tibble()

  for(i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]
    suppressMessages({
    tot_data_reference <- get_acs(
      geography = "tract",
      variables = c(tract_reference = reference_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      mutate(year = yr)
    reference_tib <- reference_tib %>%
      bind_rows(tot_data_reference) %>%
      distinct()

    tot_data_comparison <- get_acs(
      geography = "tract",
      variables = c(tract_comparison = comparison_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      mutate(year = yr)
    comparison_tib <- comparison_tib %>%
      bind_rows(tot_data_comparison) %>%
      distinct()

    count_reference <- get_acs(
      geography = "county",
      variables = c(county_reference = reference_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      mutate(year = yr)
    county_ref_tib <- county_ref_tib %>%
      bind_rows(count_reference) %>%
      select(GEOID, NAME, estimate, year) %>%
      distinct()

    count_comparison <- get_acs(
      geography = "county",
      variables = c(county_comparison = comparison_group),
      state = st,
      year = yr,
      survey = "acs5"
    ) %>%
      mutate(year = yr)
    county_comp_tib <- county_comp_tib %>%
      bind_rows(count_comparison) %>%
      select(GEOID, NAME, estimate, year) %>%
      distinct()})
  }
  county_comp_tib <- county_comp_tib %>%
    mutate(
      county = case_when(
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
      state = case_when(
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2])))
  county_ref_tib <- county_ref_tib %>%
    mutate(
      county = case_when(
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
      state = case_when(
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2])))

  clean_reference <- tib_geos %>%
    left_join(reference_tib %>%
                select(GEOID, ref_estimate = estimate, year),
              by = c("GEOID", "year")) %>% distinct()
  clean_comparison <- tib_geos %>%
    left_join(comparison_tib %>%
                select(GEOID, comp_estimate = estimate, year),
              by = c("GEOID", "year")) %>% distinct()

  compact <- left_join(clean_reference, clean_comparison, by = c("GEOID", "year"))
  compact <- compact %>%
    mutate(pi = (ref_estimate) / (ref_estimate + comp_estimate))

  modify_tib <- tib %>%
    select(address, GEOID, GEOID_county, state, county, year)

  compact_county <- left_join(county_ref_tib %>%
                                select(GEOID_county = GEOID, ref_county = estimate, county, state, year),
                              county_comp_tib %>%
                                select(GEOID_county = GEOID, comp_county = estimate, county, state, year),
                              by = c("GEOID_county", "county", "state", "year")) %>%
    mutate(county = county %>% str_replace("Municipio", "") %>% str_squish())

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR"),
    state = c(state.name, "District of Columbia", "Puerto Rico"))
  compact_county <- compact_county %>%
    left_join(state_lookup, by = "state")

  modify_tib <- modify_tib %>%
    left_join(compact_county %>%
                select(-state, state = state_abbr), by = c("GEOID_county", "state", "year")) %>% distinct()
  modify_tib <- modify_tib %>%
    left_join(compact, by = c("GEOID", "year")) %>%
    mutate(P = (ref_county) / (ref_county + comp_county)) %>%
    mutate(separation_index = abs(pi - P))
  tib <- tib %>%
    left_join(modify_tib %>%
                select(address, separation_index, year), by = c("address", "year"))

  return(tib)
}

# Decennial Census - Dissimilarity Index ----------------------------------
#' Joins a Column for Residential Segregation: Decennial Dissimilarity
#'
#' @description
#' The function `add_decennial_dissimilarity()` computes the
#' `decennial_dissimilarity_index` (D) for a given dataset containing
#' race/ethnic population counts within subareas (`Census tracts`) of a larger
#' area (`County`). The Dissimilarity Index is a commonly used measure of
#' residential segregation, reflecting the evenness of distribution of two
#' population groups across geographic areas. Accurate decennial year data
#' starts at 2010, so dates early are rounded and all other dates round normally
#' to nearest decennial year. For the decennial years of 2020, a column for
#' their 2010 data is also added. This is because not all of 2020 data is loaded
#' since the decennial year is not over, so to assure proper analysis 2010 can
#' be used to compare with.
#'
#' @details
#' The Dissimilarity Index (D) is calculated using the formula:
#'
#' \deqn{D = \frac{1}{2} \sum_{i=1}^{n} \left| \frac{x_i}{X} - \frac{y_i}{Y} \right|}
#'
#' where:
#' \itemize{
#'   \item \eqn{x_i} is the minority population in subarea
#'   \item \eqn{X} is the total minority population in the region
#'   \item \eqn{y_i} is the reference population in subarea
#'   \item \eqn{Y} is the total reference population in the region
#'   \item \eqn{n} is the number of subareas (tracts) in the region
#' }
#'
#' The index ranges from 0 (complete integration) to 1 (complete segregation).
#' @inheritParams keep_original_year
#' @param minority_group_code_dhc The P5 table code of the comparison group of
#'   interest. If the code isn't known leave null and select from the menu. Used
#'   when dates are 2020 and up. [DHC data](https://www.census.gov/data/tables/2023/dec/2020-census-dhc.html)
#' @param minority_group_code_sf1 The P005 table code of the comparison group of
#'   interest. If the code isn't known leave null and select from the menu. Used
#'   when dates are below 2020. [sf1 data](https://www.census.gov/data/datasets/2010/dec/summary-file-1.html)
#'
#' @returns A tibble with columns `decennial_year` and
#'   `decennial_dissimilarity_index` added to the data of the input tibble
#'
#' @references
#' Source:
#'
#' Recommended data sources include:
#'
#' U.S. Census Bureau decennial Census (1990, 2000, and 2010), [available from] (https://www.census.gov/programs-surveys/decennial-census/data.html).
#' U.S. Census Bureau. (1991). 1990 Census of Population and Housing, Summary Tape File 1, Technical Documentation. [Available from](https://www2.census.gov/programs-surveys/decennial/1990/technical-documentation/complete-tech-docs/summary-files/d1-d90-s100-14-tech.zip).
#' U.S. Census Bureau. (2001). Census 2000, Summary File 1, Technical Documentation,[ Available from](https://www2.census.gov/programs-surveys/decennial/2000/technical-documentation/complete-tech-docs/summary-files/sf1.pdf).
#' U.S. Census Bureau. (2012). 2010 Census Summary File 1, Technical Documentation. [Available from](https://www2.census.gov/programs-surveys/decennial/2010/technical-documentation/complete-tech-docs/summary-file/sf1.pdf).
#' [Census website](https://data.census.gov).
#'
#' Note that several online sources provide Dissimilarity Index scores for selected metropolitan statistical areas, counties, and school districts (and across Census years). See, for example, the American Communities Project and the School Segregation Project at the Brown and Lewis Mumford Center at Albany (https://www.brown.edu/academics/spatial-structures-in-social-sciences/).
#'
#' General References:
#'
#' Iceland, J., & Douzet, F. (2006). Measuring racial and ethnic segregation. Hrodote, 122(3): 25-43.
#' Iceland, J., Weinberg, D. H., & Steinmetz, E. (2002). Racial and ethnic residential segregation in the United States: 1980-2000 (U.S. Census Bureau, Series CENSR‑3). Washington DC: U.S. Government Printing Office.
#' Massey, D. S., & Denton, N. A. (1988). The dimensions of residential segregation. Social Forces,67, 281-315.
#' Morgan, P.M., Murphy, R.F., Willis, R.A., Hubbard, D.W., & Norton, J.M. (1975). Dental health of Louisiana residents based on the ten-state nutrition survey. Public Health Reports, 90(2), 173-178.
#' Morrill, R.L. (1995). Aging in place, age specific migration and natural decrease. The Annals of Regional Science, 29(1), 41-66.
#' Reardon, S. F. (2006). A conceptual framework for measuring segregation and its associations with population outcomes. In J. M. Oakes & J. S. Kaufman (Eds.), Methods in social epidemiology (pp. 169-192). San Francisco, CA: Wiley and Sons/Jossey-Bass.
#' Reardon, S. F., & Firebaugh, G. (2002). Measures of multi-group segregation. Sociological Methodology, 32, 33-67.
#' Reardon, S. F., Matthews, S. A., O’Sullivan, D., Lee, B. A., Firebaugh, G., Farrell, C. R., & Bischoff, K. (2008). The geographic scale of metropolitan racial segregation. Demography,45(3), 489-514.
#' Reardon, S. F., & O’Sullivan, D. (2004). Measures of spatial segregation. Sociological Methodology, 34, 121-162.
#' Sakoda, J.M. (1981). A generalized index of dissimilarity. Demography, 18(2), 245-50.
#' Taeuber, K. E., & Taeuber, A. F. (1965). Negroes in cities: Residential segregation and neighborhood change. Chicago, IL: Aldine.
#' Theil, H. (1972). Statistical decomposition analysis (Vol. 14). Amsterdam, The Netherlands: North-Holland.
#' White, M. J. (1983). The measurement of spatial segregation. American Journal of Sociology,88, 1008-1018.
#' White, M. J. (1986). Segregation and diversity measures in population distribution. Population Index,52, 198-221.
#' Wong, D. S. (1993). Spatial indices of segregation. Urban Studies, 30, 559-572.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_decennial_dissimilarity(test_tib)
#'
#' @importFrom dplyr select filter mutate rename group_by ungroup bind_rows left_join distinct
#' @importFrom stringr str_detect str_trim str_split_fixed
#' @importFrom utils menu
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom tidycensus get_decennial
#'
#' @export
add_decennial_dissimilarity <- function(tib, minority_group_code_dhc = NULL, minority_group_code_sf1 = NULL)
{
  checker_with_county(tib)

  tib_valid <- tib %>%
    select(address, state, county, GEOID, GEOID_county, actual_year) %>%
    filter(!is.na(GEOID) & !is.na(GEOID_county)) %>%
    mutate(decennial_year = case_when(
      actual_year < 2010 ~ 2010,
      (actual_year %% 10) < 5 ~ floor(actual_year / 10) * 10,
      (actual_year %% 10) >= 5 ~ ceiling(actual_year / 10) * 10,
      is.na(actual_year) ~ 2020))

  dhc_results <- tibble()
  sf1_results <- tibble()

  if(any(tib_valid$decennial_year >= 2015))
  {
    if(is.null(minority_group_code_dhc))
    {
      print("Because at least one of the years is counted as 2020, the dhc dataset must be used.")
      minority_groups_dhc <- c(
        "Not Hispanic or Latino" = "P5_002N",
        "Not Hispanic or Latino, White alone" = "P5_003N",
        "Not Hispanic or Latino, Black or African American alone" = "P5_004N",
        "Not Hispanic or Latino, American Indian and Alaska Native alone" = "P5_005N",
        "Not Hispanic or Latino Asian alone" = "P5_006N",
        "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P5_007N",
        "Not Hispanic or Latino Some other race alone" = "P5_008N",
        "Not Hispanic or Latino Two or more races" = "P5_009N",
        "Hispanic or Latino" = "P5_010N",
        "Hispanic or Latino White alone" = "P5_011N",
        "Hispanic or Latino Black or African American alone" = "P5_012N",
        "Hispanic or Latino American Indian and Alaska Native alone" = "P5_013N",
        "Hispanic or Latino Asian alone" = "P5_014N",
        "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P5_015N",
        "Hispanic or Latino Some Other Race alone" = "P5_016N",
        "Hispanic or Latino Two or More Races" = "P5_017N"
      )
      choice <- menu(names(minority_groups_dhc), title = "Select the group you want the dissimilarity index for:")
      minority_group_dhc <- minority_groups_dhc[choice]
      cat("You selected:", names(minority_group_dhc), "-", minority_group_dhc, "\n")
    }
    else
    {
      minority_group_dhc <- minority_group_code_dhc
    }

    reference_group_dhc <- "P5_003N"

    tib_dhc <- tib_valid %>%
      filter(decennial_year >= 2015)

    state_years_dhc <- tib_dhc %>%
      select(state, decennial_year) %>%
      distinct()

    dhc_minority_tract <- tibble()
    dhc_minority_county <- tibble()
    dhc_reference_tract <- tibble()
    dhc_reference_county <- tibble()

    for(i in seq_len(nrow(state_years_dhc)))
    {
      st <- state_years_dhc$state[i]
      yr <- state_years_dhc$decennial_year[i]

      suppressMessages({
      first_dhc <- get_decennial(
        geography = "tract",
        variables = minority_group_dhc,
        year = yr,
        survey = "dhc",
        state = st) %>%
        rename(minority_tract_dhc = variable) %>%
        rename(mt_value_dhc = value) %>%
        mutate(decennial_year = yr)
      dhc_minority_tract <- dhc_minority_tract %>%
        bind_rows(first_dhc)

      second_dhc <- get_decennial(
        geography = "county",
        variables = minority_group_dhc,
        year = yr,
        survey = "dhc",
        state = st) %>%
        rename(minority_county_dhc = variable) %>%
        rename(mc_value_dhc = value) %>%
        mutate(decennial_year = yr)
      dhc_minority_county <- dhc_minority_county %>%
        bind_rows(second_dhc)

      third_dhc <- get_decennial(
        geography = "tract",
        variables = reference_group_dhc,
        year = yr,
        survey = "dhc",
        state = st) %>%
        rename(reference_tract_dhc = variable) %>%
        rename(rt_value_dhc = value) %>%
        mutate(decennial_year = yr)
      dhc_reference_tract <- dhc_reference_tract %>%
        bind_rows(third_dhc)

      fourth_dhc <- get_decennial(
        geography = "county",
        variables = reference_group_dhc,
        year = yr,
        survey = "dhc",
        state = st) %>%
        rename(reference_county_dhc = variable) %>%
        rename(rc_value_dhc = value) %>%
        mutate(decennial_year = yr)
      dhc_reference_county <- dhc_reference_county %>%
        bind_rows(fourth_dhc)})
    }
    state_lookup <- tibble(
      state_abbr = c(state.abb, "DC", "PR"),
      state = c(state.name, "District of Columbia", "Puerto Rico"))

    nums_dhc <- left_join(dhc_minority_tract, dhc_reference_tract, by = c("GEOID", "NAME", "decennial_year")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr)

    denoms_dhc <- left_join(dhc_minority_county, dhc_reference_county, by = c("GEOID", "NAME", "decennial_year")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 1]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 2]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr) %>%
      rename(GEOID_county = GEOID)

    combine_dhc <- left_join(denoms_dhc %>%
                               select(GEOID_county, state, county, decennial_year, mc_value_dhc, rc_value_dhc),
                             nums_dhc %>%
                               select(GEOID, state, county, decennial_year, mt_value_dhc, rt_value_dhc),
                             by = c("state", "county", "decennial_year")) %>% distinct() %>%
      mutate(x_div_dhc = case_when(
        mc_value_dhc == 0 ~ 0,
        TRUE ~ mt_value_dhc / mc_value_dhc
      )) %>%
      mutate(y_div_dhc = case_when (
        rc_value_dhc == 0 ~ 0,
        TRUE ~ rt_value_dhc / rc_value_dhc
      )) %>%
      mutate(difference_dhc = abs(x_div_dhc - y_div_dhc)) %>%
      group_by(GEOID_county, state, county, decennial_year) %>%
      mutate(summation_dhc = sum(difference_dhc)) %>% ungroup() %>%
      mutate(decennial_dissimilarity_index_dhc = summation_dhc * 0.5) %>%
      select(GEOID_county, state, county, decennial_year, decennial_dissimilarity_index_dhc) %>% distinct() %>%
      mutate(county = county %>%
               str_replace("Municipio", "") %>%
               str_squish())

    tib_valid_one <- tib_valid %>% select(-county) %>%
      left_join(combine_dhc, by = c("GEOID_county", "state", "decennial_year")) %>%
      mutate(`compare_2020` = 2010)

    if(is.null(minority_group_code_sf1))
    {
      print("Because not all data for 2020 is uploaded, sf1 (2010) data will be used as an estimate to compare the reliability of 2020s given data.")
      minority_groups_comp <- c(
        "Not Hispanic or Latino" = "P005002",
        "Not Hispanic or Latino, White alone" = "P005003",
        "Not Hispanic or Latino, Black or African American alone" = "P005004",
        "Not Hispanic or Latino, American Indian and Alaska Native alone" = "P005005",
        "Not Hispanic or Latino Asian alone" = "P005006",
        "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005007",
        "Not Hispanic or Latino Some other race alone" = "P005008",
        "Not Hispanic or Latino Two or more races" = "P005009",
        "Hispanic or Latino" = "P005010",
        "Hispanic or Latino White alone" = "P005011",
        "Hispanic or Latino Black or African American alone" = "P005012",
        "Hispanic or Latino American Indian and Alaska Native alone" = "P005013",
        "Hispanic or Latino Asian alone" = "P005014",
        "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005015",
        "Hispanic or Latino Some Other Race alone" = "P005016",
        "Hispanic or Latino Two or More Races" = "P005017"
      )
      choice <- menu(names(minority_groups_comp), title = "Select the group you want the dissimilarity index for:")
      minority_group_comp <- minority_groups_comp[choice]
      cat("You selected:", names(minority_group_comp), "-", minority_group_comp, "\n")
    }
    else
    {
      minority_group_comp <- minority_group_code_sf1
    }

    reference_group_comp <- "P005003"

    state_years_comp <- tib_valid_one %>%
      filter(decennial_year == 2020) %>%
      select(state, compare_2020) %>%
      distinct()

    comp_minority_tract <- tibble()
    comp_minority_county <- tibble()
    comp_reference_tract <- tibble()
    comp_reference_county <- tibble()

    for(i in seq_len(nrow(state_years_comp)))
    {
      st <- state_years_comp$state[i]
      yr <- state_years_comp$compare_2020[i]

      suppressMessages({
      first_compare <- get_decennial(
        geography = "tract",
        variables = minority_group_comp,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(minority_tract_comp = variable) %>%
        rename(mt_value_comp = value) %>%
        mutate(compare_2020 = yr)
      comp_minority_tract <- comp_minority_tract %>%
        bind_rows(first_compare)

      second_compare <- get_decennial(
        geography = "county",
        variables = minority_group_comp,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(minority_county_comp = variable) %>%
        rename(mc_value_comp = value) %>%
        mutate(compare_2020 = yr)
      comp_minority_county <- comp_minority_county %>%
        bind_rows(second_compare)

      third_compare <- get_decennial(
        geography = "tract",
        variables = reference_group_comp,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(reference_tract_comp = variable) %>%
        rename(rt_value_comp = value) %>%
        mutate(compare_2020 = yr)
      comp_reference_tract <- comp_reference_tract %>%
        bind_rows(third_compare)

      fourth_compare <- get_decennial(
        geography = "county",
        variables = reference_group_comp,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(reference_county_comp = variable) %>%
        rename(rc_value_comp = value) %>%
        mutate(compare_2020 = yr)
      comp_reference_county <- comp_reference_county %>%
        bind_rows(fourth_compare)})
    }
    state_lookup <- tibble(
      state_abbr = c(state.abb, "DC", "PR"),
      state = c(state.name, "District of Columbia", "Puerto Rico"))

    nums_comp <- left_join(comp_minority_tract, comp_reference_tract, by = c("GEOID", "NAME", "compare_2020")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr)

    denoms_comp <- left_join(comp_minority_county, comp_reference_county, by = c("GEOID", "NAME", "compare_2020")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 1]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 2]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr) %>%
      rename(GEOID_county = GEOID)

    combine_comp <- left_join(denoms_comp %>%
                                select(GEOID_county, state, county, compare_2020, mc_value_comp, rc_value_comp),
                              nums_comp %>%
                                select(GEOID, state, county, compare_2020, mt_value_comp, rt_value_comp),
                              by = c("state", "county", "compare_2020")) %>% distinct() %>%
      mutate(x_div_comp = case_when(
        mc_value_comp == 0 ~ 0,
        TRUE ~ mt_value_comp / mc_value_comp
      )) %>%
      mutate(y_div_comp = case_when (
        rc_value_comp == 0 ~ 0,
        TRUE ~ rt_value_comp / rc_value_comp
      )) %>%
      mutate(difference_comp = abs(x_div_comp - y_div_comp)) %>%
      group_by(GEOID_county, state, county, compare_2020) %>%
      mutate(summation_comp = sum(difference_comp)) %>% ungroup() %>%
      mutate(decennial_dissimilarity_index_comparison = summation_comp * 0.5) %>%
      select(GEOID_county, state, county, compare_2020, decennial_dissimilarity_index_comparison) %>% distinct() %>%
      mutate(county = county %>%
               str_replace("Municipio", "") %>%
               str_squish())

    tib_valid_one <- tib_valid_one %>% select(-county) %>%
      left_join(combine_comp, by = c("GEOID_county", "state", "compare_2020"))
  }

  if(any(tib_valid$decennial_year < 2015))
  {
    if(is.null(minority_group_code_sf1))
    {
      print("Because at least one of the years is below the 2020 decennial years, the sf1 dataset be used.")
      minority_groups_sf1 <- c(
        "Not Hispanic or Latino" = "P005002",
        "Not Hispanic or Latino, White alone" = "P005003",
        "Not Hispanic or Latino, Black or African American alone" = "P005004",
        "Not Hispanic or Latino, American Indian and Alaska Native alone" = "P005005",
        "Not Hispanic or Latino Asian alone" = "P005006",
        "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005007",
        "Not Hispanic or Latino Some other race alone" = "P005008",
        "Not Hispanic or Latino Two or more races" = "P005009",
        "Hispanic or Latino" = "P005010",
        "Hispanic or Latino White alone" = "P005011",
        "Hispanic or Latino Black or African American alone" = "P005012",
        "Hispanic or Latino American Indian and Alaska Native alone" = "P005013",
        "Hispanic or Latino Asian alone" = "P005014",
        "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005015",
        "Hispanic or Latino Some Other Race alone" = "P005016",
        "Hispanic or Latino Two or More Races" = "P005017"
      )
      choice <- menu(names(minority_groups_sf1), title = "Select the group you want the dissimilarity index for:")
      minority_group_sf1 <- minority_groups_sf1[choice]
      cat("You selected:", names(minority_group_sf1), "-", minority_group_sf1, "\n")
    }
    else
    {
      minority_group_sf1 <- minority_group_code_sf1
    }

    reference_group_sf1 <- "P005003"

    tib_sf1 <- tib_valid %>%
      filter(decennial_year < 2015)

    state_years_sf1 <- tib_sf1 %>%
      select(state, decennial_year) %>%
      distinct()

    sf1_minority_tract <- tibble()
    sf1_minority_county <- tibble()
    sf1_reference_tract <- tibble()
    sf1_reference_county <- tibble()

    for(i in seq_len(nrow(state_years_sf1)))
    {
      st <- state_years_sf1$state[i]
      yr <- state_years_sf1$decennial_year[i]

      suppressMessages({
      first_sf1 <- get_decennial(
        geography = "tract",
        variables = minority_group_sf1,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(minority_tract_sf1 = variable) %>%
        rename(mt_value_sf1 = value) %>%
        mutate(decennial_year = yr)
      sf1_minority_tract <- sf1_minority_tract %>%
        bind_rows(first_sf1)

      second_sf1 <- get_decennial(
        geography = "county",
        variables = minority_group_sf1,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(minority_county_sf1 = variable) %>%
        rename(mc_value_sf1 = value) %>%
        mutate(decennial_year = yr)
      sf1_minority_county <- sf1_minority_county %>%
        bind_rows(second_sf1)

      third_sf1 <- get_decennial(
        geography = "tract",
        variables = reference_group_sf1,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(reference_tract_sf1 = variable) %>%
        rename(rt_value_sf1 = value) %>%
        mutate(decennial_year = yr)
      sf1_reference_tract <- sf1_reference_tract %>%
        bind_rows(third_sf1)

      fourth_sf1 <- get_decennial(
        geography = "county",
        variables = reference_group_sf1,
        year = yr,
        survey = "sf1",
        state = st) %>%
        rename(reference_county_sf1 = variable) %>%
        rename(rc_value_sf1 = value) %>%
        mutate(decennial_year = yr)
      sf1_reference_county <- sf1_reference_county %>%
        bind_rows(fourth_sf1)})
    }
    state_lookup <- tibble(
      state_abbr = c(state.abb, "DC", "PR"),
      state = c(state.name, "District of Columbia", "Puerto Rico"))

    nums_sf1 <- left_join(sf1_minority_tract, sf1_reference_tract, by = c("GEOID", "NAME", "decennial_year")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr) %>%
      mutate(county = county %>%
               str_replace("Municipio", "") %>%
               str_squish())

    denoms_sf1 <- left_join(sf1_minority_county, sf1_reference_county, by = c("GEOID", "NAME", "decennial_year")) %>%
      mutate(county = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 1]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
        state = case_when(
          str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 2)[, 2]),
          str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2]))) %>%
      left_join(state_lookup, by = "state") %>% select(-state) %>% rename(state = state_abbr) %>%
      rename(GEOID_county = GEOID) %>%
      mutate(county = county %>%
               str_replace("Municipio", "") %>%
               str_squish())

    combine_sf1 <- left_join(denoms_sf1 %>%
                               select(GEOID_county, state, county, decennial_year, mc_value_sf1, rc_value_sf1),
                             nums_sf1 %>%
                               select(GEOID, state, county, decennial_year, mt_value_sf1, rt_value_sf1),
                             by = c("state", "county", "decennial_year")) %>% distinct() %>%
      mutate(x_div_sf1 = case_when(
        mc_value_sf1 == 0 ~ 0,
        TRUE ~ mt_value_sf1 / mc_value_sf1
      )) %>%
      mutate(y_div_sf1 = case_when (
        rc_value_sf1 == 0 ~ 0,
        TRUE ~ rt_value_sf1 / rc_value_sf1
      )) %>%
      mutate(difference_sf1 = abs(x_div_sf1 - y_div_sf1)) %>%
      group_by(GEOID_county, state, county, decennial_year) %>%
      mutate(summation_sf1 = sum(difference_sf1)) %>% ungroup() %>%
      mutate(decennial_dissimilarity_index_sf1 = summation_sf1 * 0.5) %>%
      select(GEOID_county, state, county, decennial_year, decennial_dissimilarity_index_sf1) %>% distinct()
  }
    tib_valid <- tib_valid %>% select(-county) %>% left_join(tib_valid_one, by = c("address", "GEOID","GEOID_county", "state", "decennial_year", "actual_year")) %>%
      left_join(combine_sf1, by = c("GEOID_county", "state", "county", "decennial_year"))
  if("decennial_dissimilarity_index_dhc" %in% names(tib_valid) & "decennial_dissimilarity_index_sf1" %in% names(tib_valid))
  {
    tib_valid <- tib_valid %>%
      mutate(decennial_dissimilarity_index = coalesce(decennial_dissimilarity_index_dhc, decennial_dissimilarity_index_sf1)) %>%
      select(-decennial_dissimilarity_index_dhc, -decennial_dissimilarity_index_sf1) %>% distinct()
  }
  else if("decennial_dissimilarity_index_dhc" %in% names(tib_valid))
  {
    tib_valid <- tib_valid %>%
      mutate(decennial_dissimilarity_index = decennial_dissimilarity_index_dhc) %>%
      select(-decennial_dissimilarity_index_dhc) %>% distinct()
  }
  else
  {
    tib_valid <- tib_valid %>%
      mutate(decennial_dissimilarity_index = decennial_dissimilarity_index_sf1) %>%
      select(-decennial_dissimilarity_index_sf1) %>% distinct()
  }
  tib <- tib %>% select(-county) %>%
    left_join(tib_valid %>% select(-state, -GEOID, -GEOID_county, actual_year), by = c("address", "actual_year"))

  return(tib)
}
# Concentrated Poverty ----------------------------------------------------
#' Joins a Column for Concentrated Poverty
#'
#' @description
#' The `add_concentrated_poverty()` function obtains the protocol Concentrated
#' Poverty Index based on data extracted from the U.S. Census Bureau’s American
#' Community Survey (ACS) 5-year estimates. Concentrated poverty describes the
#' proportion of individuals below the poverty level living in **high-poverty**
#' neighborhoods, typically defined as those with poverty rates above 40%
#' (**PhenX uses 35%**). [Data.census.gov](https://data.census.gov/table/ACSST5Y2019.S1701?q=S1701).
#'
#' @details
#' Table S1701 - Poverty Status in the past 12 months
#' - Calculation:
#' - Total individuals below poverty = sum of `S1701_C02_046E` across all areas
#' - Total individuals below poverty in high-poverty areas = sum of `S1701_C02_046E` in areas where `S1701_C03_046E` > 35%
#' - Total individuals below poverty in high-poverty areas / Total individuals below poverty
#'
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with column `concentrated_poverty` added to the data of
#'   the input tibble
#'
#' @references
#' Source:
#'
#' U.S. Census Bureau. (2018). Poverty status in the past 12 months (table S1701), 2009-2018 American Community Survey 5-year estimates. [Retrieved from](https://data.census.gov/cedsci/table?q=table%20S1701&hidePreview=false&tid=ACSST5Y2018.S1701&vintage=2018)
#' U.S. Census Bureau. (2018). Ratio of income to poverty level in the past 12 months (table C17002), 2009-2018 American Community Survey 5-year estimates. [Retrieved from](https://data.census.gov/cedsci/table?q=C17002&hidePreview=false&tid=ACSDT5Y2018.C17002&vintage=2018)
#'
#' General Reference:
#'
#' Jargowsky, P. A. (1997). Poverty and place: Ghettos, barrios, and the American city. Russell Sage Foundation.
#' Jargowsky, P. A. (2003). Stunning progress, hidden problems: The dramatic decline of concentrated poverty in the 1990s (Living Cities Census Series). Brookings Institution Center on Urban and Metropolitan Policy.
#' Jargowsky, P. A. (2013). Concentration of poverty in the new millennium: Changes in the prevalence, composition, and location of high-poverty neighborhoods. The Century Foundation and Rutgers Center for Urban Research and Education.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_concentrated_poverty(test_tib)
#'
#' @importFrom dplyr filter mutate select distinct left_join bind_rows case_when group_by ungroup summarise
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_trim str_split_fixed
#' @importFrom tidycensus get_acs
#' @importFrom magrittr %>%
#'
#' @export
add_concentrated_poverty <- function(tib)
{
  checker_with_county(tib)

  tib_valid <- tib %>%
    filter(!is.na(tib$GEOID)) %>% select(address, state, county, year, GEOID, GEOID_county)

  blw_data <- tibble()
  blw_high <- tibble()
  pct_data <- tibble()

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()

  for(i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]
    if(yr > 2014)
    {
      suppressMessages({
      below_data <- get_acs(geography = "county",
                            variables = c(below_pov_lvl = "S1701_C02_046"),
                            tables = "S1701",
                            year = yr,
                            state = st,
                            survey = "acs5") %>%
        mutate(year = yr)
      blw_data <- blw_data %>% bind_rows(below_data)

      below_tract <- get_acs(geography = "tract",
                             variables = c(below_in_high = "S1701_C02_046"),
                             tables = "S1701",
                             year = yr,
                             state = st,
                             survey = "acs5") %>%
        mutate(year = yr)
      blw_high <- blw_high %>% bind_rows(below_tract)

      percent_below_data <- get_acs(geography = "tract",
                                    variables = c(pct_below = "S1701_C03_046"),
                                    tables = "S1701",
                                    year = yr,
                                    state = st,
                                    survey = "acs5") %>%
        mutate(year = yr)})
      pct_data <- pct_data %>% bind_rows(percent_below_data)
    }
    else
    {
      suppressMessages({
      below_data <- get_acs(geography = "county",
                            variables = c(below_pov_lvl = "S1701_C02_039"),
                            tables = "S1701",
                            year = yr,
                            state = st,
                            survey = "acs5") %>%
        mutate(year = yr)
      blw_data <- blw_data %>% bind_rows(below_data)

      below_tract <- get_acs(geography = "tract",
                             variables = c(below_in_high = "S1701_C02_039"),
                             tables = "S1701",
                             year = yr,
                             state = st,
                             survey = "acs5") %>%
        mutate(year = yr)
      blw_high <- blw_high %>% bind_rows(below_tract)

      percent_below_data <- get_acs(geography = "tract",
                                    variables = c(pct_below = "S1701_C03_039"),
                                    tables = "S1701",
                                    year = yr,
                                    state = st,
                                    survey = "acs5") %>%
        mutate(year = yr)})
      pct_data <- pct_data %>% bind_rows(percent_below_data)
    }
  }
  blw_data <- blw_data %>%
    mutate(county = case_when(
      str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 1])),
      state = case_when(
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 2)[, 2])))
  pct_data <- pct_data %>%
    mutate(county = case_when(
      str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
      str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
      state = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3])))

  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR"),
    state = c(state.name, "District of Columbia", "Puerto Rico"))
  pct_data <- pct_data %>% left_join(state_lookup, by = "state")
  blw_data <- blw_data %>% left_join(state_lookup, by = "state")

  high_pov_area <- pct_data %>%
    filter(estimate > 35) %>%
    group_by(county, state, year) %>%
    mutate(count_of_tracts = n()) %>% ungroup() %>%
    select(GEOID, county, state = state_abbr, year, count_of_tracts) %>% distinct() %>%
    left_join(blw_high %>%
                select(GEOID, year, count_of_people = estimate), by = c("GEOID", "year")) %>%
    select(-GEOID) %>% distinct() %>%
    group_by(county, state, year) %>%
    mutate(tot_in_high = sum(count_of_people)) %>% select(-count_of_tracts, -count_of_people) %>% distinct() %>%
    left_join(blw_data %>% select(county, state = state_abbr, year, tot_blw = estimate), by = c("county", "state", "year")) %>%
    mutate(concentrated_poverty = tot_in_high / tot_blw)

  tib_valid <- tib_valid %>%
    left_join(high_pov_area %>%
                select(county, state, year, concentrated_poverty), by = c("county", "state", "year")) %>%
    mutate(concentrated_poverty = ifelse(is.na(concentrated_poverty), 0, concentrated_poverty)) %>% distinct()
  tib <- tib %>%
    left_join(tib_valid %>%
                select(address, year, concentrated_poverty), by = c("address", "year"))

  message("0 is not an indication of no poverty, the 0 is from there being no high poverty tract in the county at that time(>35%)")
  return(tib)
}

# AQI ---------------------------------------------------------------------
#' Joins a Column for Air Quality Index(AQI)
#'
#' @description
#'
#' The `add_AQI()` function appends annual Air Quality Index (AQI) data from the
#' EPA county based annual dataset. It takes each county via the
#' `tidygeocoder` package `reverse_geocode()`, identifies the air quality index
#' corresponding to the EPA's county data, retrieves pollutant-specific
#' data for the specified year, and classifies each observation into a corresponding
#' health risk category as well as identifying the main pollutant. [Data from](https://www.epa.gov/outdoor-air-quality-data).
#'
#' @details
#' #' The AQI is a standardized value (ranging from 0 to 500) that reflects the level of air pollution and the potential health implications. It is calculated for key criteria pollutants:
#' \itemize{
#'   \item Fine Particulate Matter (PM2.5, code = 88101)
#'   \item Ozone (O3, code = 44201)
#'   \item Carbon Monoxide (CO, code = 42101)
#'   \item Nitrogen Dioxide (NO2, code = 42602)
#'   \item Sulfur Dioxide (SO2, code = 42401)
#' }
#' For each pollutant, the function retrieves the annual arithmetic mean concentration and converts it into an AQI value using standard EPA breakpoints. The overall AQI for the address-year combination is defined as the **highest** pollutant-specific AQI, representing the "main pollutant" and worst-case air quality exposure.
#'
#' EPA AQI values are mapped to health-based categories as follows:
#' \describe{
#'   \item{0–50}{Good — Air quality is satisfactory, and air pollution poses little or no risk.}
#'   \item{51–100}{Moderate — Air quality is acceptable; some pollutants may be a concern for a very small number of individuals.}
#'   \item{101–150}{Unhealthy for Sensitive Groups — People with respiratory or heart conditions, children, and older adults may be affected.}
#'   \item{151–200}{Unhealthy — Everyone may begin to experience health effects; sensitive groups may experience more serious effects.}
#'   \item{201–300}{Very Unhealthy — Health alert: everyone may experience more serious health effects.}
#'   \item{301–500}{Hazardous — Health warnings of emergency conditions. The entire population is more likely to be affected.}
#' }
#'
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with columns `AQI` and `category_AQI` added to the data of
#'   the input tibble
#'
#' @references
#' Source:
#'
#' U.S. Environmental Protection Agency. (n.d.). AirData. [Retrieved from](https://www.epa.gov/outdoor-air-quality-data/about-air-data-reports) #aqi
#'
#' General References:
#'
#' AirNow. (2019). Air Quality Index (AQI) basics. Retrieved from https://airnow.gov/index.cfm?action=aqibasics.aqi
#' Kumari, S., & Jain, M. K. (2018). A critical review on Air Quality Index. In V. Singh, S. Yadav, & R. Yadava (Eds.), Environmental pollution. Water Science and Technology Library (vol. 77). Springer.
#' Rice, M. B., Ljungman, P. L., Wilker, E. H., Gold, D. R., Schwartz, J. D., Koutrakis, P., … Mittleman, M. A. (2013). Short-term exposure to air pollution and lung function in the Framingham Heart Study. American Journal of Respiratory and Critical Care Medicine, 188(11), 1351–1357.
#' Talbot, T. O., Haley, V. B., Dimmick, W. F., Paulu, C., Talbott, E. O., & Rager, J. (2009). Developing consistent data and methods to measure the public health impacts of ambient air quality for Environmental Public Health Tracking: Progress to date and future directions. Air Quality, Atmosphere, & Health, 2(4), 199–206.
#' Thach, T. Q., Tsang, H., Cao, P., & Ho, L. M. (2018). A novel method to construct an air quality index based on air pollution profiles. International Journal of Hygiene and Environmental Health, 221(1), 17–26.
#' Wellenius, G. A., Burger, M. R., Coull, B. A., Schwartz, J., Suh, H. H., Koutrakis, P., … Mittleman, M. A. (2012). Ambient air pollution and the risk of acute ischemic stroke. Archives of Internal Medicine, 172(3), 229–234.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_AQI(test_tib)
#'
#' @importFrom dplyr filter mutate select bind_rows left_join group_by ungroup distinct arrange slice slice_max
#' @importFrom sf st_as_sf st_distance st_drop_geometry
#' @importFrom tidygeocoder geocode
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @export
add_AQI <- function(tib)
{
  tib <- tib %>%
    mutate(AQI_year = case_when(
      actual_year > 2024 ~ 2024,
      actual_year < 1980 ~ 1980,
      is.na(actual_year) ~ 2024,
      TRUE ~ actual_year),
      county = case_when(
        str_detect(county, regex("\\bborough\\b", ignore_case = TRUE)) ~
          str_replace(county, regex("\\bborough\\b", ignore_case = TRUE), "County"),
        !str_detect(county, regex("\\bcounty\\b", ignore_case = TRUE)) ~
          paste(county, "County"),
        TRUE ~ county))

  AQI_twenty_four <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twenty_four.csv")
  AQI_twenty_three <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twenty_three.csv")
  AQI_twenty_two <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twenty_two.csv")
  AQI_twenty_one <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twenty_one.csv")
  AQI_twenty <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twenty.csv")
  AQI_nineteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_nineteen.csv")
  AQI_eighteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eighteen.csv")
  AQI_seventeen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_seventeen.csv")
  AQI_sixteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_sixteen.csv")
  AQI_fifteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_fifteen.csv")
  AQI_fourteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_fourteen.csv")
  AQI_thirteen <- read.csv("geodeterminants_datasets/clean_AQI/AQI_thirteen.csv")
  AQI_twelve <- read.csv("geodeterminants_datasets/clean_AQI/AQI_twelve.csv")
  AQI_eleven <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eleven.csv")
  AQI_ten <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ten.csv")
  AQI_nine <- read.csv("geodeterminants_datasets/clean_AQI/AQI_nine.csv")
  AQI_eight <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eight.csv")
  AQI_seven <- read.csv("geodeterminants_datasets/clean_AQI/AQI_seven.csv")
  AQI_six <- read.csv("geodeterminants_datasets/clean_AQI/AQI_six.csv")
  AQI_five <- read.csv("geodeterminants_datasets/clean_AQI/AQI_five.csv")
  AQI_four <- read.csv("geodeterminants_datasets/clean_AQI/AQI_four.csv")
  AQI_three <- read.csv("geodeterminants_datasets/clean_AQI/AQI_three.csv")
  AQI_two <- read.csv("geodeterminants_datasets/clean_AQI/AQI_two.csv")
  AQI_one <- read.csv("geodeterminants_datasets/clean_AQI/AQI_one.csv")
  AQI_double_zero <- read.csv("geodeterminants_datasets/clean_AQI/AQI_double_zero.csv")
  AQI_ninety_nine <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetynine.csv")
  AQI_ninety_eight <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetyeight.csv")
  AQI_ninety_seven <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetyseven.csv")
  AQI_ninety_six <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetysix.csv")
  AQI_ninety_five <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetyfive.csv")
  AQI_ninety_four <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetyfour.csv")
  AQI_ninety_three <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetythree.csv")
  AQI_ninety_two <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetytwo.csv")
  AQI_ninety_one <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninetyone.csv")
  AQI_ninety <- read.csv("geodeterminants_datasets/clean_AQI/AQI_ninety.csv")
  AQI_eighty_nine <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightynine.csv")
  AQI_eighty_eight <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightyeight.csv")
  AQI_eighty_seven <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightyseven.csv")
  AQI_eighty_six <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightysix.csv")
  AQI_eighty_five <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightyfive.csv")
  AQI_eighty_four <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightyfour.csv")
  AQI_eighty_three <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightythree.csv")
  AQI_eighty_two <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightytwo.csv")
  AQI_eighty_one <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eightyone.csv")
  AQI_eighty <- read.csv("geodeterminants_datasets/clean_AQI/AQI_eighty.csv")

  binded_data <- tibble()
  binded_data <- bind_rows(AQI_twenty_four) %>%
    bind_rows(AQI_twenty_three) %>%
    bind_rows(AQI_twenty_two) %>%
    bind_rows(AQI_twenty_one) %>%
    bind_rows(AQI_twenty) %>%
    bind_rows(AQI_nineteen) %>%
    bind_rows(AQI_eighteen) %>%
    bind_rows(AQI_seventeen) %>%
    bind_rows(AQI_sixteen) %>%
    bind_rows(AQI_fifteen) %>%
    bind_rows(AQI_fourteen) %>%
    bind_rows(AQI_thirteen) %>%
    bind_rows(AQI_twelve) %>%
    bind_rows(AQI_eleven) %>%
    bind_rows(AQI_ten) %>%
    bind_rows(AQI_nine) %>%
    bind_rows(AQI_eight) %>%
    bind_rows(AQI_seven) %>%
    bind_rows(AQI_six) %>%
    bind_rows(AQI_five) %>%
    bind_rows(AQI_four) %>%
    bind_rows(AQI_three) %>%
    bind_rows(AQI_two) %>%
    bind_rows(AQI_one) %>%
    bind_rows(AQI_double_zero) %>%
    bind_rows(AQI_ninety_nine) %>%
    bind_rows(AQI_ninety_eight) %>%
    bind_rows(AQI_ninety_seven) %>%
    bind_rows(AQI_ninety_six) %>%
    bind_rows(AQI_ninety_five) %>%
    bind_rows(AQI_ninety_four) %>%
    bind_rows(AQI_ninety_three) %>%
    bind_rows(AQI_ninety_two) %>%
    bind_rows(AQI_ninety_one) %>%
    bind_rows(AQI_ninety) %>%
    bind_rows(AQI_eighty_nine) %>%
    bind_rows(AQI_eighty_eight) %>%
    bind_rows(AQI_eighty_seven) %>%
    bind_rows(AQI_eighty_six) %>%
    bind_rows(AQI_eighty_five) %>%
    bind_rows(AQI_eighty_four) %>%
    bind_rows(AQI_eighty_three) %>%
    bind_rows(AQI_eighty_two) %>%
    bind_rows(AQI_eighty_one) %>%
    bind_rows(AQI_eighty)

  binded_data <- binded_data %>%
    mutate(county = str_squish(county))

  results <- tib %>%
    left_join(binded_data, by = c("county", "state", "AQI_year" = "actual_year"))

  return(results)
}

# Residential Concentrations of Income ------------------------------------
#' Joins a Column for Residential Concentrations of Income (ICE)
#'
#' @description
#' The `add_ICE()` function computes the Index of Concentration(ICE) for census
#' tracts based on U.S. Census Bureau American Community Survey (ACS) 5-year
#' estimates. ICE measures the extent to which an area’s residents are
#' concentrated into groups at the extremes of deprivation and privilege. Values
#' range from **-1** (*100% in most deprived*) to **1** (*100% in most
#' privileged*). Data is collected from [Table B19001](https://data.census.gov/table/ACSDT5Y2021.B19001).
#'
#' @details The ICE is calculated using ACS variables representing income
#' distribution within census tracts. Specifically, it uses income percentiles
#' to identify the proportion of affluent and poor residents in each tract and
#' computes ICE as **(A_i - P_i) / T_i**, where **A_i** is the number of
#' affluent persons,
#' **P_i** is the number of poor persons, and **T_i** is the total population with known income.
#'
#' This metric can be used to assess socioeconomic polarization in neighborhoods
#' or larger areas, aiding research on social determinants of health and
#' neighborhood effects.
#'
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with column `ICE` added to the data of the input tibble
#'
#' @references
#' Source:
#'
#' U.S. Census Bureau ACS data products (5-year estimates). [source](https://data.census.gov/cedsci/).
#'   Data.census.gov website. [source](https://data.census.gov/).
#'   Massey, D. S. (2001). The prodigal paradigm returns: ecology comes back to sociology. In: A. Booth & A. Crouter (Eds.), Does it take a village? community effects on children, adolescents, and families (pp. 41-48). Lawrence Erlbaum Associates.
#'
#' General References:
#'
#' Chambers, B. D., Baer, R. J., McLemore, M. R., & Jelliffe-Pawlowski, L. L. (2019). Using Index of Concentration at the Extremes as indicators of structural racism to evaluate the association with preterm birth and infant mortality-California, 2011-2012. Journal of Urban Health, 96(2), 159-170. [source](https://doi.org/10.1007/s11524-018-0272-4).
#' Feldman, J. M., Waterman, P. D., Coull, B. A., & Krieger, N. (2015). Spatial social polarization: using the Index of Concentration at the Extremes jointly for income and race/ethnicity to analyse risk of hypertension. Journal of Epidemiology and Community Health, 69(12), 1199-1207. [source](https://doi.org/10.1136/jech-2015-205728).
#' Krieger, N., Waterman, P. D., Gryparis, A., & Coull, B. A. (2015). Black carbon exposure, socioeconomic and racial/ethnic spatial polarization, and the Index of Concentration at the Extremes (ICE). Health & Place, 34, 215-228. [source](https://doi.org/10.1016/j.healthplace.2015.05.008).
#' Krieger, N., Kim, R., Feldman, J., & Waterman, P. D. (2018). Using the Index of Concentration at the Extremes at multiple geographical levels to monitor health inequities in an era of growing spatial social polarization: Massachusetts, USA (2010-14). International Journal of Epidemiology, 47(3), 788–819. [source](https://doi.org/10.1093/ije/dyy004).
#' Krieger, N., Waterman, P. D., Spasojevic, J., Li, W., Maduro, G., & Van Wye, G. (2016). Public health monitoring of privilege and deprivation with the Index of Concentration at the Extremes. American Journal of Public Health, 106(2), 256-263. [source](https://doi.org/10.2105/AJPH.2015.302955).
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_ICE(test_tib)
#'
#' @importFrom dplyr filter select distinct rowwise mutate ungroup left_join bind_rows c_across all_of
#' @importFrom tibble tibble
#' @importFrom tidycensus get_acs
#' @importFrom magrittr %>%
#'
#' @export
add_ICE <- function(tib)
{
  checker(tib)

  tib_valid <- tib %>%
    filter(!is.na(GEOID))

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()

  vars <- paste0("B19001_", sprintf("%03d", 2:17))

  ice_results <- tibble()

  for(i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]

    suppressMessages({
    total_tracts <- get_acs(
      geography = "tract",
      variables = c("B19001_001", vars),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)})

    ice_data <- total_tracts %>%
      rowwise() %>%
      mutate(counts = list(c_across(all_of(paste0(vars, "E"))))) %>%
      mutate(total = B19001_001E) %>%
      mutate(cum_props = list(cumsum(counts) / total)) %>%
      mutate(p20_idx = which(cum_props >= 0.20)[1]) %>%
      mutate(p80_idx = which(cum_props >= 0.80)[1]) %>%
      mutate(P_i = if (is.na(p20_idx)) NA_real_ else sum(counts[1:p20_idx], na.rm = TRUE)) %>%
      mutate(A_i = if (is.na(p80_idx)) NA_real_ else sum(counts[p80_idx:length(counts)], na.rm = TRUE)) %>%
      mutate(ICE = (A_i - P_i) / total) %>%
      ungroup() %>%
      select(GEOID, year, ICE)
    ice_results <- bind_rows(ice_results, ice_data)
  }
  tib <- tib %>%
    left_join(ice_results, by = c("GEOID", "year"))

  return(tib)
}

# Social Vulnerability Index ----------------------------------------------
#' Gets County and State from NAME
#'
#' The `county_state_helper()` function is a helper function for getting the
#' `county` and `state` columns from the `NAME` column.
#'
#' @param data_tib A tibble of ACS data that needs `county` and `state` columns for future joins
#'
#' @returns A tibble of given ACS data, but with added `county` and `state` columns
#'
#' @examples
#' per_capita <- tibble()
#' (Loops through all addresses)
#' {
#'   per_cap <- get_acs(
#'   geography = "tract",
#'   variables = "B19301_001",
#'   state = st,
#'   year = yr,
#'   survey = "acs5",
#'   output = "wide") %>% mutate(year = yr)
#' }
#' per_capita <- per_capita %>% bind_rows(per_cap)
#'
#' county_state_helper(per_capita)
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect str_split_fixed str_trim
#' @importFrom magrittr %>%
#'
#' @export
county_state_helper <- function(data_tib)
{
  data_tib <- data_tib %>%
    mutate(county = case_when(
      str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 2]),
      str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 2])),
      state = case_when(
        str_detect(NAME, ";") ~ str_trim(str_split_fixed(NAME, ";", 3)[, 3]),
        str_detect(NAME, ",") ~ str_trim(str_split_fixed(NAME, ",", 3)[, 3])))
  return(data_tib)
}
#' Gets state abbreviations
#'
#' The function `look_up_helper()` gets the **abbreviation** of state names as it is
#' more compatible for joining the data found
#'
#' @param data_tib A tibble of ACS data that has a state column
#'
#' @returns A tibble of ACS data with the state column changed to **state abbreviations** *instead* of **full names**
#'
#' @examples
#' look_up_helper(any tibble with a state column)
#'
#' @importFrom tibble tibble
#' @importFrom dplyr left_join select rename
#' @importFrom magrittr %>%
#'
#' @export
look_up_helper <- function(data_tib)
{
  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR"),
    state = c(state.name, "District of Columbia", "Puerto Rico"))
  data_tib <- data_tib %>%
    left_join(state_lookup, by = "state") %>% select(-state) %>%
    rename(state = state_abbr)
  return(data_tib)
}
#' Joins a Column for Social Vulnerability(SVI)
#'
#' @description
#' The `add_SVI()` function joins and run calculations with ACS data to a
#' user-supplied tibble using `GEOID` and `year` as matching keys. The SVI
#' measures the relative vulnerability of populations in U.S. Census tracts
#' based on 15 social factors grouped into 4 themes. There are two methods
#' ranking and flagging. Ranking gives a percentile rank compared to all other
#' tracts in that same state (0-1), and flagging gives the score of factors that
#' are greater than or equal to the 90th percentile for that state (0-15).
#' Flagging is broader and quicker, whereas ranking is more in-depth. Both are
#' provided with this function.
#'
#' @details
#' Tables used:
#' - [S1701](https://data.census.gov/table/ACSST5Y2019.S1701?q=S1701) ~ Population below poverty
#' - [S2301](https://data.census.gov/table/ACSST1Y2021.S2301?g=040XX00US53_050XX00US53057_010XX00US&text=Table+s2301) ~ Unemployment count
#' - [B19301](https://data.census.gov/table/ACSDT1Y2021.B19301?q=per+capita+income) ~ Per capita
#' - [S1501](https://data.census.gov/table/ACSST1Y2022.S1501) ~ No Highschool diploma(25+)
#' - [S0101](https://data.census.gov/table/ACSST1Y2022.S0101) ~ 65 years old +
#' - [S0101](https://data.census.gov/table/ACSST1Y2022.S0101) ~ Under 18 years old
#' - [S1810](https://data.census.gov/table/ACSST1Y2021.S1810) ~ With disability
#' - [B11003](https://data.census.gov/table/ACSDT1Y2022.B11003?t=Family+Size+and+Type&g=040XX00US72&mode=results) ~ Single parent
#' - [B03002](https://data.census.gov/table/ACSDT1Y2022.B03002) ~ Minority data
#' - [C16001](https://data.census.gov/table/ACSDT1Y2021.C16001) ~ English less than well
#' - [B25024](https://data.census.gov/table/ACSDT5Y2020.B25024?q=B25024&g=500XX00US4829&table=B25024&tid=ACSDT5Y2020.B25024) ~ Multi-unit housing(10+)
#' - [B25024](https://data.census.gov/table/ACSDT5Y2020.B25024?q=B25024&g=500XX00US4829&table=B25024&tid=ACSDT5Y2020.B25024) ~ Mobile housing
#' - [B25014](https://data.census.gov/table/ACSDT5Y2021.B25014?q=B25014&g=1400000US48157672202) ~ Crowding
#' - [B25044](https://data.census.gov/table/ACSDT1Y2022.B25044) ~ No vehicle
#' - [B26001](https://data.census.gov/table/ACSDT1Y2023.B26001) ~ Group quarters
#'
#' 4 themes and the 15 factors:
#' - Factors of socioeconomic status: Below poverty, unemployment, income, no high school diploma(25+)
#' - Factors of household composition and disability: 65 years old +, 17 years old -, civilians with disabilities, single-parent household
#' - Factors of minority status and language: Minority, speaks English less than well
#' - Factors of housing and transportation: Multi-unit structures, mobile homes, crowding, no vehicle, group quarters
#'
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with columns `SVI_rank_meth` and `SVI_flag_meth` added to
#'   data of the input tibble
#'
#' @references
#' Source:
#'
#' Centers for Disease Control and Prevention (CDC). (2016, May 19). Social Vulnerability Index (SVI). [Retrieved from](https://svi.cdc.gov/data-and-tools-download.html).
#'
#' General References:
#'
#' Agency for Toxic Substances and Disease Registry. (2018, September). CDC’s Social Vulnerability Index (fact sheet). [Retrieved from](https://svi.cdc.gov/factsheet.html).
#' An, R., & Xiang, X. (2015). Social vulnerability and leisure-time physical inactivity among US adults. American Journal of Health Behavior, 39(6), 751–760.
#' Flanagan, B. E., Gregory, E. W., Hallisey, E. J., Heitgerd, J. L., & Lewis, B. (2011). A Social Vulnerability Index for disaster management. Journal of Homeland Security and Emergency Management, 8(1). doi: 10.2202/1547-7355.1792
#' Flanagan, B., & Hallisey, E. (2013). Social Vulnerability Index and Toolkit (PDF document). Presented at the New York State Department of Health and the Albany School of Public Health Annual GIS Day. [Retrieved from](https://svi.cdc.gov/publications.html).
#' Flanagan, B. E., Hallisey, E. J., Adams, E., Lavery, A. (2018). Measuring community vulnerability to natural and anthropogenic hazards: The Centers for Disease Control and Prevention’s Social Vulnerability Index. Journal of Environmental Health, 80(10), 34–36.
#' Gay, J. L., Robb, S. W., Benson, K. M., & White, A. (2016). Can the Social Vulnerability Index be used for more than emergency preparedness? An examination using youth physical fitness data. Journal of Physical Activity and Health, 13, 121–130.
#' Yee, C. W., Cunningham, S. D., & Ickovics, J. R. (2019). Application of the Social Vulnerability Index for identifying teen pregnancy intervention need in the United States. Maternal and Child Health Journal, 23, 1516–1524.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_SVI(test_tib)
#'
#' @importFrom dplyr filter select distinct mutate group_by ungroup left_join rowwise case_when c_across
#' @importFrom tibble tibble
#' @importFrom tidycensus get_acs
#' @importFrom stats quantile
#' @importFrom magrittr %>%
#'
#' @export
add_SVI <- function(tib)
{
  checker_with_county(tib)

  tib_valid <- tib %>%
    filter(!is.na(GEOID)) %>%
    select(address, GEOID, county, state, year)

  state_years <- tib_valid %>%
    select(state, year) %>%
    distinct()
  # socioecon status
  below_pov_data <- tibble()
  unemployment_data <- tibble()
  per_capita <- tibble()
  no_hs_dip <- tibble()
  # household composition/disability
  sixtyfive_up <- tibble()
  seventeen_down <- tibble()
  with_disability <- tibble()
  single_parent <- tibble()
  # minority status & language
  minority_data <- tibble()
  english_ltw <- tibble()
  # housing type & transportation
  multi_unit <- tibble()
  mobile_homes <- tibble()
  crowding_data <- tibble()
  no_vehicle <- tibble()
  group_quarters <- tibble()

  for(i in seq_len(nrow(state_years)))
  {
    st <- state_years$state[i]
    yr <- state_years$year[i]

    suppressMessages({
    pov <- get_acs(
      geography = "tract",
      variables = "S1701_C03_001",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    below_pov_data <- below_pov_data %>% bind_rows(pov) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_pov = quantile(S1701_C03_001E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(S1701_C03_001E < ninety_percentile_pov, 0, 1))

    unemploy <- get_acs(
      geography = "tract",
      variables = "S2301_C04_001",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    unemployment_data <- unemployment_data %>% bind_rows(unemploy) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_umemploy = quantile(S2301_C04_001E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(S2301_C04_001E < ninety_percentile_umemploy, 0, 1))

    per_cap <- get_acs(
      geography = "tract",
      variables = "B19301_001",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    per_capita <- per_capita %>% bind_rows(per_cap) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_percap = quantile(B19301_001E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(B19301_001E < ninety_percentile_percap, 0, 1))})

    if(yr < 2015)
    {
      suppressMessages({
      no_dip <- get_acs(
        geography = "tract",
        variables = c("S1501_C01_008", "S1501_C01_007", "S1501_C01_006"),
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      no_hs_dip <- no_hs_dip %>% bind_rows(no_dip) %>%
        county_state_helper() %>%
        mutate(percent = (S1501_C01_008E + S1501_C01_007E) / S1501_C01_006E) %>%
        group_by(state, year) %>%
        mutate(ninety_percentile_nodip_a = quantile(percent, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_a = ifelse(percent < ninety_percentile_nodip_a, 0, 1))})
    }
    else
    {
      suppressMessages({
      no_dip <- get_acs(
        geography = "tract",
        variables = "S1501_C02_008",
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      no_hs_dip <- no_hs_dip %>% bind_rows(no_dip) %>%
        county_state_helper() %>% group_by(state, year) %>%
        mutate(ninety_percentile_nodip_b = quantile(S1501_C02_008E, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_b = ifelse(S1501_C02_008E < ninety_percentile_nodip_b, 0, 1))})
    }

    if(yr > 2016)
    {
      suppressMessages({
      older_group <- get_acs(
        geography = "tract",
        variables = "S0101_C01_030",
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      sixtyfive_up <- sixtyfive_up %>% bind_rows(older_group) %>%
        county_state_helper() %>% group_by(state, year) %>%
        mutate(ninety_percentile_oga = quantile(S0101_C01_030E, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_a = ifelse(S0101_C01_030E < ninety_percentile_oga, 0, 1))})
    }
    else
    {
      suppressMessages({
      older_group <- get_acs(
        geography = "tract",
        variables = "S0101_C01_028",
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      sixtyfive_up <- sixtyfive_up %>% bind_rows(older_group) %>%
        county_state_helper() %>% group_by(state, year) %>%
        mutate(ninety_percentile_ogb = quantile(S0101_C01_028E, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_b = ifelse(S0101_C01_028E < ninety_percentile_ogb, 0, 1))})
    }

    if(yr > 2016)
    {
      suppressMessages({
      younger_group <- get_acs(
        geography = "tract",
        variables = "S0101_C01_022",
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      seventeen_down <- seventeen_down %>% bind_rows(younger_group) %>%
        county_state_helper() %>% group_by(state, year) %>%
        mutate(ninety_percentile_yga = quantile(S0101_C01_022E, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_a = ifelse(S0101_C01_022E < ninety_percentile_yga, 0, 1))})
    }
    else
    {
      suppressMessages({
      younger_group <- get_acs(
        geography = "tract",
        variables = c("S0101_C01_002", "S0101_C01_020", "S0101_C01_021"),
        state = st,
        year = yr,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      seventeen_down <- seventeen_down %>% bind_rows(younger_group) %>%
        county_state_helper() %>% mutate(total = S0101_C01_002E + S0101_C01_020E + S0101_C01_021E) %>%
        group_by(state, year) %>%
        mutate(ninety_percentile_ygb = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_b = ifelse(total < ninety_percentile_ygb, 0, 1))})
    }
    suppressMessages({
    dis <- get_acs(
      geography = "tract",
      variables = "S1810_C02_001",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    with_disability <- with_disability %>% bind_rows(dis) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_dis = quantile(S1810_C02_001E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(S1810_C02_001E < ninety_percentile_dis, 0, 1))

    sing_par <- get_acs(
      geography = "tract",
      variables = c("B11003_010", "B11003_016"),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    single_parent <- single_parent %>% bind_rows(sing_par) %>%
      county_state_helper() %>% mutate(total = B11003_010E + B11003_016E) %>%
      group_by(state, year) %>%
      mutate(ninety_percentile_singpar = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(total < ninety_percentile_singpar, 0, 1))

    min <- get_acs(
      geography = "tract",
      variables = c("B03002_001", "B03002_003"),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    minority_data <- minority_data %>% bind_rows(min) %>%
      county_state_helper() %>% mutate(total = B03002_001E - B03002_003E) %>%
      group_by(state, year) %>%
      mutate(ninety_percentile_min = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(total < ninety_percentile_min, 0, 1))})

    if(yr <= 2016)
    {
      warning("Dates before 2016 are rounded up to 2016 data because in years prior there is no data for spoken language proficiency.")
      suppressMessages({
      lang <- get_acs(
        geography = "tract",
        variables = c(g15 = "C16001_005", g18 = "C16001_008", g111 = "C16001_011", g114 ="C16001_014", g117 = "C16001_017", g120 = "C16001_020", g123 = "C16001_023", g126 = "C16001_026", g129 = "C16001_029", g132 = "C16001_032", g135 = "C16001_035", g138 = "C16001_038"),
        state = st,
        year = 2016,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      english_ltw <- english_ltw %>% bind_rows(lang) %>%
        county_state_helper() %>% mutate(total_a = g15E + g18E + g111E + g114E + g117E + g120E + g123E + g126E + g129E + g132E + g135E + g138E) %>%
        group_by(state, year) %>%
        mutate(ninety_percentile_langa = quantile(total_a, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_a = ifelse(total_a < ninety_percentile_langa, 0, 1))})
    }
    else
    {
      suppressMessages({
      lang <- get_acs(
        geography = "tract",
        variables = c(g25 = "C16001_005", g28 = "C16001_008", g211 = "C16001_011", g214 ="C16001_014", g217 = "C16001_017", g220 = "C16001_020", g223 = "C16001_023", g226 = "C16001_026", g229 = "C16001_029", g232 = "C16001_032", g235 = "C16001_035", g238 = "C16001_038"),
        state = st,
        year = 2016,
        survey = "acs5",
        output = "wide") %>% mutate(year = yr)
      english_ltw <- english_ltw %>% bind_rows(lang) %>%
        county_state_helper() %>% mutate(total_b = g25E + g28E + g211E + g214E + g217E + g220E + g223E + g226E + g229E + g232E + g235E + g238E) %>%
        group_by(state, year) %>%
        mutate(ninety_percentile_langb = quantile(total_b, probs = 0.9, na.rm = TRUE)) %>%
        mutate(flag_b = ifelse(total_b < ninety_percentile_langb, 0, 1))})
    }

    suppressMessages({
    multi <- get_acs(
      geography = "tract",
      variables = c("B25024_007", "B25024_008", "B25024_009"),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    multi_unit <- multi_unit %>% bind_rows(multi) %>%
      county_state_helper() %>% mutate(total = B25024_007E + B25024_008E + B25024_009E) %>%
      group_by(state, year) %>%
      mutate(ninety_percentile_multi = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(total < ninety_percentile_multi, 0, 1))

    mobile <- get_acs(
      geography = "tract",
      variables = "B25024_010",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    mobile_homes <- mobile_homes %>% bind_rows(mobile) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_mob = quantile(B25024_010E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(B25024_010E < ninety_percentile_mob, 0, 1))

    crowding <- get_acs(
      geography = "tract",
      variables = c("B25014_006", "B25014_007", "B25014_012", "B25014_013"),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    crowding_data <- crowding_data %>% bind_rows(crowding) %>%
      county_state_helper() %>% mutate(total = B25014_006E + B25014_007E + B25014_012E + B25014_013E) %>%
      group_by(state, year) %>%
      mutate(ninety_percentile_crow = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(total < ninety_percentile_crow, 0, 1))

    veh_access <- get_acs(
      geography = "tract",
      variables = c("B25044_003", "B25044_010"),
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    no_vehicle <- no_vehicle %>% bind_rows(veh_access) %>%
      county_state_helper() %>% mutate(total = B25044_003E + B25044_010E) %>%
      group_by(state, year) %>%
      mutate(ninety_percentile_veh = quantile(total, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(total < ninety_percentile_veh, 0, 1))

    grp_quart <- get_acs(
      geography = "tract",
      variables = "B26001_001",
      state = st,
      year = yr,
      survey = "acs5",
      output = "wide") %>% mutate(year = yr)
    group_quarters <- group_quarters %>% bind_rows(grp_quart) %>%
      county_state_helper() %>% group_by(state, year) %>%
      mutate(ninety_percentile_gq = quantile(B26001_001E, probs = 0.9, na.rm = TRUE)) %>%
      mutate(flag = ifelse(B26001_001E < ninety_percentile_gq, 0, 1))})
  }
  below_pov_data <- below_pov_data %>%
    mutate(rank_bp = (rank(S1701_C03_001E, ties.method = "min") - 1) / (length(S1701_C03_001E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_bp = flag, rank_bp)

  unemployment_data <- unemployment_data %>%
    mutate(rank_unemploy = (rank(S2301_C04_001E, ties.method = "min") - 1) / (length(S2301_C04_001E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_unemploy = flag, rank_unemploy)

  per_capita <- per_capita %>%
    mutate(rank_percap = (rank(B19301_001E, ties.method = "min") - 1) / (length(B19301_001E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_percap = flag, rank_percap)

  if(!"flag_a" %in% names(no_hs_dip))
  {
    no_hs_dip$flag_a <- NA
  }
  if(!"flag_b" %in% names(no_hs_dip))
  {
    no_hs_dip$flag_b <- NA
  }
  no_hs_dip <- no_hs_dip %>%
    mutate(rank_dip = ifelse(
      year < 2015,
      (rank(percent, ties.method = "min") - 1) / (length(percent) - 1),
      (rank(S1501_C02_008E, ties.method = "min") - 1) / (length(S1501_C02_008E) - 1))) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_erldip = flag_a, flag_latdip = flag_b, rank_dip)

  if(!"flag_a" %in% names(sixtyfive_up))
  {
    sixtyfive_up$flag_a <- NA
  }
  if(!"flag_b" %in% names(sixtyfive_up))
  {
    sixtyfive_up$flag_b <- NA
  }
  sixtyfive_up <- sixtyfive_up %>%
    mutate(rank_sfu = ifelse(
      year > 2016,
      (rank(S0101_C01_030E, ties.method = "min") - 1) / (length(S0101_C01_030E) - 1),
      (rank(S0101_C01_028E, ties.method = "min") - 1) / (length(S0101_C01_028E) - 1))) %>% ungroup() %>%
    look_up_helper() %>%
    select(GEOID, state, county, year, flag_latsfu = flag_a, flag_erlsfu = flag_b, rank_sfu)

  if(!"flag_a" %in% names(seventeen_down))
  {
    seventeen_down$flag_a <- NA
  }
  if(!"flag_b" %in% names(seventeen_down))
  {
    seventeen_down$flag_b <- NA
  }
  seventeen_down <- seventeen_down %>%
    mutate(rank_yg = ifelse(
      year > 2016,
      (rank(S0101_C01_022E, ties.method = "min") - 1) / (length(S0101_C01_022E) - 1),
      (rank(total, ties.method = "min") - 1) / (length(total) - 1))) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_latyg = flag_a, flag_erlyg = flag_b, rank_yg)

  with_disability <- with_disability %>%
    mutate(rank_dis = (rank(S1810_C02_001E, ties.method = "min") - 1) / (length(S1810_C02_001E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_dis = flag, rank_dis)

  single_parent <- single_parent %>%
    mutate(rank_sp = (rank(total, ties.method = "min") - 1) / (length(total) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_sp = flag, rank_sp)

  minority_data <- minority_data %>%
    mutate(rank_min = (rank(total, ties.method = "min") - 1) / (length(total) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_min = flag, rank_min)

  if(!"flag_a" %in% names(english_ltw))
  {
    english_ltw$flag_a <- NA
  }
  if(!"flag_b" %in% names(english_ltw))
  {
    english_ltw$flag_b <- NA
  }
  english_ltw <- english_ltw %>%
    mutate(rank_eng = ifelse(
      year <= 2016,
      (rank(total_a, ties.method = "min") - 1) / (length(total_a) - 1),
      (rank(total_b, ties.method = "min") - 1) / (length(total_b) - 1))) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_def = flag_a, flag_any = flag_b, rank_eng)

  multi_unit <- multi_unit %>%
    mutate(rank_mult = (rank(total, ties.method = "min") - 1) / (length(total) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_mult = flag, rank_mult)

  mobile_homes <- mobile_homes %>%
    mutate(rank_mob = (rank(B25024_010E, ties.method = "min") - 1) / (length(B25024_010E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_mob = flag, rank_mob)

  crowding_data <- crowding_data %>%
    mutate(rank_crow = (rank(total, ties.method = "min") - 1) / (length(total) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_crow = flag, rank_crow)

  no_vehicle <- no_vehicle %>%
    mutate(rank_veh = (rank(total, ties.method = "min") - 1) / (length(total) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_veh = flag, rank_veh)

  group_quarters <- group_quarters %>%
    mutate(rank_gq = (rank(B26001_001E, ties.method = "min") - 1) / (length(B26001_001E) - 1)) %>% ungroup() %>%
    look_up_helper() %>% select(GEOID, state, county, year, flag_gq = flag, rank_gq)

  joint <- tibble()
  joint <- below_pov_data %>%
    left_join(unemployment_data, by = c("GEOID", "state", "county", "year")) %>%
    left_join(per_capita, by = c("GEOID", "state", "county", "year")) %>%
    left_join(no_hs_dip, by = c("GEOID", "state", "county", "year")) %>%
    left_join(sixtyfive_up, by = c("GEOID", "state", "county", "year")) %>%
    left_join(seventeen_down, by = c("GEOID", "state", "county", "year")) %>%
    left_join(with_disability, by = c("GEOID", "state", "county", "year")) %>%
    left_join(single_parent, by = c("GEOID", "state", "county", "year")) %>%
    left_join(minority_data, by = c("GEOID", "state", "county", "year")) %>%
    left_join(english_ltw, by = c("GEOID", "state", "county", "year")) %>%
    left_join(multi_unit, by = c("GEOID", "state", "county", "year")) %>%
    left_join(mobile_homes, by = c("GEOID", "state", "county", "year")) %>%
    left_join(crowding_data, by = c("GEOID", "state", "county", "year")) %>%
    left_join(no_vehicle, by = c("GEOID", "state", "county", "year")) %>%
    left_join(group_quarters, by = c("GEOID", "state", "county", "year"))

  percentile <- joint %>%
    rowwise() %>% mutate(total_tract = sum(c_across(c(rank_bp, rank_unemploy, rank_percap, rank_dip, rank_sfu, rank_yg, rank_dis, rank_sp, rank_min, rank_eng, rank_mult, rank_mob, rank_crow, rank_veh, rank_gq)), na.rm = TRUE)) %>%
    ungroup() %>% group_by(state, year) %>%
    mutate(SVI_rank_meth = (rank(total_tract, ties.method = "min") - 1) / (length(total_tract) - 1)) %>% ungroup() %>%
    select(GEOID, county, state, year, SVI_rank_meth)

  flag <- joint %>%
    select(-starts_with("rank")) %>% rowwise() %>%
    mutate(SVI_flag_meth = sum(c_across(-c(GEOID, state, county, year)), na.rm = TRUE)) %>% ungroup() %>%
    select(GEOID, county, state, year, SVI_flag_meth)

  tib <- tib %>%
    left_join(percentile %>% select(-county), by = c("GEOID", "state", "year")) %>%
    left_join(flag %>% select(-county), by = c("GEOID", "state", "year"))

  return(tib)
}

# Food swamp --------------------------------------------------------------
#' Joins a Column for Food Swamp: Retail Food Environment Index
#'
#' @description
#' The `add_RFEI()` function computes the Retail Food Environment Index (RFEI)
#' at the county level for each participant in a provided tibble, using store
#' and restaurant data from the [USDA Food Environment
#' Atlas](https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads).
#' The index is calculated as the ratio of fast food and convenience stores to
#' grocery stores and supermarkets. Data is matched by year (**2011** or
#' **2016**), state, and cleaned county name. All values that will cause issues
#' due to NA are taken care of. The number indicates the times greater of
#' unhealthy food access. A higher RFEI has been correlated with a higher
#' obesity rate compared to a lower RFEI.
#'
#' @details
#' \deqn{RFEI = \frac{FFR + CONVS}{GROC + SUPERC}}
#' where:
#' \itemize{
#'   \item \code{FFR} = Number of fast food restaurants (limited-service restaurants)
#'   \item \code{CONVS} = Number of convenience stores
#'   \item \code{GROC} = Number of grocery stores
#'   \item \code{SUPERC} = Number of supermarkets
#'   }
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with column `RFEI` added to the data of the input tibble
#'
#' @references
#' Source:
#'
#' Cooksey-Stowers, K., Schwartz, M. B., & Brownell, K. D. (2017). Food swamps predict obesity rates better than food deserts in the United States. International Journal of Environmental Research and Public Health, 14(11), 1366.
#'
#' General References:
#'
#' California Center for Public Health Advocacy, PolicyLink, & the UCLA Center for Health Policy Research. (2008, April). Designed for disease: The link between local food environments and obesity and diabetes. [Retrieved from](https://escholarship.org/uc/item/9zc7p54b).
#' Cooksey-Stowers, K., Schwartz, M. B., & Brownell, K. D. (2017). Food swamps predict obesity rates better than food deserts in the United States. International Journal of Environmental Research and Public Health, 14(11), 1366.
#' Economic Research Service (ERS), U.S. Department of Agriculture (USDA). (2019). Data access and documentation downloads. [Retrieved from](https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/).
#' Economic Research Service (ERS), U.S. Department of Agriculture (USDA). (2019). Food Environment Atlas. [Retrieved from](https://www.ers.usda.gov/data-products/food-environment-atlas/go-to-the-atlas/)
#' McGuirt, J. T., Jilcott Pitts, S. B., & Gustafson, A. (2018). Association between spatial access to food outlets, frequency of grocery shopping, and objectively-assessed and self-reported fruit and vegetable consumption. Nutrients, 10(12), 1974.
#' Murphy, M., Badland, H., Jordan, H., Koohsari, M. J., & Giles-Corti, B. (2018). Local food environments, suburban development, and BMI: A mixed methods study. International Journal of Environmental Research and Public Health, 15(7), 1392.
#' Lucan, S. C., Maroko, A. R., Seitchik, J. L., Yoon, D. H., Sperry, L. E. & Schechter, C. B. (2018). Unexpected neighborhood sources of food and drink: Implications for research and community health. American Journal of Preventive Medicine, 55(2), e29–e38.
#' Spence, J. C., Cutumisu, N., Edwards, J., Raine, K. D., & Smoyer-Tomic, K. (2009). Relation between local food environments and obesity among adults. BMC Public Health, 9, 192. doi: 10.1186/1471-2458-9-192
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_RFEI(test_tib)
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate filter case_when left_join bind_rows across
#' @importFrom stringr str_replace
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#'
#' @export
add_RFEI <- function(tib)
{
  atlas_stores <- read_excel("geodeterminants_datasets/FoodEnvironmentAtlas.xls", sheet = "STORES")
  atlas_restaurants <- read_excel("geodeterminants_datasets/FoodEnvironmentAtlas.xls", sheet = "RESTAURANTS")

  clean_stores <- atlas_stores %>%
    select(state = State, county_clean = County, GROC11, GROC16, SUPERC11, SUPERC16, CONVS11, CONVS16)
  clean_restaurants <- atlas_restaurants %>%
    select(state = State, county_clean = County, FFR11, FFR16)

  twenty_eleven_stores <- clean_stores %>%
    select(state, county_clean, GROC = GROC11, SUPERC = SUPERC11, CONVS = CONVS11)
  twenty_sixteen_stores <- clean_stores %>%
    select(state, county_clean, GROC = GROC16, SUPERC = SUPERC16, CONVS = CONVS16)

  twenty_eleven_restaurants <- clean_restaurants %>%
    select(state, county_clean, FFR = FFR11)
  twenty_sixteen_restaurants <- clean_restaurants %>%
    select(state, county_clean, FFR = FFR16)

  tib_clean <- tib %>%
    select(address, state, county, actual_year) %>%
    mutate(atlas_year = case_when(
      actual_year <= 2013 ~ 2011,
      actual_year > 2013 ~ 2016,
      TRUE ~ 2016)) %>%
    mutate(county_clean = str_replace(county, " County$", "")) %>%
    filter(!is.na(county) & !is.na(state))

  results <- tibble()

  for(i in seq_len(nrow(tib_clean)))
  {
    if(tib_clean$atlas_year[i] == 2011)
    {
      join_twenty_eleven <- left_join(tib_clean[i,], twenty_eleven_stores, by = c("state", "county_clean")) %>%
        left_join(twenty_eleven_restaurants, by = c("state" , "county_clean"))
      results <- results %>%
        bind_rows(join_twenty_eleven)
    }
    else
    {
      join_twenty_sixteen <- left_join(tib_clean[i,], twenty_sixteen_stores, by = c("state", "county_clean")) %>%
        left_join(twenty_sixteen_restaurants, by = c("state" , "county_clean"))
      results <- results %>%
        bind_rows(join_twenty_sixteen)
    }
  }
  results <- results %>%
    mutate(num = rowSums(across(c(FFR, CONVS)), na.rm = TRUE)) %>%
    mutate(denom = rowSums(across(c(GROC, SUPERC)), na.rm = TRUE)) %>%
    mutate(RFEI = case_when(
      denom == 0 ~ NA,
      TRUE ~ num / denom)) %>% distinct()
  tib <- tib %>%
    left_join(results %>%
                select(address, actual_year, atlas_year, RFEI), by = c("address", "actual_year"))
  return(tib)
}

# Environmental Justice ---------------------------------------------------
#' Joins a Column for Environmental Justice Indices
#'
#' @description ⚠️ **Note:** Data is retrieved from [archived
#'   data](https://zenodo.org/records/14767363).The US EPA EJSCREEN tool and
#'   data was discontinued around May 2024 and remains unavailable. Some `GEOID`
#'   values will not have matches in data, possibly due to discontinuation or
#'   missing information for that year.
#'
#' The `add_EJ_index()` function appends Environmental Justice (EJ) index data
#' from the U.S. EPA's EJSCREEN tool to a user-provided tibble, joining by
#' `GEOID` and `actual_year` columns. It selects and merges environmental
#' indicators based on whether `GEOID` exists and is available based on the
#' EJSCREEN year (2016–2024).
#'
#' Environmental indicators added:
#' - National Scale Air Toxics Assessment Diesel PM (diesal_pm)
#' - Particulate Matter (PM2.5) (particulate_matter)
#' - Ozone (ozone)
#' - Lead Paint Indicator (lead_paint)
#' - Traffic Proximity and Volume (traffic_prox_vol)
#' - Proximity to National Priorities List Sites (prox_to_NPLS)
#' - Proximity to Risk Management Plan Sites (prox_to_RMPS)
#' - Proximity to Treatment Storage and Disposal Facilities (prox_to_TSDF)
#' - Proximity to Major Direct Water Dischargers (prox_to_MDWD)
#' - NATA Air Toxics Cancer Risk (NATA_cancer)
#' - NATA Respiratory Hazard Index (NATA_resp_haz)
#'
#' If a year is outside this range or missing, the function defaults to the closest available year.
#'
#' @inheritParams keep_original_year
#'
#' @returns A tibble with columns `diesal_pm`, `particulate_matter`, `ozone`,
#'   `lead_paint`, `traffic_prox_vol`, `prox_to_NPLS`, `prox_to_RMPS`,
#'   `prox_to_TSDF`, `prox_to_MDWD`, `NATA_cancer`, and `NATA_resp_haz` added to
#'   the data of the input tibble
#'
#' @references
#' Source:
#'
#' U.S. Environmental Protection Agency (EPA). (2018). EJSCREEN: Environmental Justice Screening and Mapping Tool. [Retrieved from](https://www.epa.gov/ejscreen).
#' Demographic information that is obtained from the U.S. Census Bureau’s American Community Survey (ACS). The 2018 version of EJSCREEN includes 2012–2016 ACS 5-year summary file data, which are based on 2014 census boundaries.
#'
#' General References:
#'
#' Cifuentes, P., Reichard, J., Im, W., Smith, S., Colen, C., Giurgescu, C., … Hood, D. B. (2019). Application of the Public Health Exposome Framework to estimate phenotypes of resilience in a model Ohio African-American women's cohort. Journal of Urban Health, 96(Suppl 1), 57–71.
#' Driver, A., Mehdizadeh, C., Bara-Garcia, S., Bodenreider, C., Lewis, J., & Wilson, S. (2019). Utilization of the Maryland Environmental Justice Screening Tool: A Bladensburg, Maryland case study. International Journal of Environmental Research and Public Health, 16(3).
#' Rowangould, D., Rowangould, G., Craft, E., & Niemeier, D. (2018). Validating and refining EPA's traffic exposure screening measure. International Journal of Environmental Research and Public Health, 16(1).
#' U.S. Environmental Protection Agency (EPA). (2019). Technical Documentation for EJSCREEN. [Retrieved from](https://www.epa.gov/ejscreen/technical-documentation-ejscreen).
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_EJ_index(test_tib)
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select filter case_when left_join across
#' @importFrom magrittr %>%
#'
#' @export
add_EJ_index <- function(tib)
{
  clean_twenty_four <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2024_clean.csv")
  clean_twenty_three <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2023_clean.csv")
  clean_twenty_two <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2022_clean.csv")
  clean_twenty_one <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2021_clean.csv")
  clean_twenty <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2020_clean.csv")
  clean_nineteen <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2019_clean.csv")
  clean_eighteen <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2018_clean.csv")
  clean_seventeen <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2017_clean.csv")
  clean_sixteen <- read.csv("geodeterminants_datasets/EJ_index_data_clean/EJSCREEN_2016_clean.csv")

  clean_tib <- tib %>%
    select(GEOID, actual_year) %>%
    filter(!is.na(GEOID)) %>%
    mutate(EJ_year = case_when(
      actual_year < 2016 ~ 2016,
      actual_year > 2024 ~ 2024,
      is.na(actual_year) ~ 2024,
      TRUE ~ actual_year)) %>%
    mutate(particulate_matter = NA,
           ozone = NA,
           diesal_pm = NA,
           traffic_prox_vol = NA,
           lead_paint = NA,
           prox_to_NPLS = NA,
           prox_to_RMPS = NA,
           prox_to_TSDF = NA,
           prox_to_MDWD = NA,
           NATA_cancer = NA,
           NATA_resp_haz = NA)

  rows <- seq_len(nrow(clean_tib))

  for(i in rows)
  {
    if(clean_tib$EJ_year[i] == 2024)
    {
      this_data  <- clean_twenty_four %>%
        filter(clean_twenty_four$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2023)
    {
      this_data  <- clean_twenty_three %>%
        filter(clean_twenty_three$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2022)
    {
      this_data  <- clean_twenty_two %>%
        filter(clean_twenty_two$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2021)
    {
      this_data  <- clean_twenty_one %>%
        filter(clean_twenty_one$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2020)
    {
      this_data  <- clean_twenty %>%
        filter(clean_twenty$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2019)
    {
      this_data  <- clean_nineteen %>%
        filter(clean_nineteen$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2018)
    {
      this_data  <- clean_eighteen %>%
        filter(clean_eighteen$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else if(clean_tib$EJ_year[i] == 2017)
    {
      this_data  <- clean_seventeen %>%
        filter(clean_seventeen$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
    else
    {
      this_data  <- clean_sixteen %>%
        filter(clean_sixteen$GEOID == clean_tib$GEOID[i])
      if(nrow(this_data) > 0)
      {
        clean_tib[i, "particulate_matter"] = this_data[,"particulate_matter"]
        clean_tib[i, "ozone"] = this_data[,"ozone"]
        clean_tib[i, "diesal_pm"] = this_data[,"diesal_pm"]
        clean_tib[i, "traffic_prox_vol"] = this_data[,"traffic_prox_vol"]
        clean_tib[i, "lead_paint"] = this_data[,"lead_paint"]
        clean_tib[i, "prox_to_NPLS"] = this_data[,"prox_to_NPLS"]
        clean_tib[i, "prox_to_RMPS"] = this_data[,"prox_to_RMPS"]
        clean_tib[i, "prox_to_TSDF"] = this_data[,"prox_to_TSDF"]
        clean_tib[i, "prox_to_MDWD"] = this_data[,"prox_to_MDWD"]
        clean_tib[i, "NATA_cancer"] = this_data[,"NATA_cancer"]
        clean_tib[i, "NATA_resp_haz"] = this_data[,"NATA_resp_haz"]
      }
    }
  }
  clean_tib <- clean_tib %>% distinct()

  tib <- tib %>%
    left_join(clean_tib, by = c("GEOID", "actual_year"))

  return(tib)
}

# Minimum Wage ------------------------------------------------------------
#' Cleans US Minimum Wage data
#'
#' The function `clean_min_wage_data()` takes excel data that is based on [U.S.
#' Department of Labor
#' records](https://www.dol.gov/agencies/whd/state/minimum-wage/history) and
#' cleans it. The data is cleaned by filling in data for states that follow the
#' federal minimum wage, neglecting minimum wages that pertain to a very small
#' population, and deals with ranges of minimum wages. The method of deciding on
#' minimum wage was taking the over estimate because weighted average is not
#' possible without time stamps and in most states the higher minimum wage was
#' the majority of the population. This function also finds the dichotomous and
#' continuous measures for the minimum wage protocol.
#'
#' @param fed_min_wage The current years minimum wage (e.g. current year: 2025, $7.25)
#'
#' @returns A dataset that contains minimum wage data from 1968 to current year
#'   with columns that contain individual years and states along with the
#'   dichotomous and continuous measures of minimum wage.
#'
#' @examples
#' clean_min_wage_data(7.25)
#' DO NOT LEAVE PARAMETER EMPTY UNLESS YOU KNOW THE DEFAULT IS THE SAME AS THE CURRENT YEAR
#' *default = $7.25
#'
#' @importFrom dplyr mutate if_else filter rename select left_join arrange group_by ungroup bind_rows
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tibble
#'
#' @export
clean_min_wage_data <- function(fed_min_wage = 7.25)
{
  consolidated <- read.csv("geodeterminants_datasets/min_wage/consol_mw.csv", check.names = FALSE)
  change <- read.csv("geodeterminants_datasets/min_wage/chg_in_mw.csv", check.names = FALSE)

  consolidated <- consolidated %>%
    mutate(above_federal = if_else(minimum_wage > fed_min_wage, 1, 0))
  df_2025_long <- consolidated %>%
    mutate(
      year = 2025,
      mw = minimum_wage,
      federal_mw = fed_min_wage) %>%
    select(year, mw, federal_mw, above_federal, state)

  mw_long <- change %>%
    pivot_longer(
      cols = -state,
      names_to = "year",
      values_to = "mw"
    ) %>%
    mutate(year = as.integer(year))
  federal_mw <- mw_long %>%
    filter(state == "Federal (FLSA)") %>%
    rename(federal_mw = mw) %>%
    select(-state)
  mw_with_fed <- mw_long %>%
    filter(state != "Federal (FLSA)") %>%
    left_join(federal_mw, by = "year")
  state_lookup <- tibble(
    state_abbr = c(state.abb, "DC", "PR", "GU", "VI"),
    state = c(state.name, "District of Columbia", "Puerto Rico", "Guam", "U.S. Virgin Islands"))
  mw_with_fed <- mw_with_fed %>%
    left_join(state_lookup, by = "state") %>%
    select(-state) %>%
    rename(state = state_abbr)
  mw_with_binary <- mw_with_fed %>%
    mutate(above_federal = if_else(mw > federal_mw, 1, 0)) %>%
    bind_rows(df_2025_long)

  mw_with_cumulative <- mw_with_binary %>%
    arrange(state, year) %>%
    group_by(state) %>%
    mutate(ever_above_federal_so_far = cummax(above_federal)) %>%
    ungroup()

  return(mw_with_cumulative)
}

#' Joins a Column for Minimum Wage
#'
#' @description
#' The function `add_min_wage()`joins state minimum wage data to a dataset
#' containing state and year variables. It merges in whether the state's minimum
#' wage was *above* the **federal minimum wage** in a *given year*, whether it **ever** had
#' a *higher* minimum wage, an **interaction** of the two(multiply), and the **difference** between
#' the *state* and *federal* minimum wage. These variables are constructed for use
#' in analyses of minimum wage policies.
#'
#' *NOTE: The years are modified as `mw_year` to account for missing years in the records(if needed)
#'
#' @inheritParams keep_original_year
#' @param current_fed_min_wage The current years minimum wage (e.g. current year: 2025, $7.25)
#'
#' @returns A tibble with columns `mw_year`, `above_federal`, `ever_above`, `dichot_inter_mw`, and `mw_above_fed` added to the data of the input tibble
#'
#' @references
#' Source:
#'
#' Merrill-Francis, M., Vernick, J. S., McGinty, E. E., & Pollack Porter, K. M. (2022). Association between fatal occupational injuries and state minimum-wage laws, 2003-2017. American Journal of Preventive Medicine, 62(6), 878-884. [source](https://doi.org/10.1016/j.amepre.2021.09.022).
#' Wage and Hour Division. (2022, January). State Minimum Wage Laws. U.S. Department of Labor, Wage and Hour Division. [source](https://www.dol.gov/agencies/whd/minimum-wage/state).
#'
#' General References:
#'
#' Avila, C. J., & Frakt, A. B. (2021). Raising the minimum wage and public health. JAMA Health Forum, 2(1), e201587. [source](https://doi.org/10.1001/jamahealthforum.2020.1587).
#' Buszkiewicz, J. H., Hill, H. D., & Otten, J. J. (2021). Association of state minimum wage rates and health in working-age adults using the National Health Interview Survey. American Journal of Epidemiology, 190(1), 21-30. [source](https://doi.org/10.1093/aje/kwaa018).
#' Kuehn, David. (2014). The importance of study design in the minimum-wage debate (Issue Brief #384). Economic Policy Institute. [source](https://www.epi.org/publication/importance-study-design-minimum-wage-debate/).
#' Leigh, J. Paul, & Du, Juan. (2018, October 4). Effects of minimum wages on population health. Health Policy Brief. Health Affairs. [source](https://doi.org/10.1377/hpb20180622.107025).
#' Neumark, D., & Wascher, W. (2006). Minimum wages and employment: A review of evidence from the new minimum wage research. Working Papers 060708, University of California-Irvine, Department of Economics, revised Jan 2007. [source](https://ideas.repec.org/p/irv/wpaper/060708.html).
#' Paul Leigh, J., Leigh, W. A., & Du, J. (2019). Minimum wages and public health: A literature review. Preventive Medicine, 118(122-134). [source](https://doi.org/10.1016/j.ypmed.2018.10.005).
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' clean_min_wage_data(7.25)
#'
#' add_min_wage(test_tib)
#'
#' @importFrom dplyr select distinct mutate case_when bind_rows filter left_join
#' @importFrom tibble tibble
#'
#' @export
add_min_wage <- function(tib, current_fed_min_wage = 7.25)
{
  mw_data <- clean_min_wage_data(current_fed_min_wage)

  state_year_pair <- tib %>%
    select(state, actual_year) %>%
    distinct() %>%
    mutate(wage_years = case_when(
      actual_year < 1968 ~ 1968,
      actual_year == 1969 ~ 1968,
      actual_year == 1971 ~ 1970,
      actual_year == 1973 ~ 1972,
      actual_year == 1974 ~ 1976,
      actual_year == 1977 ~ 1976,
      actual_year == 1978 ~ 1979,
      actual_year == 1982 ~ 1981,
      actual_year == 1983 ~ 1981,
      actual_year == 1984 ~ 1981,
      actual_year == 1985 ~ 1988,
      actual_year == 1986 ~ 1988,
      actual_year == 1987 ~ 1988,
      actual_year == 1989 ~ 1988,
      actual_year == 1990 ~ 1991,
      actual_year == 1993 ~ 1992,
      actual_year == 1995 ~ 1994,
      actual_year == 1999 ~ 1998,
      is.na(actual_year) ~ 2025,
      TRUE ~ actual_year))

  required_data <- tibble()

  for(i in seq_len(nrow(state_year_pair)))
  {
    yr = state_year_pair$wage_years[i]
    st = state_year_pair$state[i]
    org_yr = state_year_pair$actual_year[i]

    required_data <- required_data %>%
      bind_rows(mw_data %>%
                  filter(state == st, year == yr) %>%
                  mutate(actual_year = org_yr))
  }
  required_data <- required_data %>%
    mutate(dichot_inter_mw = above_federal * ever_above_federal_so_far) %>%
    mutate(mw_above_fed = mw - federal_mw)
  tib <- tib %>%
    left_join(required_data %>%
                select(state, actual_year, mw_year = year, above_federal, ever_above = ever_above_federal_so_far, dichot_inter_mw, mw_above_fed),
              by = c("state", "actual_year"))
  return(tib)
}

# Unionized ---------------------------------------------------------------
#' Joins a Column for Percent Unionized
#'
#' This function adds percent unionized data for non-agricultural workers
#' to a tibble, using state and year-based union affiliation data from
#' the [Current Population Survey (CPS)](https://www.bls.gov/cps/cpslutabs.htm).
#'
#' The data is sourced from the Bureau of Labor Statistics, based on union
#' membership(`mem_unions`) and representation(`rep_unions`) estimates. Data includes the percentage of
#' employed wage and salary workers who are members of or represented by unions.
#'
#' @inheritParams keep_original_year
#' @param current_year The current year
#' @param default_year The year that NA values should be set to if greater than
#'   current year or NA by the given input tibble
#'
#' @returns A tibble with columns `union_year`, `mem_unions`, and `rep_unions` added to the data of the input tibble
#'
#' @references
#' Source:
#'
#' Bureau of Labor Statistics. (2007). Labor force statistics from the Current Population Survey. Retrieved May 22, 2019, from [source](https://www.bls.gov/cps/cpslutabs.htm)
#'
#' General References:
#'
#' Halpern-Manners, A., & Warren, J. R. (2012). Panel conditioning in longitudinal studies: Evidence from labor force items in the Current Population Survey. Demography, 49(4), 1499–1519. doi: 10.1007/s13524-012-0124-x
#' Hirsch, B. T., & Macpherson, D. A. (n.d.). Union membership and earnings data book: Compilations from the Current Population Survey. Washington, DC: Bureau of National Affairs. Updated annually to the present.
#' Hirsch, B. T., & Macpherson, D. A. (2003). Union membership and coverage database from the Current Population Survey: Note. Industrial and Labor Relations Review, 56(2), 349–554. Updated annually at [source](www.unionstats.com).
#' Milkman, R., & Luce, S. (2017). Labor unions and the Great Recession. Russell Sage Foundation Journal of the Social Sciences, 3(3), 145–165.
#' Perusek, G. (2018). U.S. union membership data in perspective. Retrieved May 22, 2019, from [source](https://newlaborforum.cuny.edu/2018/03/03/u-s-union-membership-data-perspective/)
#'   Walker, A. N. (2014). Labor's enduring divide: The distinct path of public sector unions in the United States. Studies in American Political Development, 28(2), 175–200.
#' U.S. Census Bureau. (2018). Current Population Survey, 2018 Annual Social and Economic (ASEC) Supplement (machine-readable data file). Conducted for the Bureau of Labor Statistics, Washington, DC.
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'  "15 Main Street Flemington, NJ 08822",
#'  "120 E Main St, Ramsey, MN 55303",
#'  "1315 S Glenstone Ave, Springfield, MO 65804",
#'  "401 W 14th St, Austin, TX 78701",
#'  "340 S Lemon Ave, Walnut, CA 91789",
#'  "25 Main St, Northampton, MA 01060",
#'  "Non existent Ave",
#'  "100 Jersey Ave, New Brunswick, NJ 08901",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203",
#'  "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' test_tib <- keep_original_year(test_tib)
#' test_tib <- modify_year(test_tib, 2025)
#' test_tib <- get_GEOID(test_tib)
#' test_tib <- add_info_cols(test_tib)
#' test_tib <- get_county_geo(test_tib)
#'
#' add_pct_unionized(test_tib)
#'
#' @importFrom dplyr mutate case_when left_join
#'
#' @export
add_pct_unionized <- function(tib, current_year = 2025, default_year = 2024)
{
  union_data <- read.csv("geodeterminants_datasets/union/union_data.csv")
  tib <- tib %>%
    mutate(union_year = case_when(
      actual_year < 2000 ~ 2000,
      actual_year >= current_year ~ default_year,
      is.na(actual_year) ~ default_year,
      TRUE ~ actual_year)) %>%
    left_join(union_data, by = c("union_year" = "year", "state"))

  return(tib)
}

#' Get the Social Determinants of Health for each Address
#'
#' @description
#' ⚠️ **Note:** Data must be in tibble or vector form, read parameter descriptions for formatting break down.
#' You must have these packages installed for use of this package/function.
#' [`Street`], [`City`], [`State`] [`Area Code`] format works best for address.
#' Download and unzip the file from github for access to the required data [here](https://wchan05.github.io/data_access/).
#'
#' Converts a vector or tibble of addresses and supplementary data into Census
#' tract and block identifiers using \pkg{tidygeocoder}, and appends
#' tract-level measures of social determinants of health (SDOH) from
#' authoritative public data sources (e.g. ACS and EPA). Designed for public
#' health and policy research, this function provides a interlinking workflow for
#' joining address-level data to environmental, socioeconomic, and
#' demographic indicators.
#'
#' These packages must be installed for proper functionality:
#' **tidyverse**, **tidygeocoder**, **tibble**, **tidycensus**(API KEY), **tigris**, **sf**, **readxl**
#'
#' SDOH list that can be retrieved given strictly address data:
#' Air Quality Index, Concentrated Poverty, Educational Attainment – Community,
#' Environmental Justice, Food Swamp, Income Concentration, Minimum Wage,
#' Percent Unionized for Non-Agricultural Labor Force, Race/Ethnic Residential
#' Segregation – American Community Survey, Race/Ethnic Residential Segregation
#' – Separation (S) Index, Unbiased, Race/Ethnic Residential Segregation – U.S.
#' Census, Social Vulnerability
#'
#' @param gd_tib **Tibble** that must at least contain columns `address`,
#'   `state`(abbreviation for best use), and `year`(of diagnosis/source). This
#'   tibble may also contain other patient data that will be preserved
#'   throughout the function. Leave **NULL** if data is in *vector* form
#' @param gd_addresses **Vector** named `addresses` that contains addresses of
#'   patient/study subjects
#' @param gd_current_year The current year this function is being used
#' @param gd_minority_group_code The `B03002` table code of the minority group
#'   of interest. If the code isn't known leave null and select from the menu.
#'   [source](https://data.census.gov/table/ACSDT1Y2022.B03002)
#' @param gd_comparison_group_code The `B03002` table code of the comparison
#'   group of interest. If the code isn't known leave null and select from the
#'   menu. [source](https://data.census.gov/table/ACSDT1Y2022.B03002)
#' @param gd_minority_group_code_dhc The `P5` table code of the comparison group
#'   of interest. If the code isn't known leave null and select from the menu.
#'   Used when dates are 2020 and up. [DHC
#'   data](https://www.census.gov/data/tables/2023/dec/2020-census-dhc.html)
#' @param gd_minority_group_code_sf1 The `P005` table code of the comparison
#'   group of interest. If the code isn't known leave null and select from the
#'   menu. Used when dates are below 2020. [sf1
#'   data](https://www.census.gov/data/datasets/2010/dec/summary-file-1.html)
#' @param gd_current_fed_min_wage The current years minimum wage (e.g. current
#'   year: 2025, $7.25)
#'
#' @returns The **original** input tibble with **Social Determinants of
#'   Health(SDOH)** data appended for each address if data is available
#'   according to each determinants' function criteria
#'
#' @references "Structural Social Determinants of Health." PhenX Toolkit, Version 48.2, RTI International, 17 July 2025, [https://www.phenxtoolkit.org/sub-collections/view/32](https://www.phenxtoolkit.org/sub-collections/view/32).
#'
#' @examples
#' test_tib <- tibble(
#' address = c(
#'   "15 Main Street Flemington, NJ 08822",
#'   "120 E Main St, Ramsey, MN 55303",
#'   "1315 S Glenstone Ave, Springfield, MO 65804",
#'   "401 W 14th St, Austin, TX 78701",
#'   "340 S Lemon Ave, Walnut, CA 91789",
#'   "25 Main St, Northampton, MA 01060",
#'   "Non existent Ave",
#'   "100 Jersey Ave, New Brunswick, NJ 08901",
#'   "17350 Woodward Ave, Detroit, MI 48203",
#'   "17350 Woodward Ave, Detroit, MI 48203",
#'   "17350 Woodward Ave, Detroit, MI 48203"),
#' state = c("NJ", "MN", "MO", "TX", "CA", "MA", "PA", "NJ", "MI", "MI", "MI"),
#' year = c(2022, 2023, NA, 2023, NA, 2023, 2022, 2022, NA, 1990, 2020))
#'
#' view(geodeterminants(test_tib))
#'
#' addresses <- c(
#'   "1600 Pennsylvania Ave NW, Washington, DC 20500",
#'   "1 Infinite Loop, Cupertino, CA 95014",
#'   "350 5th Ave, New York, NY 10118",
#'   "233 S Wacker Dr, Chicago, IL 60606",
#'   "15 Main Street Flemington, NJ 08822",
#'   "Non existent Ave")
#'
#' view(geodeterminants(gd_addresses = addresses))
#' @export
get_geodeterminants <- function(gd_tib = NULL, gd_addresses = NULL, gd_current_year = 2025, gd_minority_group_code = NULL, gd_comparison_group_code = NULL, gd_minority_group_code_dhc = NULL, gd_minority_group_code_sf1 = NULL, gd_current_fed_min_wage = 7.25)
{
  check_tidycensus_key()

  cat("You will now be prompted to make four selections.\n",
      "Each selection corresponds to identifying the racial/ethnic group variable
    used in different census datasets.\n", "These groups are necessary to
    calculate segregation indices (e.g., dissimilarity and separation) and to
    ensure consistency across ACS, DHC, and SF1 data sources.\n\n",
      "Specifically:\n",
      "1. Select a minority group (ACS data, dissimilarity index).\n",
      "2. Select a comparison group (ACS data, separation index).\n",
      "3. Select a minority group for 2020 DHC data (used when 2020 data is
    included).\n",
      "4. Select a minority group from 2010 SF1 data (used for comparison with
    2020 to assess reliability).\n\n","Your choices will determine which
    population subgroups are analyzed in the
    subsequent calculations.\n\n")

  # menu 1
  if(is.null(gd_minority_group_code))
  {
    minority_groups <- c(
      "Not Hispanic or Latino" = "B03002_002",
      "Not Hispanic or Latino White alone" = "B03002_003",
      "Not Hispanic or Latino Black or African American alone" = "B03002_004",
      "Not Hispanic or Latino American Indian and Alaska Native alone" = "B03002_005",
      "Not Hispanic or Latino Asian alone" = "B03002_006",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_007",
      "Not Hispanic or Latino Some other race alone" = "B03002_008",
      "Not Hispanic or Latino Two or more races" = "B03002_009",
      "Not Hispanic or Latino Two races including Some other race" = "B03002_010",
      "Not Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_011",
      "Hispanic or Latino" = "B03002_012",
      "Hispanic or Latino White alone" = "B03002_013",
      "Hispanic or Latino Black or African American alone" = "B03002_014",
      "Hispanic or Latino American Indian and Alaska Native alone" = "B03002_015",
      "Hispanic or Latino Asian alone" = "B03002_016",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_017",
      "Hispanic or Latino Some other race alone" = "B03002_018",
      "Hispanic or Latino Two or more races" = "B03002_019",
      "Hispanic or Latino Two races including Some other race" = "B03002_020",
      "Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_021"
    )
    choice <- menu(names(minority_groups), title = "Select the group you want the dissimilarity index for:")
    gd_minority_group_code <- minority_groups[choice]

    cat("You selected:", names(gd_minority_group_code), "-", gd_minority_group_code, "\n")
  }

  # menu 2
  if(is.null(gd_comparison_group_code))
  {
    comparison_groups <- c(
      "Not Hispanic or Latino" = "B03002_002",
      "Not Hispanic or Latino White alone" = "B03002_003",
      "Not Hispanic or Latino Black or African American alone" = "B03002_004",
      "Not Hispanic or Latino American Indian and Alaska Native alone" = "B03002_005",
      "Not Hispanic or Latino Asian alone" = "B03002_006",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_007",
      "Not Hispanic or Latino Some other race alone" = "B03002_008",
      "Not Hispanic or Latino Two or more races" = "B03002_009",
      "Not Hispanic or Latino Two races including Some other race" = "B03002_010",
      "Not Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_011",
      "Hispanic or Latino" = "B03002_012",
      "Hispanic or Latino White alone" = "B03002_013",
      "Hispanic or Latino Black or African American alone" = "B03002_014",
      "Hispanic or Latino American Indian and Alaska Native alone" = "B03002_015",
      "Hispanic or Latino Asian alone" = "B03002_016",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "B03002_017",
      "Hispanic or Latino Some other race alone" = "B03002_018",
      "Hispanic or Latino Two or more races" = "B03002_019",
      "Hispanic or Latino Two races including Some other race" = "B03002_020",
      "Hispanic or Latino Two races excluding Some other race, and three or more races" = "B03002_021"
    )
    choice <- menu(names(comparison_groups), title = "Select the group you want the separation index for:")
    gd_comparison_group_code <- comparison_groups[choice]

    cat("You selected:", names(gd_comparison_group_code), "-", gd_comparison_group_code, "\n")
  }

  # menu 3
  if(is.null(gd_minority_group_code_dhc))
  {
    print("Because at least one of the years is counted as 2020, the dhc dataset must be used.")
    minority_groups_dhc <- c(
      "Not Hispanic or Latino" = "P5_002N",
      "Not Hispanic or Latino, White alone" = "P5_003N",
      "Not Hispanic or Latino, Black or African American alone" = "P5_004N",
      "Not Hispanic or Latino, American Indian and Alaska Native alone" = "P5_005N",
      "Not Hispanic or Latino Asian alone" = "P5_006N",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P5_007N",
      "Not Hispanic or Latino Some other race alone" = "P5_008N",
      "Not Hispanic or Latino Two or more races" = "P5_009N",
      "Hispanic or Latino" = "P5_010N",
      "Hispanic or Latino White alone" = "P5_011N",
      "Hispanic or Latino Black or African American alone" = "P5_012N",
      "Hispanic or Latino American Indian and Alaska Native alone" = "P5_013N",
      "Hispanic or Latino Asian alone" = "P5_014N",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P5_015N",
      "Hispanic or Latino Some Other Race alone" = "P5_016N",
      "Hispanic or Latino Two or More Races" = "P5_017N"
    )
    choice <- menu(names(minority_groups_dhc), title = "Select the group you want the dissimilarity index for:")
    gd_minority_group_code_dhc <- minority_groups_dhc[choice]
    cat("You selected:", names(gd_minority_group_code_dhc), "-", gd_minority_group_code_dhc, "\n")
  }

  # menu 4
  if(is.null(gd_minority_group_code_sf1))
  {
    print("Because not all data for 2020 is uploaded, sf1 (2010) data will be used as an estimate to compare the reliability of 2020s given data.")
    minority_groups_comp <- c(
      "Not Hispanic or Latino" = "P005002",
      "Not Hispanic or Latino, White alone" = "P005003",
      "Not Hispanic or Latino, Black or African American alone" = "P005004",
      "Not Hispanic or Latino, American Indian and Alaska Native alone" = "P005005",
      "Not Hispanic or Latino Asian alone" = "P005006",
      "Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005007",
      "Not Hispanic or Latino Some other race alone" = "P005008",
      "Not Hispanic or Latino Two or more races" = "P005009",
      "Hispanic or Latino" = "P005010",
      "Hispanic or Latino White alone" = "P005011",
      "Hispanic or Latino Black or African American alone" = "P005012",
      "Hispanic or Latino American Indian and Alaska Native alone" = "P005013",
      "Hispanic or Latino Asian alone" = "P005014",
      "Hispanic or Latino Native Hawaiian and Other Pacific Islander alone" = "P005015",
      "Hispanic or Latino Some Other Race alone" = "P005016",
      "Hispanic or Latino Two or More Races" = "P005017"
    )
    choice <- menu(names(minority_groups_comp), title = "Select the group you want the dissimilarity index for:")
    gd_minority_group_code_sf1 <- minority_groups_comp[choice]
    cat("You selected:", names(gd_minority_group_code_sf1), "-", gd_minority_group_code_sf1, "\n")
  }

  message("Fetching data... please wait. Thank you!")

  if(is.null(gd_tib) && is.null(gd_addresses))
  {
    return("Address data must be provided(Tibble or Vector)")
  }
  else if(is.null(gd_addresses))
  {
    gd_tib <- keep_original_year(tib = gd_tib)
    gd_tib <- modify_year(tib = gd_tib, current_year = gd_current_year)
    gd_tib <- geo_info(gd_tib)
  }
  else if(is.null(gd_tib))
  {
    gd_tib <- vector_to_tib(addresses = gd_addresses, current_year = gd_current_year)
  }
  else
  {
    return("Input the parameter `tib` or `addresses` and leave the other null(Tibble or Vector format)")
  }
  gd_tib <- add_education_attainment(tib = gd_tib)
  gd_tib <- add_dissimilarity_index(tib = gd_tib, minority_group_code = gd_minority_group_code)
  gd_tib <- add_separation_index(tib = gd_tib, comparison_group_code = gd_comparison_group_code)
  gd_tib <- add_decennial_dissimilarity(tib = gd_tib, minority_group_code_dhc = gd_minority_group_code_dhc, minority_group_code_sf1 = gd_minority_group_code_sf1)
  gd_tib <- add_concentrated_poverty(tib = gd_tib)
  gd_tib <- add_AQI(tib = gd_tib)
  gd_tib <- add_ICE(tib = gd_tib)
  gd_tib <- add_SVI(tib = gd_tib)
  gd_tib <- add_RFEI(tib = gd_tib)
  gd_tib <- add_EJ_index(tib = gd_tib)
  gd_tib <- add_min_wage(tib = gd_tib, current_fed_min_wage = gd_current_fed_min_wage)
  gd_tib <- add_pct_unionized(tib = gd_tib, current_year = gd_current_year)

  message("Completed!")

  return(gd_tib)
}

