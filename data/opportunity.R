# ==================================================
# Opportunity
# ==================================================


#' Calculate Employment Rate at the county level
#' @family opportunity
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source 2019 ACS 5 year
#' @return dataframe
#' @format Containing count and percent of county population with a particular origin
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{opportunity_employment_percentage}{Employed percent in each county}
#' }
#'
get_employment <- function() {
  unemp <- tidycensus::get_acs(
    geography = "county",
    variables = c(unemp_rate = "B23025_005E", total_emp = "B23025_003E"),
    year = 2019,
    survey = "acs5",
    geometry = F,
  )

  emp_pct <- unemp %>%
    pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = estimate) %>%
    mutate(opportunity_employment_percentage = (B23025_003 - B23025_005) / B23025_003 * 100) %>%
    select(GEOID, NAME, opportunity_employment_percentage)

  return(emp_pct)
}



#' Calculate population at the county level
#' @family opportunity
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source estimates
#' @return dataframe
#' @format Containing count and percent of county population with a particular origin
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{opportunity_population_count}{Population count in each county}
#'   \item{opportunity_population_density}{Population density in each county}
#' }
#'
get_population <- function() {

  ## Population and geometry
  population <- get_estimates("county",
    product = "population",
    geometry = T,
    output = "wide"
  ) %>%
    sf::st_transform(2273) %>% # convert to projected coord system for better centroid
    sf::st_centroid()

  data("fips_codes")

  lat_long <-
    do.call(rbind, st_geometry(population)) %>%
    as.data.frame() %>%
    rename(county_lat = V1, county_long = V2) %>%
    bind_cols(population) %>%
    mutate(state_code = str_sub(GEOID, 1, 2), county_code = str_sub(GEOID, 3)) %>%
    rename(opportunity_population_count = POP, opportunity_population_density = DENSITY)

  return(lat_long)
}
