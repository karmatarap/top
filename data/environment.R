# ===================================================================
# Environment

#' Retrieve environmental indices of interest
#' @family community
#' @source [EPA](https://edg.epa.gov/EPADataCommons/public/ORD/CPHEA/EQI_2006_2010/)
#' @return dataframe
#' @format Evironmental indices
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{environment_social_index}
#'   \item{environment_build_index}
#'   \item{environment_air_index}
#'   \item{environment_water_index}
#'   \item{environment_land_index}
#' }
#'
get_environment <- function() {
  environ <- rio::import("https://edg.epa.gov/data/public/ORD/CPHEA/EQI_2006_2010/2006_2010_EQI_2Jan2018_VC.csv") %>%
    select(!matches("(RUCC)|\\d$")) %>% # remove unnecessary varibales
    rename(
      environment_social_index = sociod_EQI_2Jan2018_VC,
      environment_build_index = built_EQI_2Jan2018_VC,
      environment_air_index = air_EQI_2Jan2018_VC,
      environment_water_index = water_EQI_2Jan2018_VC,
      environment_land_index = land_EQI_2Jan2018_VC,
    ) %>%
    mutate(fips_code = str_pad(stfips, 5, pad = "0")) %>%
    select(fips_code, starts_with("environ"))
  return(environ)
}
