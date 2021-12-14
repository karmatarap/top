# ==================================================
# Affordability
# ==================================================


#' Calculate median mortgage and rent
#' @family affordability
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source estimates
#' @return dataframe
#' @format Median values
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{affordability_median_rent}{Housing Median Rent}
#'   \item{affordability_median_mortgage}{Housing Median Mortgage}
#' }
#'
get_housing <- function() {
  housing <- tidycensus::get_acs(
    geography = "county",
    variables = c(median_rent = "DP04_0134E", median_value = "DP04_0080E"),
    # state = my_states,
    year = 2019,
    survey = "acs5",
    geometry = F,
  )

  housing_afford <- housing %>%
    mutate(disp_name = ifelse(variable == "DP04_0080", "median_mortgage", "median_rent")) %>%
    pivot_wider(id_cols = c(GEOID, NAME), names_from = disp_name, values_from = estimate, names_glue = "affordability_{disp_name}")

  return(housing_afford)
}
