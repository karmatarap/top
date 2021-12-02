# ==================================================
# Taxes
# ==================================================


#' Retrieve Tax expenditure for key areas by county
#' @family tax
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source [2019 Annual Survey of State Government Finances Tables](https://www.census.gov/data/tables/2019/econ/state/historical-tables.html)
#' @return dataframe
#' @format Percent of government budget spent on key areas
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{tax_education_percentage}{Percent of budget spent on education}
#'   \item{tax_welfare_percentage}{Percent of budget spent on welfare}
#'   \item{tax_health_percentage}{Percent of budget spent on health}
#' }
#'
get_taxes <- function() {
  taxes_2019 <- rio::import(file = "https://www2.census.gov/programs-surveys/state/tables/2019/2019_ASFIN_State_Totals.xlsx") %>%
    filter(`(Thousands of Dollars)` %in% c("Total revenue", "Education", "Public welfare", "Health")) %>%
    mutate(category = if_else(`(Thousands of Dollars)` == "Public welfare", "Welfare", `(Thousands of Dollars)`, `(Thousands of Dollars)`)) %>%
    select(-c(`United States`, `(Thousands of Dollars)`)) %>%
    pivot_longer(`Alabama`:`Wyoming`)

  total_rev <- taxes_2019 %>%
    filter(category == "Total revenue") %>%
    rename(total_val = value) %>%
    select(-c("category"))


  taxes_pct <- taxes_2019 %>%
    filter(category != "Total revenue") %>%
    left_join(total_rev) %>%
    mutate(percentage = value / total_val * 100, disp_name = glue::glue("tax_{tolower(category)}_percentage")) %>%
    rename(state_name = name) %>%
    pivot_wider(id_cols = state_name, names_from = disp_name, values_from = percentage)
  return(taxes_pct)
}
