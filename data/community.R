# ==================================================
# Community
# ==================================================


#' Retrieve Ethnic Origin data at the county level
#' @family community
#' @param denom dataframe containg cencus total population for percent calculations
#' @source 2019 ACS 5 year
#' @return dataframe
#' @format Containing count and percent of county population with a particular origin
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{community_origin_[country]_count}{Total community from [country] within the county}
#'   \item{community_origin_[country]_percentage}{Percentage of community from [country] within the county}
#' }
#'
get_origin <- function(denom=population) {
  df_origin <- v2019_5 %>%
    dplyr::filter(startsWith(name, "B05006")) %>%
    tidyr::separate(label, into = c("type", "stat", "continent", "region", "country", "subcountry"), sep = "!!") %>%
    dplyr::mutate(disp_country = if_else(is.na(continent), "Total", coalesce(subcountry, country))) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "China"), "China", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "United Kingdom"), "United Kingdom", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "Czechoslovakia"), "Czechoslovakia", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "Democratic Republic of Congo"), "Democratic Republic of Congo", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "Portugal"), "Portugal", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "North Macedonia"), "North Macedonia", disp_country, disp_country)) %>%
    dplyr::mutate(disp_country = if_else(startsWith(country, "Other") | startsWith(disp_country, "Other"), "Other", disp_country, disp_country)) %>%
    dplyr::filter(disp_country != "" & !stringr::str_detect(name, "PR")) %>% # & !stringr::str_detect(disp_country, ":")) %>%
    dplyr::mutate(disp_name = namify(disp_country)) %>%
    dplyr::arrange(disp_country)

  origin <- tidycensus::get_acs(
    geography = "county",
    variables = setNames(df_origin$name, df_origin$disp_name),
    year = 2019,
    survey = "acs5",
    geometry = F,
  ) %>%
    group_by(GEOID, NAME, variable) %>%
    summarize(estimate = sum(estimate)) %>%
    ungroup()

  orig_denom <- origin %>%
    filter(variable == "total") %>%
    rename(orig_total = estimate) %>%
    select(GEOID, NAME, orig_total)

  orig_pct <- origin %>%
    filter(variable != "orig_total") %>%
    left_join(denom) %>%
    mutate(percentage = coalesce(estimate / POP * 100,0)) %>%
    rename(count = estimate) %>%
    pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = c(percentage, count), names_glue = "community_origin_{variable}_{.value}")
  return(orig_pct)
}



#' Retrieve language data at the county level
#' @family community
#' @param denom dataframe containg cencus total population for percent calculations
#' @source 2019 ACS 1 year
#' @return dataframe
#' @format Containing count and percent of county population with a particular spoken language
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{community_language_[language]_count}{Total speakers of [language] within the county}
#'   \item{community_language_[language]_percentage}{Percentage of speakers of [language] within the county}
#' }
#'
get_language <- function(denom=population) {
  df_language <- v2019_1 %>%
    dplyr::filter(startsWith(name, "B16001")) %>%
    tidyr::separate(label, into = c("type", "stat", "language", "fluency"), sep = ":?!!") %>%
    dplyr::mutate(language = coalesce(language, stat)) %>% # To capture total for percent calculations
    dplyr::filter(is.na(fluency) & language != "Speak only English") %>%
    dplyr::mutate(language = str_replace(language, ":", "")) %>%
    # If language has multiple subgroups, we pick the first language in the language family
    # This is to group languages into their parent language. Eg. French and French Creole will be French
    dplyr::mutate(language = ifelse(stringr::str_count(language, "[a-zA-Z-]+") > 1, str_extract(language, "[A-Za-z]+"), language)) %>%
    dplyr::mutate(disp_name = namify(language)) %>%
    dplyr::arrange(language)

  language <- tidycensus::get_acs(
    geography = "county",
    variables = setNames(df_language$name, df_language$disp_name),
    year = 2019,
    survey = "acs1",
    geometry = F,
  ) %>%
    group_by(GEOID, NAME, variable) %>%
    summarize(estimate = sum(estimate, na.rm = T)) %>%
    ungroup()

  language_pct <- language %>%
    filter(!variable %in% c("speak_only_english", "total")) %>%
    # Changing denom to total survey population
    left_join(denom) %>%
    mutate(percentage = coalesce(estimate / POP * 100,0)) %>%
    rename(count = estimate) %>%
    pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = c(percentage, count), names_glue = "community_language_{variable}_{.value}")

  return(language_pct)
}



#' Retrieve percentage of same-sex couples per household at state level
#' @family community
#' @source 2019 ACS 5 year
#' @return dataframe
#' @format Containing percent of state population in same-sex households
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of state}
#'   \item{community_lgbt_percentage}
#' }
#'
get_lgbt <- function() {
  df_lgbt <- v2019_5 %>%
    dplyr::filter(startsWith(name, "B11009")) %>%
    tidyr::separate(label, into = c("type", "stat", "status", "sex", "household"), sep = ":?!!") %>%
    dplyr::filter(is.na(status) | sex == "Same-sex:") %>%
    dplyr::mutate(disp_name = namify(if_else(is.na(status), "total", paste0(sex, status)), prefix = "lgbt"))

  lgbt <- tidycensus::get_acs(
    geography = "state",
    variables = setNames(df_lgbt$name, df_lgbt$disp_name),
    year = 2019,
    survey = "acs5",
    geometry = F,
  )

  lgbt_pct <- lgbt %>%
    pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = estimate) %>%
    mutate(
      lgbt_sum = lgbt_same_sex_married_couple_households + lgbt_same_sex_cohabiting_couple_households,
      community_lgbt_percentage = lgbt_sum / lgbt_total * 100
    ) %>%
    select(GEOID, NAME, community_lgbt_percentage)

  return(lgbt_pct)
}



#' Calculate cultural diversity using race as a proxy
#' @family community
#' @source 2019 ACS 5 year
#' @return dataframe
#' @format Shannon Index for race at the county level
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{community_cultural_index}
#' }
#'
get_cultural_diversity <- function() {
  df_race <- v2019_5 %>%
    dplyr::filter(startsWith(name, "B02001")) %>%
    tidyr::separate(label, into = c("type", "stat", "group", "subgroup", "subsubgroup"), sep = ":?!!") %>%
    dplyr::filter(is.na(subgroup) & !is.na(group)) %>%
    dplyr::mutate(disp_name = namify(group, prefix = "race"))

  race <- tidycensus::get_acs(
    geography = "county",
    variables = setNames(df_race$name, df_race$disp_name),
    year = 2019,
    survey = "acs5",
    geometry = F,
  )

  ## Diversity for race is Shannon Index -sum(p_i*log(p_i))
  race_di <- race %>%
    group_by(GEOID, NAME) %>%
    mutate(estimate = coalesce(estimate, 0)) %>%
    mutate(total = sum(estimate)) %>%
    ungroup() %>%
    mutate(pi = estimate / total, pi1 = ifelse(estimate == 0, 0, pi * log(pi))) %>%
    group_by(GEOID, NAME) %>%
    summarise(community_cultural_index = -sum(pi1))

  return(race_di)
}



#' Calculate economic diversity using household income (Aggregate household income in the past 12 month) of each racial group
#' @family community
#' @source 2019 ACS 5 year
#' @return dataframe
#' @format Shannon Index for income at the county level
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{community_economic_index}
#' }
#'
get_economic_diversity <- function() {
  df_econ <- v2019_5 %>%
    dplyr::filter(startsWith(name, "B1902")) %>%
    tidyr::separate(label, into = c("type", "stat"), sep = ":?!!") %>%
    dplyr::mutate(disp_name = (str_extract(concept, "\\([A-Z ]+\\)\\S*$"))) %>%
    dplyr::filter(!is.na(disp_name)) %>%
    dplyr::mutate(disp_name = namify(disp_name))

  econ <- tidycensus::get_acs(
    geography = "county",
    variables = setNames(df_econ$name, df_econ$disp_name),
    year = 2019,
    survey = "acs5",
    geometry = F,
  )

  ## Diversity for econ is Shannon Index -sum(p_i*log(p_i))
  ## Defining economic diversity as how equal income is across racial groups
  ## An alternative metric of interest may be income equality as measured by Gini coefficient
  econ_di <- econ %>%
    group_by(GEOID, NAME) %>%
    mutate(estimate = coalesce(estimate, 0)) %>%
    mutate(total = sum(estimate, na.rm = T)) %>%
    ungroup() %>%
    mutate(pi = estimate / total, pi1 = ifelse(estimate == 0, 0, pi * log(pi))) %>%
    group_by(GEOID, NAME) %>%
    summarise(community_economic_index = -sum(pi1)) %>%
    ungroup()

  return(econ_di)
}
