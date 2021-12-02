# ==================================================
# Votes
# ==================================================




#' Retrieve national and local voting rate
#' @family vote
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source [National and Local Election](https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-585.html)
#' Table 4a
#' @return dataframe
#' @format Percent of government budget spent on key areas
#' \describe{
#'   \item{NAME}{Name of county}
#'   \item{local_vote_rate}{fraction of eligible voters that vote in local elections}
#'   \item{national_vote_rate}{fraction of eligible voters that vote in national elections}
#' }
#'
get_votes <- function() {
  national_votes <- rio::import(file = "https://www2.census.gov/programs-surveys/cps/tables/p20/585/table04a.xlsx")[7:57, c(1, 10)]
  colnames(national_votes) <- c("state_name", "national_vote_rate")
  national_votes <- national_votes %>%
    mutate(
      national_vote_rate = as.numeric(national_vote_rate),
      state_name = str_to_title(state_name)
    )


  local_votes <- rio::import(file = "https://www2.census.gov/programs-surveys/demo/tables/voting/table01.xlsx")[4:439, c(3, 8)]
  colnames(local_votes) <- c("state_name", "vote_rate")
  local_votes <- local_votes %>%
    mutate(vote_rate = as.numeric(vote_rate)) %>%
    group_by(state_name) %>%
    summarize(local_vote_rate = mean(vote_rate, na.rm = T)) %>%
    ungroup()

  votes <- local_votes %>% full_join(national_votes)
  return(votes)
}



#' Calculate voting power
#' @family vote
#' @param vmap tidycensus dataframe containing key, label and concept
#' @source 2019 ACS 5 year census
#' @return dataframe
#' @format Electoral College votes per million as a proxy for voting power
#' \describe{
#'   \item{GEOID}{Location map}
#'   \item{NAME}{Name of county}
#'   \item{national_vote_power}{EC votes per million voters}
#' }
#'
get_voting_power <- function() {


  # Voting Power
  state_pop <- tidycensus::get_acs(
    geography = "state",
    variables = c(pop = "B01003_001"),
    # state = my_states,
    year = 2019,
    survey = "acs5",
    geometry = F,
  )


  ec_votes <- tribble(
    ~state_name, ~ec_votes,
    "Alabama", 9,
    "Montana", 4,
    "Alaska", 3,
    "Nebraska", 5,
    "Arizona", 11,
    "Nevada", 6,
    "Arkansas", 6,
    "New Hampshire", 4,
    "California", 54,
    "New Jersey", 14,
    "Colorado", 10,
    "New Mexico", 5,
    "Connecticut", 7,
    "New York", 28,
    "Delaware", 3,
    "North Carolina", 16,
    "Florida", 30,
    "North Dakota", 3,
    "Georgia", 16,
    "Ohio", 17,
    "Hawaii", 4,
    "Oklahoma", 7,
    "Idaho", 4,
    "Oregon", 8,
    "Illinois", 19,
    "Pennsylvania", 19,
    "Indiana", 11,
    "Rhode Island", 4,
    "Iowa", 6,
    "South Carolina", 9,
    "Kansas", 6,
    "South Dakota", 3,
    "Kentucky", 8,
    "Tennessee", 11,
    "Louisiana", 8,
    "Texas", 40,
    "Maine", 4,
    "Utah", 6,
    "Maryland", 10,
    "Vermont", 3,
    "Massachusetts", 11,
    "Virginia", 13,
    "Michigan", 15,
    "Washington", 12,
    "Minnesota", 10,
    "West Virginia", 4,
    "Mississippi", 6,
    "Wisconsin", 10,
    "Missouri", 10,
    "Wyoming", 3
  )
  voting_power <- ec_votes %>%
    left_join(state_pop, by = c("state_name" = "NAME")) %>%
    # Voting power is Electoral College votes per million
    mutate(national_vote_power = ec_votes / (estimate / 1e6)) %>%
    select(state_name, national_vote_power)

  return(voting_power)
}
