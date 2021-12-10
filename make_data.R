# Load libraries
library(tidyverse)
library(tidycensus)
library(sf)

# Make sure CENSUS_API_KEY is a global env variable
# Sys.setenv(CENSUS_API_KEY = "...")

# Cache shapefiles
options(tigris_use_cache = TRUE)


# Get a list of available census data for years and surveys of interest
v2015_5 <- tidycensus::load_variables(2015, "acs5", cache = TRUE)
v2019_5 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)
v2019_1 <- tidycensus::load_variables(2019, "acs1", cache = TRUE)

# import all functions
data.funcs <- list.files("./data", pattern = "*.R$", full.names = T)
sapply(data.funcs, source, .GlobalEnv)


#----------------------------------------------------------------
# Census Data Sources
#----------------------------------------------------------------

# Population - required for big N
## Population + latitude/longitude data
population <- get_population()

# ===================================================================
# Community

## Ethnic Origin
orig_pct <- get_origin()

## Language
language_pct <- get_language()

## LGBT -- only available at state level
lgbt_pct <- get_lgbt()

## Cultural Diversity
race_di <- get_cultural_diversity()

## Economic Diversity S1901/S1902
econ_di <- get_economic_diversity()


# ===================================================================
# Opportunity

## Employment
emp_pct <- get_employment()



# ===================================================================
# Affordability
## Housing
## Mortgage & Rent
housing_afford <- get_housing()


#----------------------------------------------------------------
# Other data Sources
# EPA
#----------------------------------------------------------------

# ===================================================================
# Environment
environ <- get_environment()


# ===================================================================
# Tax
taxes_pct <- get_taxes()

# ===================================================================
# Vote

votes <- get_votes()
voting_power <- get_voting_power()


final_df <-
  lat_long %>%
  left_join(housing_afford) %>%
  left_join(emp_pct) %>%
  left_join(econ_di) %>%
  left_join(race_di) %>%
  left_join(language_pct) %>%
  left_join(orig_pct) %>%
  # rename(county_name=NAME, fips_code=GEOID) %>%
  mutate(state_name = word(NAME, 2, sep = ","), county_name = word(NAME, 1, sep = ",")) %>%
  rename(fips_code = GEOID) %>%
  left_join(environ) %>%
  select(-c(NAME, geometry)) %>%
  relocate(any_of(c("fips_code", "state_code", "state_name", "county_code", "county_name"))) %>%
  left_join(lgbt_pct %>% rename(state_code = GEOID) %>% select(-NAME)) %>%
  left_join(environ) %>%
  mutate(state_name = str_trim(state_name, side = "both")) %>%
  left_join(votes) %>%
  left_join(taxes_pct) %>%
  left_join(voting_power) %>% 
  mutate_all(~replace(., is.na(.), 0.0))


# make parameters csv
order <- data.frame(variable_names = names(final_df), data_type = sapply(final_df, class)) %>%
  mutate(
    n = str_count(variable_names, "_"),
    category = word(variable_names, 1, sep = "_"),
    subcategory = word(variable_names, 2, sep = "_"),
    metric = ifelse(n > 1, word(variable_names, -1, sep = "_"), ""),
    field = ifelse(n > 2, word(variable_names, 3, n, sep = "_"), "")
  ) %>%
  arrange(n > 1, variable_names) %>%
  select(category, subcategory, field, metric, variable_names, data_type)

# write out
order %>%
  mutate(data_variable = paste(category, subcategory, field, sep="_")) %>% 
  write_csv("parameters.csv")
final_df %>%
  select(order$variable_names) %>% 
  write_csv("final_df.csv")


# make pandas code

order %>% 
  filter(subcategory=="origin" & metric =="percentage") %>% 
  mutate(
    dummy = glue::glue("{category}_{subcategory}_{field}_rank"),
    actual= glue::glue("{category}_{subcategory}_{field}_{metric}"),
    code = glue::glue("dummy_data['{dummy}'] = actual_data['{actual}'].rank(pct=True)")) %>% 
  pull(code)
