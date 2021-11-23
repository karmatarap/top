# Load libraries

library(tidyverse)
library(tidycensus)
library(sf)
Sys.setenv(CENSUS_API_KEY = "58191bd42d1e755d54d40334cd0c1fa8184e5aa5")

# Cache shapefiles
options(tigris_use_cache = TRUE)

# Convert texts to variable names
namify <- function(text, prefix = "") paste0(ifelse(prefix == "", prefix, paste0(prefix, "_")), str_replace_all(tolower(text), "[^a-z]", "_"))


v2015 <- tidycensus::load_variables(2015, "acs5", cache = TRUE)
# Get a list of available census data for 2019
v2019 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)


# Develop pipeline with subset of states first
my_states <- c("MA", "NY", "PA", "NJ")

#----------------------------------------------------------------
# Census Data Sources
#----------------------------------------------------------------

# ===================================================================
# Immigration

## Ethnic Origin
df_origin <- v2019 %>%
  dplyr::filter(startsWith(name, "B05006")) %>%
  tidyr::separate(label, into = c("type", "stat", "continent", "region", "country", "subcountry"), sep = "!!") %>%
  dplyr::mutate(disp_country = ifelse(is.na(continent), "Total", coalesce(subcountry, country))) %>%
  # dplyr::mutate(disp_country = ifelse(is.na(continent), "Total", disp_country)) %>%
  # tidyr::unite(., col = "disp_country", country, subcountry, na.rm = TRUE, sep = "") %>%
  dplyr::filter(disp_country != "" & !stringr::str_detect(name, "PR") & !stringr::str_detect(disp_country, ":")) %>%
  dplyr::mutate(disp_name = namify(disp_country)) %>%
  dplyr::arrange(disp_country)

origin <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_origin$name, df_origin$disp_name),
  #state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

orig_denom <- origin %>%
  filter(variable == "orig_total") %>%
  rename(orig_total = estimate) %>%
  select(GEOID, NAME, orig_total)

orig_pct <- origin %>%
  filter(variable != "orig_total") %>%
  left_join(orig_denom) %>%
  mutate(percentage = estimate / orig_total * 100) %>%
  rename(count = estimate) %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = c(percentage, count), names_glue = "immigrant_origin_{variable}_{.value}")


# ## Ancestry
# df_ancestry <- v2019 %>%
#   dplyr::filter(startsWith(name, "B04004")) %>%
#   tidyr::separate(label, into = c("type", "stat", "group", "specific"), sep = "!!") %>%
#   dplyr::mutate(disp_country = coalesce(specific, group)) %>%
#   dplyr::filter(!is.na(disp_country) & !stringr::str_detect(disp_country,":")) %>%
#   dplyr::mutate(disp_name = namify(disp_country)) %>%
#   dplyr::arrange(disp_country)
#
# ancestry <- tidycensus::get_acs(
#   geography = "county",
#   variables = setNames(df_ancestry$name, df_ancestry$disp_name),
#   state = my_states,
#   year = 2019,
#   survey = "acs5",
#   geometry = F,
# )
# write_csv(ancestry, "data/ancestry.csv")
#

## Language

df_language <- v2015 %>%
  dplyr::filter(startsWith(name, "B16001")) %>%
  tidyr::separate(label, into = c("type", "stat", "language", "fluency"), sep = ":?!!") %>%
  dplyr::mutate(language = coalesce(language, stat)) %>% # To capture total for percent calculations
  dplyr::filter(is.na(fluency)) %>%
  dplyr::mutate(language = str_replace(language, ":", "")) %>%
  dplyr::mutate(disp_name = namify(language, prefix = "lang")) %>%
  dplyr::arrange(language)


language <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_language$name, df_language$disp_name),
  #state = my_states,
  year = 2015,
  survey = "acs5",
  geometry = F,
)

language_pct <- language %>%
  filter(!variable %in% c("speak_only_english", "total")) %>%
  left_join(language %>% filter(variable == "total") %>% select(GEOID, NAME, estimate) %>% rename(total = estimate)) %>%
  mutate(percentage = estimate / total * 100) %>%
  rename(count = estimate) %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = c(percentage, count), names_glue = "immigrant_language_{variable}_{.value}")


# ===================================================================
# Diversity

# LGBT -- only available at state level
df_lgbt <- v2019 %>%
  dplyr::filter(startsWith(name, "B11009")) %>%
  tidyr::separate(label, into = c("type", "stat", "status", "sex", "household"), sep = ":?!!") %>%
  dplyr::filter(is.na(status) | sex == "Same-sex:") %>%
  dplyr::mutate(disp_name = namify(if_else(is.na(status), "total", paste0(sex, status)), prefix = "lgbt"))



lgbt <- tidycensus::get_acs(
  geography = "state",
  variables = setNames(df_lgbt$name, df_lgbt$disp_name),
  
  #state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

lgbt_pct <- lgbt %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = estimate) %>%
  mutate(
    lgbt_sum = lgbt_same_sex_married_couple_households + lgbt_same_sex_cohabiting_couple_households,
    diversity_lgbt_percentage = lgbt_sum / lgbt_total * 100
  ) %>%
  select(GEOID, NAME, diversity_lgbt_percentage)



# Race
df_race <- v2019 %>%
  dplyr::filter(startsWith(name, "B02001")) %>%
  tidyr::separate(label, into = c("type", "stat", "group", "subgroup", "subsubgroup"), sep = ":?!!") %>%
  dplyr::filter(is.na(subgroup) & !is.na(group)) %>%
  dplyr::mutate(disp_name = namify(group, prefix = "race"))


race <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_race$name, df_race$disp_name),
  #state = my_states,
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
  summarise(diversity_cultural_index = -sum(pi1))



# Economic Diversity S1901/S1902
df_econ <- v2019 %>%
  dplyr::filter(startsWith(name, "B1902")) %>%
  tidyr::separate(label, into = c("type", "stat"), sep = ":?!!") %>%
  dplyr::mutate(disp_name = (str_extract(concept, "\\([A-Z ]+\\)\\S*$"))) %>%
  dplyr::filter(!is.na(disp_name)) %>%
  dplyr::mutate(disp_name = namify(disp_name))

econ <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_econ$name, df_econ$disp_name),
  #state = my_states,
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
  summarise(diversity_economic_index = -sum(pi1)) %>%
  ungroup()


#
# df_industry <- v2019 %>%
#   dplyr::filter(startsWith(name, "C24050"))  %>%
#   tidyr::separate(label, into = c("type", "stat", "industry", "subindustry"), sep = ":?!!") %>%
#   dplyr::filter(is.na(subindustry) & !is.na(industry)) %>%
#   dplyr::arrange(industry)
#
# df_occupation <- v2019 %>%
#   dplyr::filter(startsWith(name, "C24060")) %>%
#   tidyr::separate(label, into = c("type", "stat", "occupation", "suboccupation"), sep = ":?!!")
#
#

# ===================================================================
# Opportunity

## Unemployment
unemp <- tidycensus::get_acs(
  geography = "county",
  variables = c(unemp_rate = "B23025_005E", total_emp = "B23025_003E"),
  #state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

emp_pct <- unemp %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = variable, values_from = estimate) %>%
  mutate(opportunity_employment_percentage = (B23025_003 - B23025_005) / B23025_003 * 100) %>%
  select(GEOID, NAME, opportunity_employment_percentage)

# Housing
housing <- tidycensus::get_acs(
  geography = "county",
  variables = c(median_rent = "DP04_0134E", median_value = "DP04_0080E"),
  #state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

housing_afford <- housing %>%
  mutate(disp_name = ifelse(variable == "DP04_0080", "median_rent", "median_mortgage")) %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from = disp_name, values_from = estimate, names_glue = "housing_{disp_name}")



## Population and geometry
population <- get_estimates("county",
                            product = "population",
                            #state = my_states,
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


#----------------------------------------------------------------
# Other data Sources
# EPA
#----------------------------------------------------------------
# ===================================================================
# Opportunity
## Data taken from EPA
## https://edg.epa.gov/EPADataCommons/public/ORD/CPHEA/EQI_2006_2010/
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

# Voting
## Census
## National Election
## https://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-585.html
## Table 4a 

national_votes <- rio::import(file = 'https://www2.census.gov/programs-surveys/cps/tables/p20/585/table04a.xlsx')[7:57,c(1,10)]  
colnames(national_votes) <- c("state_name", "national_vote_rate")
national_votes <- national_votes %>% 
  mutate(national_vote_rate = as.numeric(national_vote_rate),
         state_name = str_to_title(state_name))

 
local_votes <- rio::import(file = 'https://www2.census.gov/programs-surveys/demo/tables/voting/table01.xlsx')[4:439,c(3,8) ] 
colnames(local_votes) <- c("state_name", "vote_rate")
local_votes <- local_votes %>% 
  mutate(vote_rate = as.numeric(vote_rate)) %>% 
  group_by(state_name) %>% 
  summarize(local_vote_rate = mean(vote_rate, na.rm=T)) %>% 
  ungroup()

votes <- local_votes %>% full_join(national_votes) 

# Voting Power
state_pop <- tidycensus::get_acs(
  geography = "state",
  variables = c(pop="B01003_001"),
 # state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)


ec_votes <- tribble(
  ~state_name,   ~ec_votes, 	
  "Alabama" ,	9, 	
  "Montana", 	4,
  "Alaska" 	,3 ,	
  "Nebraska" ,	5,
  "Arizona" ,	11, 	
  "Nevada" 	,6,
  "Arkansas" 	,6, 	
  "New Hampshire", 	4,
  "California" ,	54 	,
  "New Jersey" ,	14,
  "Colorado" 	,10 	,
  "New Mexico" ,	5,
  "Connecticut", 	7, 	
  "New York" 	,28,
  "Delaware" 	,3 ,	
  "North Carolina" ,	16,
  "Florida" 	,30 	,
  "North Dakota", 	3,
  "Georgia", 	16 	,
  "Ohio" 	,17,
  "Hawaii" 	,4, 	
  "Oklahoma" ,	7,
  "Idaho" 	,4 	,
  "Oregon" 	,8,
  "Illinois" 	,19, 	
  "Pennsylvania", 	19,
  "Indiana" 	,11 	,
  "Rhode Island", 	4,
  "Iowa" 	,6 	,
  "South Carolina", 	9,
  "Kansas" 	,6 	,
  "South Dakota" ,	3,
  "Kentucky" 	,8, 	
  "Tennessee" ,	11,
  "Louisiana" ,	8, 	
  "Texas" ,	40,
  "Maine" ,	4, 	
  "Utah" 	,6,
  "Maryland" 	,10, 	
  "Vermont" 	,3,
  "Massachusetts", 	11, 	
  "Virginia" 	,13,
  "Michigan" 	,15 ,	
  "Washington" 	,12,
  "Minnesota" 	,10 	,
  "West Virginia" ,	4,
  "Mississippi" 	,6, 	
  "Wisconsin" 	,10,
  "Missouri" 	,10 ,	
  "Wyoming" 	,3
)
voting_power <- ec_votes %>% 
  left_join(state_pop, by=c("state_name"="NAME")) %>% 
  #Voting power is Electoral College votes per million
  mutate(national_vote_power = ec_votes / (estimate/1e6)) %>% 
  select(state_name, national_vote_power)


# Taxes
## Census
## 2019 Annual Survey of State Government Finances Tables
## https://www.census.gov/data/tables/2019/econ/state/historical-tables.html

 
taxes_2019 <- rio::import(file = 'https://www2.census.gov/programs-surveys/state/tables/2019/2019_ASFIN_State_Totals.xlsx')%>% 
  filter(`(Thousands of Dollars)` %in% c("Total revenue","Education","Public welfare","Health")) %>% 
  select(-c(`United States`)) %>% 
  rename(category = `(Thousands of Dollars)`) %>% 
  pivot_longer(`Alabama`:`Wyoming`)

total_rev <- taxes_2019 %>% 
  filter(category=="Total revenue") %>% 
  rename(total_val=value) %>% 
  select(-c("category"))

taxes_pct <- taxes_2019 %>% 
  filter(category != "Total revenue") %>% 
  left_join(total_rev) %>% 
  mutate(percentage = value/total_val*100, disp_name = glue::glue("tax_{tolower(category)}_percentage")) %>% 
  rename (state_name=name) %>% 
  pivot_wider(id_cols = state_name, names_from = disp_name, values_from=percentage)

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
  mutate(state_name = str_trim(state_name, side="both")) %>% 
  left_join(votes) %>% 
  left_join(taxes_pct) %>% 
  left_join(voting_power)


# make parameters csv
order <- data.frame(variable_names = names(final_df), data_type = sapply(final_df, class) ) %>% 
  mutate(n = str_count(variable_names, "_"),
         category = word(variable_names, 1, sep="_"), 
         subcategory=word(variable_names, 2, sep="_"), 
         metric=ifelse(n>1,word(variable_names,-1, sep="_"),""), 
         field = ifelse(n>2, word(variable_names, 3, n, sep="_"),"")) %>% 
  arrange(n>1,variable_names ) %>% 
  select(category, subcategory, field, metric, variable_names, data_type) 

# write out
order %>% 
  write_csv("parameters.csv")
final_df[, order$variable_names] %>% 
  write_csv( "final_df.csv")


#
#TODO
# Add Tax 
# Populate Actual Data Sheet
# Populate categories sheet
# Table to map languages?
# Add Readme of data sources
