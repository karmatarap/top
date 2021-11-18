# Load libraries

library(tidyverse)
library(tidycensus)

Sys.setenv(CENSUS_API_KEY="58191bd42d1e755d54d40334cd0c1fa8184e5aa5")

# Cache shapefiles
options(tigris_use_cache = TRUE)

# Convert texts to variable names
namify <- function(text, prefix="") paste0(ifelse(prefix=="",prefix,paste0(prefix,'_')),str_replace_all(tolower(text),"[^a-z]","_")) 

 
v2015 <- tidycensus::load_variables(2015, "acs5", cache = TRUE)
# Get a list of available census data for 2019
v2019 <- tidycensus::load_variables(2019, "acs5", cache = TRUE)
 


# Develop pipeline with subset of states first
my_states <- c("MA", "NY", "PA", "NJ")


#===================================================================
# Immigration

## Ethnic Origin
df_origin <- v2019 %>%
  dplyr::filter(startsWith(name, "B05006")) %>%
  tidyr::separate(label, into = c("type", "stat", "continent", "region", "country", "subcountry"), sep = "!!") %>%
  dplyr::mutate(disp_country = ifelse(is.na(continent),"Total", coalesce(subcountry, country)) )%>% 
  #dplyr::mutate(disp_country = ifelse(is.na(continent), "Total", disp_country)) %>% 
  #tidyr::unite(., col = "disp_country", country, subcountry, na.rm = TRUE, sep = "") %>%
  dplyr::filter(disp_country != "" & !stringr::str_detect(name, 'PR') & !stringr::str_detect(disp_country, ':') ) %>%
  dplyr::mutate(disp_name=namify(disp_country, prefix = "orig")) %>% 
  dplyr::arrange(disp_country)

origin <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_origin$name, df_origin$disp_name), 
  state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

orig_denom <- origin %>% filter(variable == "orig_total") %>% 
  rename(orig_total = estimate) %>% 
  select(GEOID, NAME, orig_total)

orig_pct <- origin %>% 
  filter(variable != "orig_total") %>% 
  left_join(orig_denom) %>% 
  mutate(percentage = estimate / orig_total * 100) %>% 
  rename(count=estimate) %>% 
  pivot_wider(id_cols=c(GEOID, NAME), names_from =variable, values_from=c(percentage, count), names_glue = "immigrant_origin_{variable}_{.value}")

write_csv(orig_pct, "data/origin_pct.csv")

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
  dplyr::mutate(language=coalesce(language,stat)) %>% # To capture total for percent calculations
  dplyr::filter(is.na(fluency)) %>%
  dplyr::mutate(language = str_replace(language, ":", "")) %>%
  dplyr::mutate(disp_name = namify(language, prefix="lang")) %>% 
  dplyr::arrange(language)


language <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_language$name, df_language$disp_name), 
  state = my_states,
  year = 2015,
  survey = "acs5",
  geometry = F,
)

language_pct <- language %>% 
  filter(! variable %in% c("speak_only_english","total")) %>% 
  left_join(language %>% filter(variable =="total") %>% select(GEOID, NAME, estimate) %>% rename(total=estimate)) %>% 
  mutate(percentage = estimate/total*100) %>% 
  rename(count=estimate) %>% 
  pivot_wider(id_cols=c(GEOID, NAME), names_from =variable, values_from=c(percentage, count), names_glue = "immigrant_language_{variable}_{.value}")
write_csv(language_pct, "data/language_pct.csv")

#===================================================================
# Diversity

# LGBT -- only available at state level
df_lgbt <- v2019 %>%
  dplyr::filter(startsWith(name, "B11009")) %>%
  tidyr::separate(label, into = c("type", "stat", "status", "sex","household"), sep = ":?!!") %>%
  dplyr::filter(is.na(status) | sex == "Same-sex:")  %>% 
  dplyr::mutate(disp_name = namify(if_else(is.na(status), "total", paste0(sex, status)), prefix="lgbt"))
  


lgbt <- tidycensus::get_acs(
  geography = "state",
  variables = setNames(df_lgbt$name, df_lgbt$disp_name), 
  state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

lgbt_pct <- lgbt %>% 
  pivot_wider(id_cols = c(GEOID, NAME), names_from=variable, values_from=estimate) %>% 
  mutate(lgbt_sum = lgbt_same_sex_married_couple_households + lgbt_same_sex_cohabiting_couple_households, 
         diversity_lgbt_percentage = lgbt_sum / lgbt_total * 100) %>% 
  select(GEOID, NAME, diversity_lgbt_percentage)

write_csv(lgbt_pct, "data/lgbt.csv")


# Race
df_race <- v2019 %>%
  dplyr::filter(startsWith(name, "B02001")) %>%
  tidyr::separate(label, into = c("type", "stat", "group", "subgroup","subsubgroup"), sep = ":?!!") %>%
  dplyr::filter(is.na(subgroup) & !is.na(group)) %>% 
  dplyr::mutate(disp_name = namify(group, prefix="race"))


race <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_race$name, df_race$disp_name), 
  state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

## Diversity for race is Shannon Index -sum(p_i*log(p_i))
race_di <- race %>% 
  group_by(GEOID, NAME) %>% 
  mutate(estimate=coalesce(estimate,0)) %>% 
  mutate(total = sum(estimate)) %>% 
  ungroup() %>% 
  mutate(pi = estimate/total , pi1 = ifelse(estimate == 0, 0, pi*log(pi))) %>% 
  group_by(GEOID, NAME) %>% 
  summarise(diversity_cultural_index = -sum(pi1))

write_csv(race_di, "data/race_di.csv")

# Economic Diversity S1901/S1902
df_econ <- v2019 %>%
  dplyr::filter(startsWith(name, "B1902")) %>%
  tidyr::separate(label, into = c("type", "stat"), sep = ":?!!") %>%
  dplyr::mutate(disp_name=(str_extract(concept,"\\([A-Z ]+\\)\\S*$"))) %>% 
  dplyr::filter(!is.na(disp_name)) %>% 
  dplyr::mutate(disp_name=namify(disp_name))

econ <- tidycensus::get_acs(
  geography = "county",
  variables = setNames(df_econ$name, df_econ$disp_name), 
  state = my_states,
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
  mutate(total = sum(estimate, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(pi = estimate/total , pi1 = ifelse(estimate == 0, 0, pi*log(pi))) %>% 
  group_by(GEOID, NAME) %>% 
  summarise(diversity_economic_index = -sum(pi1)) %>% 
  ungroup()

write_csv(econ_di, "data/econ_di.csv")
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

# Opportunity

## Unemployment
unemp <- tidycensus::get_acs(
  geography = "county",
  variables = c(unemp_rate="B23025_005E", total_emp = "B23025_003E"), 
  state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

emp_pct <- unemp %>%
  pivot_wider(id_cols = c(GEOID, NAME), names_from =variable, values_from =estimate ) %>% 
  mutate(opportunity_employment_percentage = (B23025_003-B23025_005)/ B23025_003*100) %>% 
  select(GEOID, NAME, opportunity_employment_percentage)

# Housing
housing <- tidycensus::get_acs(
  geography = "county",
  variables = c(median_rent="DP04_0134E", median_value = "DP04_0080E"), 
  state = my_states,
  year = 2019,
  survey = "acs5",
  geometry = F,
)

housing_afford <- housing %>% 
  mutate(disp_name = ifelse(variable=="DP04_0080","median_rent","median_mortgage")) %>% 
  pivot_wider(id_cols=c(GEOID, NAME), names_from=disp_name, values_from=estimate, names_glue = "housing_{disp_name}")
