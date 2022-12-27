library(tidycensus)
library(tidyverse)
library(magrittr)
library(janitor)
library(keyring)
library(jsonlite)

################ Functions built off direct census API calls ##################

#load variable key
#census_api - header url string for survey, e.g. "https://api.census.gov/data/2021/acs/acs5"
load_vars <- function(census_api){
  
  vars <- fromJSON(paste0(census_api,"/variables")) %>%
    as.data.frame() %>%
    row_to_names(row_number = 1)
  
  return(vars)
  
}

#pulls ACS pop counts for either munis or zctas
#census_api - header url string for survey of interest
#geo_type - quoted character, either "muni" or "zcta"
#place_code - quoted character, the census place code for muni or zcta of interest
#variable - quoted character, the variable or table you want to pull
#group - logical, set to T if pulling a table, F if pulling single variable
#pop_in_cook - logical, pull only pop within Cook by default, change to F if you want total pops for partial towns
#var_key - name of object storing variable lables, use load_vars to pull
get_small_geo_pops_acs <- function(census_api, geo_type, place_code, variable, group, pop_in_cook = T, var_key){
  
  #construct api from arguments
  variable_param <- ifelse(group, paste0("group(", variable,")"), variable)
  
  geo_param <- case_when(geo_type == "muni" & pop_in_cook ~ paste0("&for=county%20(or%20part):031&in=state:17%20place:", place_code),
                         geo_type == "muni" & pop_in_cook == F ~ paste0("&for=place:", place_code, "&in=state:17"),
                         geo_type == "zcta" & pop_in_cook == F ~ paste0("&for=zip%20code%20tabulation%20area:", place_code))
  
  #partial zctas not available in ACS, return error if selected
  if (geo_type == "zcta" & pop_in_cook) {return("Partial ZCTA populations are not available in ACS surveys. Please revise your function arguments.")}
  
  api <- paste0(census_api, "?get=NAME,GEO_ID,", variable_param, geo_param, "&key=", key_get("census-api-key"))
  
  #pull census data
  results <- try(fromJSON(api))
  
  #stop function if URL doesn't return anything
  if (any(class(results) == "try-error")) {
    return(paste("The API call failed for place code", place_code))
  }
  
  #clean data
  town_pull <- results %>% 
    as.data.frame() %>%
    row_to_names(row_number = 1) %>%
    pivot_longer(cols = -c(NAME, GEO_ID), names_to = "variable", values_to = "estimate") %>%
    filter(grepl("*(E|M)$", variable)) %>%    #keep only population estimates and MOEs
    separate(variable, into = c("variable", "estimate_type"), sep = -1) %>%
    pivot_wider(c(NAME, variable), names_from = estimate_type, values_from = estimate) %>%
    rename(estimate = E, moe = M) %>%
    mutate(variable = paste0(variable, "E")) %>%
    left_join(var_key, by = c("variable" = "name")) %>%   #link back to acs list of variables for label to ensure expected variables are pulled
    mutate(census_place_code = as.character(place_code)) %>%
    mutate(estimate = as.numeric(as.character(estimate))) 
  
  return(town_pull)
  
}

#example code
# acs_zcta_age_sex_total <- map(zctas, ~get_small_geo_pops_acs(place_code = .x, geo_type = "zcta", variable = "B01001", group = T, pop_in_cook = F, var_key = vars)) %>% 
#   discard(inherits, "character") %>%  
#   bind_rows()

################ Functions built off tidycensus package ##################

#Calculate a census estimate for CCDPH jurisdiction
ccdph_census_population = function(variable, year = 2010, sumfile = "sf1"){
  all_cook  = get_decennial(geography = "county", 
                                             state = "IL", county = "Cook",
                                             year = year, variables = variable, sumfile = sumfile) %>%
    pull(value)
  
  ooj  = get_decennial(geography = "place", 
                                        state = "IL", 
                                        year = year, variables = variable, sumfile = sumfile) %>%
    filter(NAME %in% c("Chicago city, Illinois",
                       "Evanston city, Illinois",
                       "Skokie village, Illinois",
                       "Oak Park village, Illinois"
    )) %>%
    pull(value) %>%
    sum()
  
  ooj2  = get_decennial(geography = "county subdivision", 
                                         state = "IL", county = "Cook",
                                         year = year, variables = variable, sumfile = sumfile) %>%
    filter(NAME == "Stickney township, Cook County, Illinois") %>%
    pull(value)
  
  ccdph = all_cook - ooj - ooj2
  return(ccdph)
  
}

#example- total population
# ccdph_census_population("P001001")

#example- males under 5 + females under 5 = total under 5
# ccdph_census_population("P012003") + ccdph_census_population("P012027")

#example all SEX BY AGE vars
# sex_by_age_vars = variables_sf1 %>% filter(concept == "SEX BY AGE") %>% pull(name)
# names(sex_by_age_vars) = variables_sf1 %>% filter(concept == "SEX BY AGE") %>% pull(label)

# sex_by_age_vector = sapply(sex_by_age_vars, ccdph_census_population)
# sex_by_age_ccdph = cbind(names(sex_by_age_vector), sex_by_age_vector) %>%
#   as.tibble() %>%
#   set_colnames(c("group", "population")) %>%
#   mutate(sex = str_extract(group, "(Male|Female)"),
#          sex = ifelse(is.na(sex), "Total", sex),
#          age = gsub("^.*!!", "", group),
#          age = ifelse(age %in% c("Total", "Male", "Female"), "Total", age)
#   ) %>%
#   relocate(population, .after = last_col())

#data tables
acs_2019_variables = load_variables(2019, "acs5", cache = TRUE)
census_2010_variables = load_variables(2010, "sf1", cache = TRUE)


#Calculate an ACS estimate for CCDPH jurisdiction
ccdph_acs_population = function(variable, year = 2019, survey = "acs5"){
  all_cook  = get_acs(geography = "county", survey = survey,
                      state = "IL", county = "Cook",
                      year = year, variables = variable) %>%
    pull(estimate)
  
  ooj  = get_acs(geography = "place",  survey = survey,
                 state = "IL", 
                 year = year, variables = variable) %>%
    filter(NAME %in% c("Chicago city, Illinois",
                       "Evanston city, Illinois",
                       "Skokie village, Illinois",
                       "Oak Park village, Illinois"
    )) %>%
    pull(estimate) %>%
    sum()
  
  ooj2  = get_acs(geography = "county subdivision",  survey = survey,
                  state = "IL", county = "Cook",
                  year = year, variables = variable) %>%
    filter(NAME == "Stickney township, Cook County, Illinois") %>%
    pull(estimate)
  
  ccdph = all_cook - ooj - ooj2
  return(ccdph)
  
}


