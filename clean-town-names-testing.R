source("miscellaneous-helper-functions.R")
library(keyring)
library(janitor)
library(stringr)
library(stringdist)

library(magrittr)
library(tidyverse)

library(readxl)




downloads_path = key_get("downloads_path")
test = read_csv(file_name("Covid Patients by County", extension = "csv"), skip = 3) %>%
  clean_names() %>%
  mutate(state = gsub("^.*\\, ", "", city),
         city = gsub("\\,.*$", "", city), 
         zip = gsub("\\-.*$", "", zip_code)
         ) %>%
  select(city, zip)

il_cities = read_csv("data/illinois-cities.csv")
city_zips = read_csv("data/cook-zipcodes-and-cities.csv")
chi_areas = read_csv("data/chicago-neighborhoods.csv")
chi_areas = c(chi_areas$neighborhood, chi_areas$community_area) %>% unique() 
chi_areas = chi_areas[!grepl("\\,", chi_areas)]
il_zips = read_csv("data/illinois-zip-codes.csv")



find_closest_match = function(name, possible_matches, threshold = 0.25, method = "jw"){
  #function used within clean_cook_town to find best fuzzy match for town names
    
  matches = stringdist(tolower(name), tolower(possible_matches), method = method)
  
  best = which.min(matches)
  
  if(min(matches) <= threshold){
    return(possible_matches[best])
  }else{
    return(NA_character_)
  }
}

clean_cook_town = function(town, zip_code = NULL){
  
  if(is.na(town)){
    return(NA_character_)
  }
  
  #check if blank/null
  if(town %in% c("Null", "Illinois", "Il", "Needs Updating", "", "Cook",
                 "Unknown", "Homeless", "Cook County", "Lake", "Usa",
                 "N", "N/A", "Illinois City")){
    return(NA_character_)
  }
  
  
  #initial cleaning for better matching
  town %<>%
    str_to_title() %>%
    trimws() %>%
    gsub("\\Hts\\b", "Heights", .) %>%
    gsub("\\Hgts\\b", "Heights", .) %>%
    gsub("\\Vlg\\b", "Village", .) %>%
    gsub("\\Pk\\b", "Park", .) %>%
    gsub("\\Mt\\b", "Mount", .) %>%
    gsub("\\Spgs\\b", "Springs", .) %>%
    gsub("\\bN\\b", "North", .) %>%
    gsub("\\bS\\b", "South", .) %>%
    gsub("\\bE\\b", "East", .) %>%
    gsub("\\bW\\b", "West", .) %>%
    gsub("\\.|\\,|\\`", "", .) %>%
    gsub("  ", " ", .) %>%
    gsub("\\bSo\\b", "South", .) %>%
    gsub("Mccook", "McCook", .) %>%
    gsub("La Grange Highlands", "La Grange", .) %>%
    gsub("Summit Argo|\\Argo\\b", "Summit", .) %>%
    gsub("Techny", "Northbrook", .) %>%
    gsub("\\Chgo\\b|\\Chg\\b|\\Cgo\\b", "Chicago", .) 
    
  
  #check if real town name
  if(town %in% il_cities$city){
    return(town)
  }
  

  #check if Chicago neighborhood/community area (unless neighborhood name is also a town in IL)
  if(town %in% setdiff(chi_areas, il_cities$city)){
    return("Chicago")
  }
  
  #check if super close to a town name with jw
  closest_match = find_closest_match(town, il_cities$city, threshold =0.05)
  #if no closest match with jw- use osa to see if only one letter off from something (and only one thing)
  if(is.na(closest_match)){
    if(length(which(stringdist(tolower(town), tolower(il_cities$city)) == 1)) == 1){
      closest_match = find_closest_match(town, il_cities$city, threshold = 1, 
                                         method = "osa")
    }
  }
  if(!is.na(closest_match)){
    return(closest_match)
  }
  
  #if zipcode provided, match within zipcode towns
  if(!is.null(zip_code) && zip_code %in% city_zips$zip){
    zip_towns = city_zips %>%
      filter(zip == zip_code) %>%
      pull(city)
    
    match = find_closest_match(town, zip_towns, threshold = 0.35)
    
    #Check to see if matches with a Chi community area if no match to towns
    if(is.na(match) & "Chicago" %in% zip_towns){
        if(!is.na(find_closest_match(town, chi_areas, threshold = 0.2))){
          match = "Chicago"
      }
    }
    
    if(!is.na(match)){
      return(match)
    }
    
  }
  
  #if zipcode provided and is not in IL- stricter matching
  if(!is.null(zip_code) && !is.na(zip_code) && !(zip_code %in% il_zips$zip)){
    return(find_closest_match(town, il_cities$city, threshold =  0.1))
  }
  
  #try matching with cook towns
  match = find_closest_match(town, il_cities$city[il_cities$in_cook], threshold = 0.25)
  if(!is.na(match)){
    return(match)
  }
  
  #try matching with all il towns
  match = find_closest_match(town, il_cities$city, threshold = 0.2)
  if(!is.na(match)){
    return(match)
  }
  
  return(NA_character_)
  
  
}

#Clean names for multiple towns instead of one
clean_cook_towns = function(towns, zips = NULL){
  out = character()
  for(i in 1:length(towns)){
    town = towns[i]
    zip_code = zips[i]
    
    clean = clean_cook_town(town, zip_code)
    out = c(out, clean)
  }
  return(out)
}

start_time = Sys.time()
test_clean2 = test %>%
  mutate(clean = clean_cook_towns(city, zip))
end_time = Sys.time()
end_time-start_time #takes about 1 second per 1000 towns

test_clean2_dif = test_clean2 %>%
  filter(clean != str_to_title(city)) %>%
  mutate(clean_no_zip = clean_cook_towns(city),
         dif_no_zip = clean != clean_no_zip
         ) %>%
  unique()

test_clean = test %>% 
  rowwise() %>%
  mutate(clean = clean_cook_town(city)) %>%
  filter(is.na(clean))


test_methods_fx = function(towns, zips = NULL, fuzzy_method = "cosine",
                           weight = c(d = 1, i = 1, s = 1, t = 1),
                           p = 0, bt = 0){
  out = character()
  for(i in 1:length(towns)){
    town = towns[i]
    town = str_to_title(town)
    zip_code = zips[i]
    
    if(is.null(zip_code) || !(zip_code %in% city_zips$zip)){
      possible_matches = il_cities$city
    }
    
    else{
      possible_matches = city_zips %>%
        filter(zip == zip_code) %>%
        pull(city)
      
    }
    
    if("Chicago" %in% possible_matches & town %in% chi_areas){
      possible_matches = "Chicago"
    }
    
    
    best = which.min(stringdist(town, possible_matches, method = fuzzy_method,
                                weight = weight, p=p, bt = bt))
    
    out = c(out, possible_matches[best])
    
  }
  return(out)
  
}

test_methods = test_clean %>%
  mutate(
         jw = test_methods_fx(city, zip, "jw"),
         jw_all = test_methods_fx(city, fuzzy_method = "jw"),
         
         jw_dist = stringdist(str_to_title(city), jw_all, method = "jw"),
         
         cosine = test_methods_fx(city, zip, "cosine"),
         cosine_all = test_methods_fx(city, fuzzy_method = "cosine"),
         
         cos_dist = stringdist(str_to_title(city), jw_all, method = "cosine")
         
         ) %>%
  select(-clean)

test_clean_new = test_clean %>%
  mutate(clean = clean_cook_towns_fuzzy(city, zip),
         clean_no_zip = clean_cook_towns_fuzzy(city),
         dif = clean != clean_no_zip
         )

case_towns_unmatched = read_csv(paste0(downloads_path, "case_towns.csv")) %>%
  set_colnames("city") %>%
  filter(!(city %in% il_cities$city)) %>%
  unique() %>%
  mutate(clean_city = clean_cook_towns_fuzzy(city))

####################################################################################################
city_in_zip = function(towns, zips){
  out = logical()
  for(i in 1:length(towns)){
    town = towns[i]
    zip = zips[i]
    
    if(is.na(town)|is.na(zip)){
      out = c(out, NA)
    }else{
      l = length(which(city_zips$zip == zip & city_zips$city == town)) > 0
      out = c(out, l)
    }
    
  }
  return(out)
  
}

case_towns_zips = read_csv("/Users/hannahsteinberg/Downloads/New__Document.csv") %>%
  set_colnames(c("city", "zip")) %>%
  filter(!(city %in% il_cities$city)) %>%
  unique() %>%
  mutate(clean_city_zip = clean_cook_towns(city, zip),
         clean_city = clean_cook_towns(city),
         dif = clean_city_zip != clean_city,
         in_zip = city_in_zip(clean_city_zip, zip),
         stringdist_city_zip = stringdist(city, clean_city_zip, "jw"),
         stringdist_city = stringdist(city, clean_city, "jw")
         )
####################################################################################################

#commonly mispelled town names vaccine data
vaccine_mispellings = read_xlsx("/Users/hannahsteinberg/Downloads/common_town_misspellings_vaccine.xlsx") %>%
  mutate(clean = clean_cook_towns(city),
         stringdist = stringdist(city, clean, "jw")
         )
write_csv(vaccine_mispellings, paste0(downloads_path, "vaccine_misspellings_clean.csv"))

#read in check
check = read_csv("check-clean-towns-cases.csv") %>%
  filter(!is.na(result)) %>%
  mutate(stringdist = stringdist(city, clean_city_zip, "jw"),
         clean2_zip = clean_cook_towns(city, zip),
         clean2_no_zip = clean_cook_towns(city),
         zip_same = clean_city_zip == clean2_zip,
         no_zip_same = clean_city == clean2_no_zip,
  ) %>%
  rowwise() %>%
  mutate(closest = ifelse(is.na(city), NA,find_closest_match(city, il_cities$city, 0.05)))


round(sort(table(check$result), decreasing = T)/nrow(check)*100,1)

out_of_state = check %>%
  filter(grepl("out of state", result)) %>%
  mutate(stringdist = stringdist(city, clean_city_zip, "jw"))

oos = check %>%
  filter(!(zip %in% il_zips$zip))%>%
  mutate(stringdist = stringdist(city, clean_city_zip, "jw"),
         clean2_zip = clean_cook_towns(city, zip),
         clan2_no_zip = clean_cook_towns(city)
         )
