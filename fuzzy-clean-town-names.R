library(stringr)
library(stringdist)
library(magrittr)
library(tidyverse)

#load data
il_cities = read_csv("data/illinois-cities.csv")
city_zips = read_csv("data/cook-zipcodes-and-cities.csv")
chi_areas = read_csv("data/chicago-neighborhoods.csv")
chi_areas = c(chi_areas$neighborhood, chi_areas$community_area) %>% unique() 
chi_areas = chi_areas[!grepl("\\,", chi_areas)]
il_zips = read_csv("data/illinois-zip-codes.csv")

find_closest_match = function(name, possible_matches, threshold = 0.25, method = "jw"){
  #function used within fuzzy_clean_cook_town_helper to find best fuzzy match for town names
  
  matches = stringdist(tolower(name), tolower(possible_matches), method = method)
  
  best = which.min(matches)
  
  if(min(matches) <= threshold){
    return(possible_matches[best])
  }else{
    return(NA_character_)
  }
}


fuzzy_clean_cook_town_helper = function(town, zip_code = NULL){
  #function to clean one town with fuzzy matching
  
  #if missing, return NA
  if(is.na(town)){
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
    gsub("\\Chgo\\b|\\Chg\\b|\\Cgo\\b|\\Chi\\b", "Chicago", .) %>%
    gsub("Illinois|\\Il\\b", "", .) %>%
    gsub("\\Parque\\b", "Park", .) %>%
    gsub("Cc Hills", "Country Club Hills", .)
  
  
  #check if blank/null
  if(town %in% c("Null", "Illinois", "Il", "Needs Updating", "", "Cook",
                 "Unknown", "Homeless", "Cook County", "Lake", "Usa",
                 "N", "N/A", "Illinois City", "N A")){
    return(NA_character_)
  }
  
  

  
  #check if real town name
  if(town %in% il_cities$city){
    return(town)
  }
  
  
  #check if Chicago neighborhood/community area (unless neighborhood name is also a town in IL)
  if(town %in% setdiff(chi_areas, il_cities$city)){
    return("Chicago")
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
  
  #if zipcode provided and is not in IL- stricter matching
  if(!is.null(zip_code) && !is.na(zip_code) && !(zip_code %in% il_zips$zip)){
    return(find_closest_match(town, il_cities$city, threshold =  0.1))
  }
  
  #try matching with cook towns
  match = find_closest_match(town, il_cities$city[il_cities$in_cook], threshold = 0.25)
  if(!is.na(match)){
    return(match)
  }
  
  #if no match in cook towns and zipcode is only in one city, return that city
  if(!is.null(zip_code) && !is.na(zip_code)){
    zip_towns = city_zips %>%
      filter(zip == zip_code) %>%
      pull(city)    
    if(length(zip_towns) == 1){
      return(zip_towns)
    }
  }
  
  #try matching with all il towns
  match = find_closest_match(town, il_cities$city, threshold = 0.2)
  if(!is.na(match)){
    return(match)
  }
  
  return(NA_character_)
  
  
}

fuzzy_clean_cook_towns = function(towns, zips = NULL){
  #Clean names for multiple towns with fuzzy matching (or just one)
  
  out = character()
  for(i in 1:length(towns)){
    town = towns[i]
    zip_code = zips[i]
    
    clean = fuzzy_clean_cook_town_helper(town, zip_code)
    out = c(out, clean)
  }
  return(out)
}
