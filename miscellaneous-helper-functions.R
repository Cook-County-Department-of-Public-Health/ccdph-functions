
library(tidyverse)
library(magrittr)
library(lubridate)

#Function to calculate start date for MMWR week for a date
library(MMWRweek)

MMWRweekStart = function(date){
  year = MMWRweek(date)[[1]]
  week = MMWRweek(date)[[2]]
  return(MMWRweek2Date(year, week))
}

MMWRweekEnd = function(date){
  MMWRweekStart(date) + 6
}

#Function to censor data
suppress_count <- function(number_var, censor_threshold = 5) { 
  
  number_var = ifelse(number_var > 0 & number_var < censor_threshold, NA, number_var) 
  
  }


#function to calculate rates
rate <- function(num, pop, pop_denom, decimals = 0) { 
  
  round(num/pop*pop_denom, decimals) 
  
  }


#Clean common misspellings and inconsistencies in sub-Cook town names from INEDSS records
clean_towns_INEDDS <- function(towns){
  
  #trim leading and following whitespace
  towns = trimws(towns)
  
  #correct common shorthands
  towns = gsub("Hts", "Heights", towns)
  towns = gsub("Hgts", "Heights", towns)
  towns = gsub("Vlg", "Village", towns)
  towns = gsub("Pk", "Park", towns)
  towns = gsub("Mt", "Mount", towns)
  towns = gsub("Spgs", "Springs", towns)
  
  #correct observed typos
  towns = gsub("Bridge View", "Bridgeview", towns)
  towns = gsub("Mccook", "McCook", towns)
  towns = gsub("McCook", "McCook", towns)
  towns = gsub("Mc Cook", "McCook", towns)
  towns = gsub("Matttenson", "Matteson", towns)
  towns = gsub("Argo", "Summit", towns)
  towns = gsub("Summit Argo", "Summit", towns)
  towns = gsub("Summit Summit", "Summit", towns)
  towns = gsub("Chicgo", "Chicago", towns)
  towns = gsub("Chcago", "Chicago", towns)
  towns = gsub("Chicsgo", "Chicago", towns)
  towns = gsub("Chiago", "Chicago", towns)
  towns = gsub("Chigago", "Chicago", towns)
  towns = gsub("Chocago", "Chicago", towns)
  towns = gsub("Desplaines", "Des Plaines", towns)
  towns = gsub("Markjam", "Markham", towns)
  towns = gsub("Matheson", "Matteson", towns)
  towns = gsub("Barrinton", "Barrington", towns)
  towns = gsub("Hrightds", "Heights", towns)
  towns = gsub("Bellweood", "Bellwood", towns)
  towns = gsub("Does Plaines", "Des Plaines", towns)
  towns = gsub("(,|) Il", "", towns)
  towns = gsub("Hangover Park", "Hanover Park", towns)
  towns = gsub("Malrose Park", "Melrose Park", towns)
  towns = gsub("Melrose Rark", "Melrose Park", towns)
  towns = gsub("Merlose Park", "Melrose Park", towns)
  towns = gsub("River Groove", "River Grove", towns)
  towns = gsub("Schaumborg", "Schaumburg", towns)
  towns = gsub("Summit Argo", "Summit", towns)
  towns = gsub("\\.", "", towns)
  towns = gsub("Cicerol1", "Cicero", towns)
  towns = gsub("Schuanburg", "Schaumburg", towns)
  towns = gsub("Henover", "Hanover", towns)
  towns = gsub("Ciscero", "Cicero", towns)
  towns = gsub("Eik Grove Village", "Elk Grove Village", towns)
  towns = gsub("  ", " ", towns)
  towns = gsub("Harveh", "Harvey", towns)
  towns = gsub("Harwoord Heights|Hradwood Heights", "Harwood Heights", towns)
  towns = gsub("Oawlawn|Oakalwn", "Oak Lawn", towns)
  towns = gsub("Olumpia Fields", "Olympia Fields", towns)
  towns = gsub("Sauk Chicago Heights|S Chicago Heights", "South Chicago Heights", towns)
  towns = gsub("Wilimwnte", "Wilmette", towns)
  towns = gsub("Polas Park", "Palos Park", towns)
  towns = gsub("Palos Holls", "Palos Hills", towns)
  towns = gsub("Des Paines", "Des Plaines", towns)
  towns = gsub("Elmwood Parke", "Elmwood Park", towns)
  towns = gsub("  ", " ", towns)
  towns = gsub("Mrkham", "Markham", towns)
  towns = gsub("La Grarge", "La Grange", towns)
  towns = gsub(",", "", towns)
  towns = gsub("Lansaing", "Lansing", towns)
  towns = gsub("Flossmour", "Flossmoor", towns)
  towns = gsub("Buffulo Grove", "Buffalo Grove", towns)
  towns = gsub("Counting Club Hills", "Country Club Hills", towns)
  towns = gsub("Country Clb Hls", "Country Club Hills", towns)
  towns = gsub("Eveergreen Park", "Evergreen Park", towns)
  towns = gsub("Forest Park60130", "Forest Park", towns)
  towns = gsub("Schriller Park", "Schiller Park", towns)
  towns = gsub("Harwood Heishts", "Harwood Heights", towns)
  towns = gsub("Harwoods Heights", "Harwood Heights", towns)
  towns = gsub("Milothian", "Midlothian", towns)
  towns = gsub("Mount Prosrect", "Mount Prospect", towns)
  towns = gsub("Oak Foest", "Oak Forest", towns)
  towns = gsub("Orland Aprk", "Orland Park", towns)
  towns = gsub("Whjeeling", "Wheeling", towns)
  towns = gsub("Winnnetka", "Winnetka", towns)
  towns = gsub("S Chiacgo Heights", "South Chiacgo Heights", towns)
  towns = gsub("Arlington Heighra", "Arlington Heights", towns)
  towns = gsub("Berwnyn|Brwyn|Berwyn I", "Berwyn", towns)
  towns = gsub("Blue Islane|Blue Island D", "Blue Island", towns)
  towns = gsub("Chicagi", "Chicago", towns)
  towns = gsub("Chgo", "Chicago", towns)
  towns = gsub("Cicerocicero", "Cicero", towns)
  towns = gsub("\\`", "", towns)
  towns = gsub("Despplaines", "Des Plaines", towns)
  towns = gsub("E G Village|Elk Grv Vkg", "Elk Grove Village", towns)
  towns = gsub("Elm Wood Park|Elmowood Park", "Elmwood Park", towns)
  towns = gsub("Hickory Hils", "Hickory Hills", towns)
  towns = gsub("Hillisde", "Hillside", towns)
  towns = gsub("Indian Herd Patk", "Indian Head Park", towns)
  towns = gsub("Ind Head Park", "Indian Head Park", towns)
  towns = gsub("Mel Rose Park|Melrode Park", "Melrose Park", towns)
  towns = gsub("Morto Grove|Morton Grv|Mortongrove", "Morton Grove", towns)
  towns = gsub("Mount Propsect|Mount Prospct|Mount Prowpect", "Mount Prospect", towns)
  towns = gsub("Foresst|Foreost", "Forest", towns)
  towns = gsub("Orlaand|Oralnd", "Orland", towns)
  towns = gsub("Orland Patk", "Orland Park", towns)
  towns = gsub("Sachaumburg", "Schaumburg", towns)
  towns = gsub("Schaumberg", "Schaumburg", towns)
  towns = gsub("Schiiler", "Schiller", towns)
  towns = gsub("Tinl Park|Tinle Park|Tinley Parl", "Tinley Park", towns)
  towns = gsub("Wheeliing|Wheeliong", "Wheeling", towns)
  towns = gsub("Cntry", "Country", towns)
  towns = gsub("Elk Grove$", "Elk Grove Village", towns)
  towns = gsub("Villag$", "Village", towns)
  towns = gsub("Lagrange|LaGrange", "La Grange", towns)
  towns = gsub("Oaklawn", "Oak Lawn", towns)
  towns = gsub("So Chicago Heights", "South Chicago Heights", towns)
  towns = gsub("Crestwook", "Crestwood", towns)
  towns = gsub("Palos\\?heights", "Palos Heights", towns)
  towns = gsub("N Riverside", "North Riverside", towns)
  towns = gsub("Western Sprgs", "Western Springs", towns)
  towns = gsub("Parkridge", "Park Ridge", towns)
  towns = gsub("^Hoffman Est$|^Hoffam Estate$", "Hoffman Estates", towns)
  towns = gsub("North Lake", "Northlake", towns)
  towns = gsub("Hazelcrest", "Hazel Crest", towns)
  towns = gsub("Rolling Mdws", "Rolling Meadows", towns)
  
  
  
  
  #move La Grange Highlands to La Grange
  towns = gsub("La Grange Highlands", "La Grange", towns)
  
  return(towns)
}

#named above function with a spelling mistake, used for more than just I-NEDSS now, duplicate function but 
#also keep original so scripts don't fail
clean_cook_town_names = clean_towns_INEDDS

#steps taken to clean formatted salesforce reports
clean_salesforce_report = function(report, formatted = T){
  out = report %>%
    rename_all(~str_replace_all( ., " |\\/|\\-|\\?|\\:|\\(|\\)|\\'", "" )) %>%
    mutate(across(contains("Date"), parse_date_time, orders = c("mdy HM p", "mdy")),
           across(contains("City"), clean_towns_INEDDS)) 
  
  if(formatted){
    out %<>% select(-2)
  }
  
  return(out)
}

#clean addresses so matching is easier
clean_addresses = function(address){
  address = toupper(address)
  address = gsub("\\.", "", address)
  
  address = gsub("\\bN\\b", "NORTH", address)
  address = gsub("\\bS\\b", "SOUTH", address)
  address = gsub("\\bE\\b", "EAST", address)
  address = gsub("\\bW\\b", "WEST", address)
  address = gsub("\\bSW\\b", "SOUTHWEST", address)
  address = gsub("\\bNW\\b", "NORTHWEST", address)
  address = gsub("\\bSE\\b", "SOUTEAST", address)
  address = gsub("\\bNE\\b", "NORTHEAST", address)
  
  address = gsub("\\bHWY\\b", "HIGHWAY", address)
  address = gsub("\\bRD\\b", "ROAD", address)
  address = gsub("\\bST\\b", "STREET", address)
  address = gsub("\\bAVE\\b", "AVENUE", address)
  address = gsub("\\bAV\\b", "AVENUE", address)
  address = gsub("\\bBLVD\\b", "BOULEVARD", address)
  address = gsub("\\bDR\\b", "DRIVE", address)
  address = gsub("\\bLN\\b", "LANE", address)
  
  address = gsub(" (APT|#|ROOM|RM|UNIT)(| )\\d+", "", address)
  
  return(address)
}


#read in a file matching a pattern

#file for getting most recent data for report type
file_name = function(pattern, path = downloads_path, extension = "xlsx", num = 1){
  list.files(path, pattern = paste0(pattern,".*", extension, "$"), full.names = T) %>%
    cbind(file.mtime(.)) %>%
    set_colnames(c("file", "time")) %>%
    as_tibble() %>%
    arrange(time) %>%
    pull(file) %>%
    tail(num)
}


#create age decade variable
age_decade = function(age){
  factor(case_when(age < 20 ~ "< 20",
                               age > 79 ~ "> 80",
                               age < 80 & age > 19 ~ paste0(substr(age,1,1),"0s")),
                     levels = c("< 20", "20s", "30s", "40s", "50s", "60s", "70s", "> 80"), ordered = TRUE)
}


#use variable in interval notation format (left closed, ordered factor) to create labels suitable for display
make_pretty_intervals <- function(interval_var){
  
  intervals_calc <- levels(interval_var)
  interval_starts <- map_dbl(intervals_calc, ~round(as.numeric(gsub("^[\\[\\(](.+),.*", "\\1", .x)), 1))
  
  pretty_intervals <- sapply(1:(length(interval_starts)-1), function(i){
    return(paste(interval_starts[i], "-", interval_starts[i+1] - .1))
  }) %>%
    c(paste("More than", interval_starts[length(interval_starts)])) 
  
  pretty_intervals <- factor(pretty_intervals,ordered = T, levels = pretty_intervals)
  
  return(pretty_intervals[interval_var])
  
}