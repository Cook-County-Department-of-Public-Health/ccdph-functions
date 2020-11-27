
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


#Clean common misspellings and inconsistencies in sub-Cook town names from INEDSS records
clean_towns_INEDDS <- function(towns){
  
  #trim leading and following whitespace
  towns = trimws(towns)
  
  #correct common shorthands
  towns = gsub("Hts", "Heights", towns)
  towns = gsub("Vlg", "Village", towns)
  towns = gsub("Pk", "Park", towns)
  towns = gsub("Mt", "Mount", towns)
  towns = gsub("Spgs", "Springs", towns)
  
  #correct observed typos
  towns = gsub("Bridge View", "Bridgeview", towns)
  towns = gsub("Mccook", "McCook", towns)
  towns = gsub("McCook", "McCook", towns)
  towns = gsub("Matttenson", "Matteson", towns)
  towns = gsub("Argo", "Summit", towns)
  towns = gsub("Summit Argo", "Summit", towns)
  towns = gsub("Summit Summit", "Summit", towns)
  towns = gsub("Chicgo", "Chicago", towns)
  towns = gsub("Chcago", "Chicago", towns)
  towns = gsub("Chicsgo", "Chicago", towns)
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
  
  
  #move La Grange Highlands to La Grange
  towns = gsub("La Grange Highlands", "La Grange", towns)
  
  return(towns)
}

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
  address = gsub("\\bSW\\b", "SOUTHWEST", address)
  address = gsub("\\bNW\\b", "NORTHWEST", address)
  address = gsub("\\bSE\\b", "SOUTEAST", address)
  address = gsub("\\bNE\\b", "NORTHEAST", address)
  
  address = gsub("\\bHWY\\b", "HIGHWAY", address)
  address = gsub("\\bRD ", "ROAD", address)
  address = gsub("\\bST\\b", "STREET", address)
  address = gsub("\\bAVE\\b", "AVENUE", address)
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