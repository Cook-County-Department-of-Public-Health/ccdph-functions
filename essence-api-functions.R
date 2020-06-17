library(jsonlite)
library(httr)
library(magrittr)
library(dplyr)
library(keyring)
library(tidyverse)
library(lubridate)

#function to get data from NSSP 
NSSP_GET = function(url){
  #download NSSP API url, get username and password, and error check for incorrect username/password
  
  #enter NSSP username and then password if not in system- only has to be done once per computer
  if(!("NSSP_username" %in% key_list()$service)|!("NSSP_password" %in% key_list()$service)){
    message("Enter NSSP username")
    key_set("NSSP_username")
    message("Enter NSSP password")
    key_set("NSSP_password")
  }
  
  #authenticate and download ESSENCE API url
  out = GET(url,
            authenticate(key_get("NSSP_username"), key_get("NSSP_password")))
  
  #ask for username and password again if exit status is 401
  if(out$status_code == 401){ 
    message("Invalid user credentials, try again\n")
    key_delete("NSSP_password")
    NSSP_GET(url)
  }
  
  else{return(out)}
}

#function to call NSSP get function and format output
nssp_get_table = function(url){
  #download NSSP API url as a table (transformed from JSON) 
  if(grepl("dataDetails", url, ignore.case = T)){
    if(grepl("csv\\?",url, ignore.case = T)){
      table = content(NSSP_GET(url), as = "text", encoding = "UTF-8") %>% read_csv() 
    }
    else{
      table = content(NSSP_GET(url), as = "text", encoding = "UTF-8") %>%
        fromJSON %>%
        extract2("dataDetails")
    }
  }
  else{
    table =  NSSP_GET(url) %>% content() #extract table contents
    if(length(table) == 1){ #sometimes all in single upper level list
      table = table[[1]]
    }
    else if(length(table) == 2 & length(table[[1]] == 1)){ #time series data formatted like this
      table = table[[2]]
    }
    table = lapply(table, function(x){return(x)}) %>% bind_rows() #format to dataframe structure
  }
  return(table)
}

#Calls NSSP get_table but selects only needed column and can add a name column for future rbinding
nssp_get_time_series = function(url, columns = c("date", "count"), name = NULL, startDate = NULL, endDate = NULL){
  url = essence_change_api_date(url, startDate, endDate)
  
  table = nssp_get_table(url) %>%
    select(columns)
  
  if("date" %in% columns){
    table %<>% mutate(date = as.Date(date))
  }
  
  if(!is.null(name)){
    table %<>% mutate(name = name)
  }
  
  return(table)
}

#Format date for essence API based on today's date or given date
essence_api_date = function(date = NULL, days_ago = NULL){
  if(!is.null(date)){
    if(class(date) == "character"){
      date = as.Date(date)
    }
    return(format(date, "%d%b%Y"))
  }
  
  else if (!is.null(days_ago)){
    return(format(Sys.Date() - days_ago, "%d%b%Y"))
  }
  
  else{
    return(format(Sys.Date(), "%d%b%Y"))
  }
}

#Change start and end date in url
essence_change_api_date = function(url, start = NULL, end = NULL){
  if(!is.null(end)){
    url = gsub("endDate=.*?&", paste0("endDate=",end, "&"), url)
  }
  if(!is.null(start)){
    url = gsub("startDate=.*?&", paste0("startDate=",start, "&"), url)
  }
  return(url)
}

#Add a race/ethnicity column to a data table pulled that has both Race_flat and Ethnicity_flat colums
essence_add_race_eth = function(df){
  df %>%
    mutate(
      race_temp = gsub(";UNK|;PHC1175|;NR", "", Race_flat),
      
      raceEth = case_when(
        grepl(";2135-2;", Ethnicity_flat) ~ "Hispanic/Latino",
        race_temp == ";2106-3;" ~ "Non-Hispanic White",
        race_temp == ";2054-5;" ~ "Non-Hispanic Black",
        race_temp == ";2131-1;" ~ "Non-Hispanic Other Race",
        race_temp == ";2028-9;" ~ "Non-Hispanic Asian",
        race_temp == ";1002-5;" ~ "Non-Hispanic American Indian or Alaska Native",
        race_temp == ";2076-8;" ~ "Non-Hispanic Native Hawaiian or Other Pacific Islander",
        race_temp == ";" ~ "Unknown",
        grepl(";.*;.*;", race_temp) ~ "Non-Hispanic Multiple Races",
        TRUE ~ "Unknown"
      )
      
    ) %>%
    select(-race_temp)
  
  
}
