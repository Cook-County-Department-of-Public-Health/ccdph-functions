
#### Required packages ####

library(magrittr)
library(jsonlite)
library(keyring)
library(httr)


#URLs for internal geocoders are private -- use key_set to store once per computer
#key_set("geocoder-url")
#key_set("batch-geocoder-url")


#### Single-Address Geocoder Functions ####
#default CRS for returned coordinates is state plane; use the wkid argument to adjust as needed

#address stored in separate fields
geocode_address <- function(street, city, zip, wkid = 3435) {
  
  geocoder_service <- paste0(key_get("geocoder-url"), "rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates")
  geocoder_options <- "&SingleLine=&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, 
                          "?Street=", street, "&City=", city, "&ZIP=", zip, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call), flatten = T) %>% .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(
      returned_result %>%
        select(match_address = attributes.Match_addr,
               score,
               x = location.x,
               y = location.y,
               address_type = attributes.Addr_type,
               residence_city = attributes.User_fld)
      )
  }
  
}

#address stored in single field
geocode_address_sl<- function(address, wkid = 3435) {
  
  geocoder_service <- paste0(key_get("geocoder-url"), "rest/services/AddressLocator/CookAddressComposite/GeocodeServer/findAddressCandidates?Street=&City=&SingleLine=")
  geocoder_options <- "&outFields=*&maxLocations=1&matchOutOfRange=false&langCode=&locationType=&sourceCountry=&category=&location=&distance=&searchExtent=&magicKey=&f=pjson"
  
  geocoder_call <- paste0(geocoder_service, address, "&outSR=", wkid,
                          geocoder_options)
  
  returned_result <- fromJSON(file(geocoder_call), flatten = T) %>% 
    .$candidates
  
  if (length(returned_result) == 0) {
    return(NA)
  } else {
    return(
      returned_result %>% 
        select(match_address = attributes.Match_addr,
               score,
               x = location.x,
               y = location.y,
               address_type = attributes.Addr_type,
               residence_city = attributes.User_fld))
  }
  
}



#### Single Address Use Example ####

# #Note: functions above are not vectorized so if using them in the tidyverse, they must be used with either rowwise() or map() functions
# # using purrr map functions seems faster in speed tests and is cleaner code
# 
# test_address <- read_csv("https://datacatalog.cookcountyil.gov/resource/ybvh-5bzv.csv") %>%
#   select(-url) %>%
#   mutate(location_1 = trimws(gsub("\\s*\\([^\\)]+\\)", "", location_1))) 
# 
# #multiple field example
# test_call <- test_address %>%
#   mutate(result = pmap(list(street = street_address, city = municipality, zip = zip_code), 
#                        .f = geocode_address)) %>%
#   #note: if address fields have same names as arguments: pmap(., .f=geocode_address) will work
#   unnest(result) %>%
#   select(-result)
# 
# #single field example
# test_call_sl <- test_address %>%
#   mutate(result = map(location_1, geocode_address_sl)) %>%
#   unnest(result) %>%
#   select(-result)



#### Batch Geocoder Function ####

#Batch geocoder is a little clunkier to use but much faster; use when you have a large number of address
#For best results, clean all address fields before using (e.g. remove special characters)
#Only use with addresses stored in separate fields and when both city and zip are available
#Batch geocoder has a limit of 5000 addresses, see code example for splitting datasets

batch_geocode <-function(dataset, id_field, street, city, zip) {
  
  # extract address fields from dataset and rename to geocoder expected attributes
  address_fields <- dataset %>%
    select(ADDRESS = {{ street }}, CITY = {{ city }}, ZIP = {{ zip }}, OBJECTID = {{id_field}}) %>%
    drop_na()
  
  # create input json for POST request
  addresses_json <- jsonlite::toJSON(list(records=address_fields),flatten = T)
  addresses_text <- addresses_json %>% 
    str_replace_all('\\{\\"ADDRESS\\"', '\\{\\"attributes\\":\\{\"ADDRESS\\"') %>% 
    str_replace_all('\\},\\{\\"attributes\\"','\\}\\},\\{\\"attributes\\"') %>%
    str_replace_all('\\}\\]\\}','\\}\\}\\]\\}')
  
  
  # send POST request
  geocoder_service <- key_get("batch-geocoder-url")
  addresses_json_rev <- rjson::fromJSON(addresses_text)
  addresses_json_rev <- jsonlite::toJSON(addresses_json_rev, flatten=TRUE, auto_unbox = TRUE)
  request_geo <- POST(url = geocoder_service,
                      body = list(addresses=addresses_json_rev,f="json"),
                      encode="form")
  result_json <- content(request_geo,"parsed","application/json")
  
  # format returned content
  result_df <- data.frame()
  for (i in seq_len(length(result_json$locations))){
    d <- with(result_json$locations[[i]], {data.frame(OBJECTID = attributes$ResultID,
                                                      #X = as.numeric(location$x),
                                                      #Y = as.numeric(location$y),
                                                      X = attributes$X,
                                                      Y = attributes$Y,
                                                      score = score, 
                                                      status = attributes$Status,
                                                      address_match = attributes$Match_addr,
                                                      residence_city = attributes$User_fld)})
    result_df <- rbind(result_df, d)
  }
  
  #re-join geocoding results to input dataset
  join_index <- enquo(id_field)
  by <- set_names("OBJECTID", quo_name(join_index))
  final_result <- dataset %>%
    left_join(result_df, by = by)
  
}


#### Batch Use Example ####

# geocoded_results <- dataset_to_be_geocoded %>% 
#   group_by(row_number() %/% 5000) %>%   #split dataset into list of datasets under batch geocoder limit
#   group_map(~batch_geocode(.x, patient_id, address, city, zip_code)) %>%   #apply function to each item of list
#   bind_rows()  #bind geocoded mini datasets back into one large dataset
