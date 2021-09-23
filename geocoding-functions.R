
#### Required packages ####

library(magrittr)
library(jsonlite)
library(keyring)


#URL for internal geocoder is private -- use key_set to store once per computer
key_set("geocoder-url")


#### Functions ####
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



#### USE EXAMPLE ####

#Note: functions above are not vectorized so if using them in the tidyverse, they must be used with either rowwise() or map() functions
# using purrr map functions seems faster in speed tests and is cleaner code

test_address <- read_csv("https://datacatalog.cookcountyil.gov/resource/ybvh-5bzv.csv") %>%
  select(-url) %>%
  mutate(location_1 = trimws(gsub("\\s*\\([^\\)]+\\)", "", location_1))) 

#multiple field example
test_call <- test_address %>%
  mutate(result = pmap(list(street = street_address, city = municipality, zip = zip_code), 
                       .f = geocode_address)) %>%
  #note: if address fields have same names as arguments: pmap(., .f=geocode_address) will work
  unnest(result) %>%
  select(-result)

#single field example
test_call_sl <- test_address %>%
  mutate(result = map(location_1, geocode_address_sl)) %>%
  unnest(result) %>%
  select(-result)


