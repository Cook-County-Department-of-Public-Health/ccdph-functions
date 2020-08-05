

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
  
  
  #move La Grange Highlands to La Grange
  towns = gsub("La Grange Highlands", "La Grange", towns)
  
  return(towns)
}