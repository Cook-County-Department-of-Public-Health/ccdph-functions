library(RSelenium)
library(keyring)
library(purrr)

#=========================GENERAL USE FUNCTIONS=========================#
click <- function(element, selectorType = "css"){
  rD$findElement(using = selectorType, value = element)$clickElement()
  
}

#Wait to click an element until present on the screen, defaults to 2 minutes of trying
ifVisiblethenClick <- function(element, selectorType = "css", waitTime = 120) {
  
  checkElement <- try(rD$findElement(using = selectorType, value = element))
  checkElementWait <- 0
  
  while(class(checkElement) == "try-error" & checkElementWait < waitTime) {
    
    Sys.sleep(1)
    
    checkElementWait <- checkElementWait + 1
    
    checkElement <- try(rD$findElement(using = selectorType, value = element))
    
  }
  
  rD$findElement(using = selectorType, value = element)$clickElement()
  
}


#Wait to move to next step in script until visible
isPageLoaded<- function(element, selectorType = "css", waitTime = 120) {
  
  checkElement <- try(rD$findElement(using = selectorType, value = element))
  checkElementWait <- 0
  
  while(class(checkElement) == "try-error" & checkElementWait < waitTime) {
    
    Sys.sleep(1)
    
    checkElementWait <- checkElementWait + 1
    
    checkElement <- try(rD$findElement(using = selectorType, value = element))
    
  }
  
  return(checkElementWait)
  
}


#Accept an alert with wait time; default is 3 seconds
acceptAlertwithWait <- function(wait = 3) {
  
  Sys.sleep(wait)
  rD$acceptAlert()
  Sys.sleep(wait)
  
}

#Start firefox server session, assign remDr and rD to global environment
start_server = function(){
  #Open selenium session
  remDr <<- rsDriver(browser = "firefox")
  
  #Extract the client for navigation
  rD <<- remDr[['client']]
  
}

#End server session
stop_server = function(){
  remDr$server$stop() 
}

#enter text into a text box- text can be a single argument or a vector
enter_text = function(element, text, selectorType = "css"){
  rD$findElement(using = selectorType, value = element)$sendKeysToElement(as.list(text))
  
}

#same as enter text but only executes if the field pulling data isn't empty
enter_text_na = function(element, text, selectorType = "css", field = text){
  
  if (!is.na(field)) {
    rD$findElement(using = selectorType, value = element)$sendKeysToElement(as.list(text))
  }
  
}

#Select from drop down
select_drop_down <- function(element, selection, selectorType = "css") {
  
  #Find unknown option
  optionChild <- map_chr(rD$findElement(using = selectorType, value = element)$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
    grepl(selection, .) %>%
    which(. == TRUE)
  
  #click option
  click(paste0(element, " > option:nth-child(", optionChild,")"))
  
}


#Same as select_drop_down but only executes if field isn't empty
select_drop_down_na <- function(element, selection, selectorType = "css", field = selection) {
  
  if (!is.na(field)) {
    #Find unknown option
    optionChild <- map_chr(rD$findElement(using = selectorType, value = element)$findChildElements("css", "option"), function(x) x$getElementText()[[1]]) %>%
      grepl(selection, .) %>%
      which(. == TRUE)
    
    #click option
    click(paste0(element, " > option:nth-child(", optionChild,")"))
  }
  
}


#Get text of an element
get_text = function(element, selectorType = "css"){
  rD$findElement(using = selectorType, value = element)$getElementText()[[1]]
}

