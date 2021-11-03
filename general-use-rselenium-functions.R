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
start_server = function(instance = 1){
  
  
  #Setting Firefox to avoid download type
  firefoxProfile <- makeFirefoxProfile(list(browser.helperApps.neverAsk.saveToDisk = "application/comma-separated-values ,text/csv"))
  
  #Open selenium session
  remDr <<- rsDriver(browser = "firefox", extraCapabilities = firefoxProfile, port = as.integer(4566 + instance))
  
  #Extract the client for navigation
  rD <<- remDr[['client']]
  
}

#End server session
stop_server = function(need_java_kill = F){
  
  remDr$server$stop() #stop server
  
  if (need_java_kill) {system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)}
  #open rselenium issue affecting some environments - lingering java process keeps port open
  #reference: https://github.com/ropensci/RSelenium/issues/228
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

#Find a specific (target) element from another's elements children
#Target element should be identified from the text displayed on the page
find_child_element <- function(element, child, target, selectorType = "css") {
  
  target_location <- rD$findElement(using = selectorType, element)$findChildElements(using = selectorType, child) %>%
    map_chr(., function(x) x$getElementText()[[1]]) %>%
    grepl(target, .) %>%
    which(. == TRUE)
  
  return(target_location)
  
}

#Select from drop down
select_drop_down <- function(element, selection, selectorType = "css") {
  
  #Find option
  optionChild <- find_child_element(element = element, child = "option", target = selection)
  
  #click option
  click(paste0(element, " > option:nth-child(", optionChild,")"))
  
}


#Same as select_drop_down but only executes if field isn't empty
select_drop_down_na <- function(element, selection, selectorType = "css", field = selection) {
  
  if (!is.na(field)) {
    #Find option
    optionChild <- find_child_element(element = element, child = "option", target = selection)

    #click option
    click(paste0(element, " > option:nth-child(", optionChild,")"))
  }
  
}


#See if dropdown menu has NA selected
dropdown_is_na = function(element, selectorType = "css"){
  dropdown = rD$findElement(using = selectorType, element)
  na_option_index = which(dropdown$selectTag()$text == "")[1]
  na_option = dropdown$findChildElements("css", "option")[[1]]
  return(na_option$isElementSelected()[[1]])
  
}


#Get text of an element 
#(put textbox = TRUE if looking for text written into textbox, textbox = FALSE if text written on page)
get_text = function(element, selectorType = "css", textbox = FALSE){
  if(textbox == FALSE){
    rD$findElement(using = selectorType, value = element)$getElementText()[[1]]
  }else{
    rD$findElement(using = selectorType, value = element)$getElementAttribute("value")[[1]]
  }
}

#translate name or value to css
value.is = function(value){
  paste0('input[value = \"', value, '\"]')
}
name.is = function(name){
  paste0('input[name = \"', name, '\"]')
}

#get vector of text for all elements of a class type
get_text_class = function(element, selectorType = "css"){
  if(!grepl("^\\.", element)){ #if the element doesn't start with a . it won't work as a class
    element = paste0(".", element)
  }
  
  rD$findElements(using = "css", element) %>%
    sapply(function(x){x$getElementText()[[1]]})
}


#Click a link with href name
click_href_link = function(href_link){
  val = paste0('//a[@href = "', href_link, '"]')
  link = rD$findElement(value = val)
  link$clickElement()
  
}
