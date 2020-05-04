library(RSelenium)
library(keyring)
library(purrr)


#=================LOGGING INTO THE I-NEDSS PORTAL======================#

login_inedss = function(){
  
  #Navigating to log-in page
  rD$navigate("https://dph.partner.illinois.gov/my.policy")
  
  #Pause to give page time to load
  Sys.sleep(5)
  
  #Check for cookies error
  login_error <- try(rD$findElement("css", "#newSessionDIV > a:nth-child(1)"))
  if (class(login_error) != "try-error") {login_error$clickElement()}
  
  #Pause to give page time to load
  Sys.sleep(5)
  
  #Clicking link to access log-in screen
  rD$findElement("css", ".interaction_table_text_cell > a:nth-child(1)")$clickElement()
  
  #Pausing execution to give time to log in and load page
  Sys.sleep(5)
  
  #Enter credentials and log in
  rD$findElement(using = "css", value = "#input_1")$sendKeysToElement(list(key_get("idph_username"), key = "tab", key_get("idph_portal")))
  rD$findElement("css", "input[value = \"Logon\"]")$clickElement()
  
  #Pausing execution to give time to log in and load page
  Sys.sleep(10)
  
  #Mousing over applications button
  rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a/span/span')$mouseMoveToLocation()
  
  #Finding production apps button
  rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a')$clickElement()
  
  #Finding INEDSS buttons  -- IMPORTANT: XPATH WILL BE DIFFERENT DEPENDING ON APPS USER HAS
  ifVisiblethenClick('//*[@id="column"]/table[5]/tbody/tr/td[2]/a', selectorType = "xpath") 
  
  #Pausing execution to give time to load page
  Sys.sleep(10)
  
  #Switching focus to INEDSS tab   
  windows <- rD$getWindowHandles()   
  rD$switchToWindow(windows[[2]])
  
  #Clicking login button
  rD$findElement(using = "css", value = "input[name = \"login\"]")$clickElement()
  
  #Pausing execution to give time to load page
  Sys.sleep(5)
}

#============I-NEDSS FUNCTIONS==========#
#Return name of current I-NEDSS page
current_page = function(){
  rD$findElement(using = "css", value = ".pageDesc")$getElementText()[[1]] 
}

#Click on Dash Board if not already there
click_dashboard = function(){
  if(current_page() != "Dash Board"){
    click("th.dataTableNotSelected:nth-child(1) > a:nth-child(1)")
    
  }
}

#Wait for a given page to load, return false if not there by end of waitTime
wait_page = function(pageName, waitTime = 10){
  #Give time to load
  c = 0
  while(current_page()!=pageName & c < waitTime){
    Sys.sleep(1)
    c = c+1
    if(c == waitTime-1){
      message("Not on page")
      return(FALSE)
    }
  }
  return(TRUE)
}

#Search for a state case number and go to their case summary
search_scn = function(caseNumber){
  click_dashboard()
  if(wait_page("Dash Board") == TRUE){
    enter_text("#idNumber", caseNumber)
    click("input[name = \"Search\"]")
  }
  else{
    message("Not on Dash Board")
  }
}

#Assign an investigator from inside Case Summary page
assign_investigator = function(caseNumber, investigator){
  
  search_scn(caseNumber)
  wait_page("Case Summary")
  
  #Click Assign Investigator
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1)")
  
  #Investigators dropdown menu
  investigatorsMenu = rD$findElement(using = "css", value = "#investigator")
  
  #Make sure no one assigned yet
  first = investigatorsMenu$findChildElements("css", "option")[[1]]
  if(first$isElementSelected() == FALSE){
    message(paste(caseNumber, "already assigned"))
    click("input[name = \"cancel\"]")
    return()
  }
  
  #Assign investigator
  investigatorsList = investigatorsMenu$selectTag()$text 
  investigatorIndex = which(investigatorsList == investigator)[[1]]
  click(paste0("#investigator > option:nth-child(", investigatorIndex,")"))
  
  #Confirm on correct case
  if(get_text("#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(6) > td:nth-child(1)")
     != caseNumber){
    #If case numbers don't match, cancel and give message
    message("Not in correct case")
    click("input[name = \"cancel\"]")
  }
  
  #Save
  click("input[name = \"save\"]")
  message(paste(caseNumber, "assigned to", investigator))
  
  #Close
  #click("input[value = \"Close\"]") #takes too long, don't think it's necessary
}
