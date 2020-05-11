library(RSelenium)
library(keyring)
library(purrr)

source("general-use-rselenium-functions.R")

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

#==============SEARCHING FOR A NAME======================#

#Search for a case in Add/Search
search_name = function(first, last, sex, dob){
  enter_text("#first", first)
  enter_text("#last", last)
  enter_text("#sex", sex)
  enter_text("#dob", c(format(dob, "%m"),format(dob, "%d"), format(dob, "%Y")))
  Sys.sleep(1) #Adding a pause, seems to freeze a lot during search
  click("#bnameSearch")
}


#==============DETERMINE PATIENT MATCH======================#

determinePatientMatch <- function(threshold_low) {
  
  #Check if probability table exists by looking for highest probability value
  highestProb <- try(rD$findElement(using = "css", value = '#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(6) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1)'))
  
  #If not, return "no matches found"
  if (class(highestProb) == "try-error") {
    
    return(list(patientMatchFound = "no match", row = NA))  #no potential matches returned
    
  } 
  
  #If table does exist, extract highest value   
  highestProbVal <- as.numeric(str_remove(highestProb$getElementText()[[1]], "%"))
  
  #Execute steps depending on highest value
  #highest probability match less than established threshold for no match
  if (highestProbVal < threshold_low) {
    
    return(list(patientMatchFound = "no match", row = NA))  
    
  } 
  
  #100% match returned
  if (highestProbVal == 100) {
    
    return(list(patientMatchFound = "match", row = 3))    
    
  }
  
  #81%-99% match returned
  #Format case name for matching
  case_name <- paste(str_to_title(case$lastName), str_to_title(case$firstName), sep = ", ")
  #Remove middle inital if present
  case_name <- strsplit(case_name, " ")[[1]] %>%
    .[nchar(.) > 1] %>%
    paste(., collapse = " ")
  
  
  #Count potential matches in table
  patientMatchTable <- rD$findElement(using = "css", value = "#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(6)")  
  patientRows <- (length(patientMatchTable$findChildElements(using = "css", "tbody > tr")) - 2) / 2  #Subtract two for header trs and each patient has extra tr so divide by 2
  
  #Get nth-child positions of top 3 rows or fewer
  patientNthChild <- seq(from = 3, by = 2, length.out = ifelse(patientRows < 3, patientRows, 3))
  
  #Compare case name and DOB to potential matches
  for (i in patientNthChild) {
    
    match_name <- rD$findElement(using = "css", value = paste0("#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(6) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(2)"))$getElementText()[[1]]
    match_name <- strsplit(match_name, " ")[[1]] %>%
      .[nchar(.) > 1] %>%
      paste(., collapse = " ")
    
    match_DOB <- rD$findElement(using = "css", value = paste0("#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(6) > tbody:nth-child(1) > tr:nth-child(",i,") > td:nth-child(4)"))$getElementText()[[1]] %>%
      mdy()
    
    name_match <- any(grepl(case_name, match_name), grepl(match_name, case_name))
    dob_match <- match_DOB == case$dob
    
    if (all(name_match,dob_match)) {
      
      return(list(patientMatchFound = "match", row = i)) 
      
    } 
    
  }    
  
  return(list(patientMatchFound = "unclear", row = NA)) 
  
  
}



#==============DETERMINE CASE MATCH======================#
determine_case_match <- function(){
  
  #Give page time to load
  isPageLoaded(".indessTable")
  
  #Count number of case entries
  numCases <- length(rD$findElement(using = "css", value =".indessTable")$findChildElements(using = "css", "tbody > tr"))
  
  #Search for previous COVID case
  for (i in 2:numCases) {
    
    disease <- get_text(paste0(".indessTable > tbody:nth-child(1) > tr:nth-child(", i, ") > td:nth-child(1) > a:nth-child(1)"))
    
    if (disease == "Coronavirus Novel 2019") {
      return(i)
    }
    
  }
  
  #If no COVID case found
  return(NA)
  
}



#==============ENTER DEMOGRAPHICS======================#

enter_demographics <- function(){
  
  isPageLoaded("#first")
  
  #Name and DOB pre-filled from search
  
  #General note for drop-downs: enter text generally works and is faster than select_drop_down but prone to errors
  #Only use as a short cut if sending first letter and first letter of all options are unique
  
  #Enter sex
  enter_text_na("#sex", substr(case$sex,1,1))
  
  #Enter race and click add   
  enter_text_na("#availableRace", substr(case$race,1,1))
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(3) > p:nth-child(1) > input:nth-child(1)")
  
  #Enter ethnicity
  enter_text_na("#ethnic", substr(case$ethnicity,1,1))
  
  #Enter phone 1
  enter_text_na("#number1", c(substr(case$phone,1,3), substr(case$phone,5,7), substr(case$phone,9,12)), field = case$phone)
  
  #Enter phone 2
  enter_text_na("input[name = \"cellareaCode\"]", c(substr(case$phone2,1,3), substr(case$phone2,5,7), substr(case$phone2,9,12)), field = case$phone2)
  
  #Enter email
  enter_text_na("#email", case$email)
  
  #Enter address
  enter_text_na("#street1", case$address)
  enter_text_na("#street2", case$address2)
  enter_text_na("#city", case$city)
  
  enter_text_na("#zip", as.character(case$zip))
  select_drop_down_na("#county", paste0("^",case$county,"$"))
  
}


#==============SELECT DISEASE======================#

#Set function argument to TRUE when adding new person AND case, FALSE when only adding new case (elements differ, see below)
select_disease <- function(newperson){  
  
  #Disease table is an iframe, only becomes accessible when scrolled into view
  rD$findElement("css", "body")$sendKeysToElement(list(key = "end"))   #scroll to end of page
  Sys.sleep(1)
  diseaseList <- rD$findElement("css", "#DiseaseSerotypeFrame")
  
  #Switch into frame
  rD$switchToFrame(diseaseList)
  
  #Expand all disease options
  click("#container > div:nth-child(2) > form:nth-child(8) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > a:nth-child(1)")
  Sys.sleep(1)
  
  #Element ID differs whether new person or just new case
  covidID <- ifelse(newperson, 259, 253)
  
  #IDs are non-descript so make sure coded element matches coronavirus
  diseaseText <- get_text(paste0("#webfx-tree-object-", covidID, "-anchor"))
  if (diseaseText != "Coronavirus Novel 2019") {
    
    #Exit iframe
    rD$switchToFrame(NULL)
    
    #Return error code
    return("error")
  }
  
  #Select coronavirus
  click(paste0("#webfx-tree-object-", covidID, "-anchor"))
  
  #Exit iframe
  rD$switchToFrame(NULL)
  
  #Click "Save" as opposed to "Fast Track" - can change later if preferred
  click("input[name=\"save\"]")
  
  #Return
  return("success")
  
}

#==============ENTER LAB DATA======================#

enter_lab_data <- function(){
  
  #Click on lab section 
  click("fieldset.fieldsetNameBlock:nth-child(14) > legend:nth-child(1) > a:nth-child(2)")
  
  #Enter lab tests conducted
  enter_text("#TESTSDONElbl", defaults[defaults$Variable == "labTestsConducted", "Default_Value"])
  
  #Click to add specimen 
  click("input[name=\"addspecimen\"]")
  isPageLoaded("#specimenNumber")
  
  #Enter specimen data
  enter_text_na("#specimenNumber", case$specimenNumber)
  select_drop_down_na("#specimenSource", case$specimenSource)
  enter_text("#collectionDate", c(format(case$specimenCollectionDate, "%m"),format(case$specimenCollectionDate, "%d"), format(case$specimenCollectionDate, "%Y")))
  ##lab?
  
  #Enter ordering facility
  enter_text("input[name=\"orderingFacilityName\"]", case$orderingFacility)
  enter_text_na("input[name=\"orderingFacilityStreet1\"]", case$orderingFacilityAddress)
  enter_text_na("input[name=\"orderingFacilityStreet2\"]", case$orderingFacilityAddress2)
  enter_text_na("input[name=\"orderingFacilityCity\"]", case$orderingFacilityCity)
  select_drop_down_na("input[name=\"orderingFacilityState\"]", case$orderingFacilityState)
  enter_text_na("input[name=\"orderingFacilityZip\"]", as.character(case$orderingFacilityZip))
  enter_text_na("input[name=\"orderingFacility_areacode\"]", c(substr(case$orderingFacilityPhone,1,3), substr(case$orderingFacilityPhone,5,7), substr(case$orderingFacilityPhone,9,12)), field = case$orderingFacilityPhone)
  
  #Enter ordering provider
  enter_text_na("input[name=\"orderingProviderFirstName\"]", case$orderingProviderFirst)
  enter_text_na("input[name=\"orderingProviderLastName\"]", case$orderingProviderLast)
  enter_text_na("input[name=\"orderingProvider_areacode\"]", c(substr(case$orderingProviderPhone,1,3), substr(case$orderingProviderPhone,5,7), substr(case$orderingProviderPhone,9,12)), field = case$orderingProviderPhone)
  
  #Click to add lab result
  click("input[name=\"addresult\"]")
  isPageLoaded("#reportDate")
  
  #Enter lab result
  enter_text("#reportDate", c(format(case$labReportDate, "%m"),format(case$labReportDate, "%d"), format(case$labReportDate, "%Y")))
  select_drop_down("#testType", defaults[defaults$Variable == "testType", "Default_Value"])
  select_drop_down("#testMethod", defaults[defaults$Variable == "testMethod", "Default_Value"])
  click("#container > div:nth-child(4) > form:nth-child(5) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(8) > td:nth-child(1) > fieldset:nth-child(1) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > input:nth-child(1)")
  select_drop_down("#code", defaults[defaults$Variable == "testResult", "Default_Value"])
  enter_text("#comment", defaults[defaults$Variable == "testComment", "Default_Value"])
  
  #Save lab result
  click("input[name=\"save\"]")
  
  #Save lab section
  isPageLoaded("#TESTSDONElbl")
  click("input[name=\"save\"]")
  
  #Let case details page load
  isPageLoaded(".fullPageDescription")
  
}


#==============ENTER EPI DATA======================#

enter_epi_data <- function(){
  
  #Click into epi section
  click("fieldset.fieldsetNameBlock:nth-child(22) > legend:nth-child(1) > a:nth-child(2)")
  isPageLoaded("#case")
  
  #Enter case status
  select_drop_down("#case", defaults[defaults$Variable == "caseStatus", "Default_Value"])
  
  #Save
  click("input[name=\"save\"]")
  
  #Let case details page load
  isPageLoaded(".fullPageDescription")
  
}



#==============ENTER REPORTING SOURCE======================#

enter_reporter <- function(){
  
  #Click into reporting section
  click("fieldset.fieldsetNameBlock:nth-child(24) > legend:nth-child(1) > a:nth-child(2)")
  isPageLoaded("#report")
  
  #Enter reporting dates
  enter_text_na("#report", c(format(case$earliestReportDate, "%m"),format(case$earliestReportDate, "%d"), format(case$earliestReportDate, "%Y")), field = case$earliestReportDate)
  enter_text_na("#received", c(format(case$dateLHDReceived, "%m"),format(case$dateLHDReceived, "%d"), format(case$dateLHDReceived, "%Y")), field = case$dateLHDReceived)
  
  #Enter reporter
  enter_text_na("#first1", case$reporterFirst)
  enter_text_na("#last1", case$reporterLast)
  enter_text_na("#number", c(substr(case$reporterPhone,1,3), substr(case$reporterPhone,5,7), substr(case$reporterPhone,9,12)), field = case$reporterPhone)
  enter_text("#reporter", defaults[defaults$Variable == "reporterComment", "Default_Value"])
  
  #Enter reporting organization
  #Perform a search first to activate text box
  click("fieldset.fieldsetNameBlock:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(2) > a:nth-child(1)")
  isPageLoaded("#hospital")
  enter_text("#hospital", "zzzz")
  click("input[name = \"search\"]")
  click("input[name = \"cancel\"]")
  isPageLoaded("#otherReportingOrg")
  enter_text("#otherReportingOrg", case$reporterOrganizationName)
  
  #Save
  click("input[name=\"save\"]")
  
  #Let case details page load
  isPageLoaded(".fullPageDescription")
  
}


#==============ENTER NEW CASE======================#

#Group together other functions to enter new case (e.g. everything but demographics)
enter_new_case <- function() {
  
  #Open all case details
  isPageLoaded(".pageDesc")
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > a:nth-child(1)")
  
  #Enter onset date on General Illness screen if available
  if (!is.na(case$onset)) {
    
    #No sites sending as of now, write function to enter if becomes available
    
  }
  
  #Enter patient status on Clinical screen if available
  if (!is.na(case$patientStatus)) {
    
    #Come back and test on newer PIC files
    
  }
  
  #Enter laboratory data
  enter_lab_data()
  
  #Enter case status on Epi screen
  enter_epi_data()
  
  #Enter reporting source
  enter_reporter()
  
  #Close case details
  click("input[name=\"closebottom\"]")
  isPageLoaded(".pageDesc")
  
  #Entering case will automatically assign logged in-person as investigator, change to null
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(1) > a:nth-child(1)")
  click("#investigator > option:nth-child(1)")
  click("input[name=\"save\"]")
  isPageLoaded(".pageDesc")
  
}


#============RETURN NAME OF CURRENT PAGE==========#
#Return name of current I-NEDSS page
current_page = function(){
  rD$findElement(using = "css", value = ".pageDesc")$getElementText()[[1]] 
}

#============GET BACK TO DASHBOARD==========#
#Click on Dash Board if not already there
click_dashboard = function(){
  if(current_page() != "Dash Board"){
    click("th.dataTableNotSelected:nth-child(1) > a:nth-child(1)")
    
  }
}

#============WAIT FOR PAGE TO LOAD==========#
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

#============SEARCH STATE CASE NUMBER==========#
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

#============ASSIGN INVESTIGATOR==========#
#Assign an investigator
assign_investigator = function(caseNumber, investigator){
  
  search_scn(caseNumber)
  wait_page("Case Summary")
  
  #Check to see if case has been transferred
  jurisdiction = get_text(".NoBorderFull > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(2)")
  
  if(jurisdiction != "Cook County Department of Public Health"){
    message(paste(caseNumber, "has been transferred to", jurisdiction))
    return()
  }
  
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

#============TRANSFER CASE==========#
#transfer cases
jurisdiction_select = read_csv("jurisdiction_selections_list.csv", col_types = cols())

transfer = function(caseNumber, jurisdiction){
  
  #Translate county name to health department name in select dropdown menu
  if(!exists("jurisdiction_select")){
    jurisdiction_select = read_csv("jurisdiction_selections_list.csv", col_types = cols())
  }
  
  if(jurisdiction %in% jurisdiction_select$county){ 
    #If county name, translate to dropdown selection name
    jurisdiction = jurisdiction_select$selection[jurisdiction_select$county == jurisdiction]
    
  }else if(jurisdiction %in% jurisdiction_select$selection){
    #If already selection name, do nothing
    
  }else{
    #If jurisdiction is not in either county name list or dropdown selection list
    message(paste(jurisdiction, "is not an acceptable selection."))
    return()
  }
  
  #search state case number
  search_scn(caseNumber)
  wait_page("Case Summary")
  
  #click transfer case
  click("fieldset.fieldsetHeader:nth-child(6) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(2) > td:nth-child(2) > a:nth-child(1)")
  
  #Check to see if required fields are completed for transferring
  error = try(get_text("#container > div:nth-child(4) > form:nth-child(3) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(4) > td:nth-child(1) > table:nth-child(1) > tbody:nth-child(1) > tr:nth-child(1) > td:nth-child(1) > center:nth-child(1)"),
              silent = T)
  
  if(grepl("Earliest Report Date and Date LHD Received are mandatory fields", error)){
    click("input[value = \"Cancel\"]")
    wait_page("Case Summary")
    
    #Click into Reporting Source
    click("fieldset.fieldsetHeader:nth-child(4) > table:nth-child(2) > tbody:nth-child(1) > tr:nth-child(5) > td:nth-child(1) > a:nth-child(1)")
    wait_page("Reporting Source")
    
    earliestReportMonth = get_text("#report", textbox = T)
    earliestReportDay = get_text(name.is("d1day"),textbox = T)
    earliestReportYear = get_text(name.is("d1year"), textbox = T)
    
    phReceivedMonth = get_text("#received", textbox = T)
    phReceivedDay = get_text(name.is("d2day"),textbox = T)
    phReceivedYear = get_text(name.is("d2year"), textbox = T)
    
    #If one date is filled in, just copy to the other
    if(phReceivedMonth !="" & earliestReportDay ==""){
      enter_text("#report", c(phReceivedMonth, phReceivedDay, phReceivedYear))
      click(value.is("Save"))
    }
    else if (phReceivedMonth =="" & earliestReportDay !=""){
      enter_text("#received", c(earliestReportMonth, earliestReportDay, earliestReportYear))
      click(value.is("Save"))
    }
    #If neither filled in, put in log create day
    else{
      reportDate = get_text()
      
    }
  }
  
 
  #select jurisdiction from dropdown list
  select_drop_down(element = "#jurisdiction", selection = jurisdiction)
  
  #Transfer
  #click(value.is("Transfer"))
  
  #Accept alert
  acceptAlertwithWait()
  
  message(paste(caseNumber, "transferred to", jurisdiction))
}