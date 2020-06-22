library(RSelenium)
library(magrittr)
library(tidyverse)
library(keyring)

#key_set("idph_username") #set your IDPH web portal username -- only needs to be done once per computer
#key_set("idph_portal")  #set your IDPH web portal password -- only needs to be done once per computer

#Set report name 
name <- paste0(Sys.Date(), " ")

#Load helper functions
devtools::source_url("https://github.com/hsteinberg/ccdph-functions/blob/master/general-use-rselenium-functions.R?raw=TRUE")
devtools::source_url("https://github.com/hsteinberg/ccdph-functions/blob/master/inedss-rselenium-functions.R?raw=TRUE")

#Setting Firefox to avoid download type
firefoxProfile <- makeFirefoxProfile(list(browser.helperApps.neverAsk.saveToDisk = "application/comma-separated-values ,text/csv"))

#Open selenium session
start_server()

#Navigating to log-in page
rD$navigate("https://dph.partner.illinois.gov/my.policy")

#Pause for page to load
Sys.sleep(5)

#Check for cookies error
login_error <- try(rD$findElement("css", "#newSessionDIV > a:nth-child(1)"))
if (class(login_error) != "try-error") {login_error$clickElement()}

#Pause for page to load
Sys.sleep(5)

#Clicking link to access log-in screen
click(".interaction_table_text_cell > a:nth-child(1)")

#Pause for page to load
Sys.sleep(5)

#Enter credentials and log in
enter_text("#input_1", c(key_get("idph_username"), key = "tab", key_get("idph_portal")))
click(value.is("Logon"))

#Pausing execution to give time to log in and load page
Sys.sleep(10)

#Mousing over applications button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a/span/span')$mouseMoveToLocation()

#Finding production apps button
rD$findElement(using = "xpath", value = '//*[@id="zz6_RootAspMenu"]/li/ul/li[1]/a')$clickElement()

#Identify production apps present on page
appsTableLinks <- rD$findElements("css", "a.dph-applink") 

#Store location of BO link  
BOLink <- map_chr(appsTableLinks, function(x) x$getElementText()[[1]]) %>%
  grepl("Business Objects", .) %>%
  which(. == TRUE)

#Click INEDSS link
appsTableLinks[[BOLink]]$clickElement()

#Pausing execution to give time to load page
Sys.sleep(5)

#Switching focus to BO tab   
windows <- rD$getWindowHandles()   
rD$switchToWindow(windows[[2]])

#Clicking link to access BO log-in screen
click(".interaction_table_text_cell > a:nth-child(1)")

#Pause for page to load
Sys.sleep(5)

#Enter BO credentials
enter_text("#input_1", c(key_get("idph_username"), key = "tab", key_get("idph_portal")))
ifVisiblethenClick("option[value = \"idph.il\"]")
click(value.is("Logon"))

#Pausing execution to give time to log in and load page
Sys.sleep(60)

#Get into inbox
#Report in nested iframe so need to get into correct frame first
frames_first <- rD$findElements(using = "tag name", "iframe")
#length(frames_first)  #1
rD$switchToFrame(frames_first[[1]])

frames_second <- rD$findElements(using = "tag name", "iframe")
#length(frames_second) #5
rD$switchToFrame(frames_second[[5]]) #WORKSSS!!!!

#Get titles of unread items in BO inbox
titles <- character(10)
for (i in 1:10) {
  titles[i] <- get_text(paste0("#id_", i + 52))
}

#Click desired item
click(paste0("#id_", which(grepl(name, titles)) + 52))

#Pausing execution for 1 minute to give time to download the report
Sys.sleep(60)

#stop session -- ALWAYS RUN THIS CODE AT THE END TO STOP THE SERVER
remDr$server$stop() #stop session

