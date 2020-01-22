####                                        #
#### https://github.com/gcarrghub/BV-Shiny  # 
####                                        # 

####
#### If defaults are satisfied (chrome or firefox are your default browser) devtools installed), should be able to just do the following:
dtCheck <- require(devtools)
if(!dtCheck)install.packages("devtools", repos = "https://cran.rstudio.com/", dependencies=TRUE)
#### Following are two equivalent one-liners that will launch the tool by the defaults of this script
if(FALSE){
  devtools::source_url("https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R?raw=TRUE")
  devtools::source_url("https://raw.githubusercontent.com/gcarrghub/BV-Shiny/master/shinyFromGIT.R?raw=TRUE")
}
#### The latest version of this file can be found at the link below
#### Just click on the same file named shinyFromGIT.R to see the code, then copy-paste
#### into any R script window
#### See the file directly by this link in plain text:
#### https://raw.githubusercontent.com/gcarrghub/BV-Shiny/master/shinyFromGIT.R
#### or in a code-formatted window:
#### https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R
#### From this view, right click on the "Raw" button to save to a local file

#### Example data are at 
#### https://github.com/gcarrghub/BV-Shiny/blob/master/BV%20paper%20data.xlsx
#### Click the download button on the page that opens to use it locally

#### For using git/github with Rstudio, 
#### see https://happygitwithr.com/, https://www.r-bloggers.com/rstudio-and-github/


#### Run these lines first to check for required packages and install if necessary
#### Typically should only need to do this once
packages = c("shiny", "dplyr", "gridExtra", "openxlsx", "optimx", "plotrix")
packageTests <- sapply(packages,FUN = require,character.only=TRUE)
if(sum(!packageTests)>0)install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/", dependencies=TRUE)
### In rare cases, if package installation returns errors related to 00LOCK... folder/file, this may work (no promises)
if(FALSE)install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/", dependencies=TRUE, INSTALL_opts = c('--no-lock'))

### once the packages above are installed
### the tool will run with only these two lines
### as long as chrome or firefox are your default browsers
### this should work in Rstudio, the Rgui, or even a terminal window on mac
library(shiny)
runGitHub("BV-Shiny", "gcarrghub",launch.browser=TRUE)


if(FALSE){
  ### If Firefox or Chrome are not default browser, open tool with
  ### the following two lines, then copy the http web address
  ### from the resulting basic browser window into chrome or firefox address bar
  ###
  ### At least on mac, instead of copy-paste you could alternatively highlight 
  ### the whole address and drag it into an open browser window too
  library(shiny)
  ### NOTE:  This will ONLY work in Rstudio
  runGitHub("BV-Shiny", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)
  
  ### If something other than Chrome or Firefox is your default browser and you 
  ### don't want to change it, another option may be to set a browser option first 
  ### as follows:
  library(shiny)
  ### The location in the following two commands may need to be modified to work on 
  ### your MSwindows system.  It is up to you to find where chrome.exe or firefox.exe 
  ### is located if these don't work.  Use only one of the following two depending on 
  ### your preference on windows
  options(browser = "C:/Program Files/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
  options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")
  ### Use only one of the following two on mac
  options(browser = "/usr/bin/open -a '/Applications/Google Chrome.app'")
  options(browser = "/usr/bin/open -a '/Applications/Firefox.app'")
  #now the app will open in your above selected browser, independent of defaults
  runGitHub("BV-Shiny", "gcarrghub",launch.browser=TRUE)
  
  ### it seems that an artifact of messing with browser option is that help pages now 
  ### open in the browser too, instead of internal to Rstudio.  To get back to default 
  ### behavior reset with this command:
  options(browser = function(url){.Call("rs_browseURL", url)})
  }


