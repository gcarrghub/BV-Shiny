####
#### https://github.com/gcarrghub/BV-Shiny
####
# Run these lines to check for required packages and install if necessary
# Typically should only need to do this once
packages = c("shiny", "dplyr", "gridExtra", "openxlsx", "optimx", "plotrix")
packageTests <- sapply(packages,FUN = require,character.only=TRUE)
install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/")

### once the packages above are installed
### the tool will run with only these two lines
# If chrome or firefox are your default browsers,
# this should work fine, even in a terminal or Rgui
library(shiny)
runGitHub("BV-Shiny", "gcarrghub",launch.browser=TRUE)

# If Firefox or Chrome are not default browser, open tool with
# the two lines inside this if(FALSE){...}, then copy the http web address
# from the basic browser window into chrome or firefox address bar
if(FALSE){
  library(shiny)
  runGitHub("BV-Shiny", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)
}
