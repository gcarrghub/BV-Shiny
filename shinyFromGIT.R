# Check for required packages and install if necessary
packages = c("shiny", "dplyr", "gridExtra", "openxlsx", "optimx", "rgl", "plotrix")
packageTests <- sapply(packages,FUN = require,character.only=TRUE)
install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/")

#with the right packages installed, the tool will run with only these two lines
library(shiny)
# If chrome or firefox are your default browsers, this should work fine, even in
# a terminal or Rgui
runGitHub("BV-Shiny", "gcarrghub",launch.browser=TRUE)

# If Firefox or Chrome are not default browser, open tool with
# the line inside this if(FALSE){...}, then copy the http web address
# into chrome or firefox
if(FALSE){
  runGitHub("BV-Shiny", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)
}
