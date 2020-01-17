####                                        #
#### https://github.com/gcarrghub/BV-Shiny  # 
####                                        # 

#### The latest version of this file can be found at the link above
#### Just click on the same file named shinyFromGIT.R to see the code, then copy-paste
#### into any R script window
#### See the file directly by this link:  https://github.com/gcarrghub/BV-Shiny/blob/master/shinyFromGIT.R

#### For using git/github with Rstudio, see https://happygitwithr.com/, https://www.r-bloggers.com/rstudio-and-github/

#### Run these lines first to check for required packages and install if necessary
#### Typically should only need to do this once
packages = c("shiny", "dplyr", "gridExtra", "openxlsx", "optimx", "plotrix")
packageTests <- sapply(packages,FUN = require,character.only=TRUE)
install.packages(packages[!packageTests], repos = "https://cran.rstudio.com/")

### once the packages above are installed
### the tool will run with only these two lines
### as long as chrome or firefox are your default browsers
### this should work in Rstudio, the Rgui, or even a terminal window on mac
library(shiny)
runGitHub("BV-Shiny", "gcarrghub",launch.browser=TRUE)

if(FALSE){
  ### If Firefox or Chrome are not default browser, open tool with
  ### the two lines inside this if(FALSE){...}, then copy the http web address
  ### from the basic browser window into chrome or firefox address bar
  ###
  ### At least on mac, instead of copy-paste you could alternatively highlight 
  ### the whole address and drag it into an open browser window too
  library(shiny)
  runGitHub("BV-Shiny", "gcarrghub",launch.browser = .rs.invokeShinyWindowViewer)
}
