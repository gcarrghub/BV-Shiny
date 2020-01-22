library(shiny)

options(java.parameters = "-Xss2560k")

shinyUI(
     fluidPage(
          titlePanel(HTML("<h1><strong>Bruce-Versteeg Model in Environmental Safety</strong></h1><h4>Version 2.0</h4><h4>Running from github repository https://github.com/gcarrghub/BV-Shiny</h4>"), 
                     windowTitle = "Environmental Safety Bruce-Versteeg Tool"),
          fluidRow(
               
               column(
                    width=3,
                    wellPanel(
                         p("Enter the data you wish to analyze"),
                         #progressInit(),
                         fileInput('inputFile', 'Choose File', multiple=FALSE),
                         uiOutput("sheetUI"),
                         uiOutput("ycolUI"),
                         uiOutput("doseColUI"),
                         br(),
                         sliderInput("ECXvalue","ECx Value",0.05,.95,.5,step=0.025),
                         br(),
                         checkboxInput("varFixed","Assume variance of response is fixed across doses"),
                         uiOutput("zeroOpt"),
                         uiOutput("zeroCond"),
                         #textInput("zeroSub","Values to Substitute for Zero (when applicable)",value=NULL),
                         textInput("xlab","x-label for plot",value="Concentration"),
                         textInput("ylab","y-label for plot",value="Response Level"),
                         checkboxInput("annot8Plot","Annotate Plot",value=FALSE),	
                         checkboxInput("debugPrint","Print Debug Info (local only)",value=FALSE),	
                         br(),
                         uiOutput("button")
                    )
               ),
               column(
                    width=9,
                    tabsetPanel(id="tabs",
                                tabPanel("Help/Documentation",
                                         h3("Requires Firefox or Chrome browser."),
                                         h4("If this page opens in anything else, simply copy-paste the full link/address into the Chrome or Firefox address bar."),
                                         h4("Your R session will also give the same address in a message that should look like 'Listening on http://127.0.0.1:XXXX'"),
                                         tags$li("It's even OK to have multiple sessions open that point to the same address ;-)"),
                                         br(),
                                         strong("Purpose of the Tool:  "),
                                         "Estimate the concentration that results in a given effect level, as a percentage of the average response of untreated controls, by methods described in 
                                              Bruce and Versteeg, ETC 1992.  This analysis is appropriate for experiments in which a continuous response is measured on each individual or 
                                              experimental unit, such as a body weight, or possibly for groups of individuals, such as counts of algae in a vessel.  The response level must decline as 
                                              toxicity (the exposure level) increases.",
                                         tags$ul(
                                         tags$li(strong("This tool is developed for continuous positive value responses such as body weights (NOT counts such as from mortality endpoints).")),
                                         tags$li("Proper usage is the sole responsibility of the user.  No warranty is made or implied, but we appreciate constructive feedback.")),
                                         br(),
                                         column(
                                              width=8,
                                              strong("Directions:"),
                                              tags$ul(
                                                   tags$li("Choose the file containing data for analysis, from a single experiment, (see Data Format and Acceptable File Formats below) using the 
                                                 'Choose File' button in the grey panel at left. Verify data in the 'Data For Analysis' tab."),
                                                   tags$li("Select the desired Effect Level using the slider (effect is %DECREASE from control level) eg, 0.20 is for the concentration resulting in at 20% decrease in the response level.  
                                                      This program attempts to limit this selection to levels plausible for the range of responses observed."),
                                                   tags$li("If it is reasonable to assume the variance of the response is fixed across doses, check the box in the panel."),
                                                   tags$li("Click the Results tab to go to the results page, then click Calculate the Results button in the grey panel.  Both PDF and Excel versions of output can be downloaded at the bottom of results.  If data were provided in Excel, the entire
                                              Excel workbook is returned, plus two sheets with results.")
                                              ),br(),
                                              strong("Notes:"),
                                              tags$ul(
                                                   tags$li("Results may take a minute or more to display on the Results tab, depending on server load at the time."),
                                                   tags$li("In some cases, the numerical results of confidence limits may be of little value, such as a confidence limit that is 
                                              well beyond the range of concentrations tested.")
                                              ),br(),
                                              strong("Data Format:"),
                                              tags$ul(
                                                   tags$li("!!!-IMPORTANT-!!! Data must be arranged in a clean, column-wise rectangular layout with the column labels in the first row, and no extra space/columns on the left margin, no extra columns between data columns, etc.
                                              No annotation of data values should be used (eg, asterisks); everything should be purely numeric except for the column labels.  Even formatting of values can be problematic, so as a 
                                              general rule-of-thumb keep the data file minimal, and clean."),
                                                   tags$li("The input data MUST contain at least two columns, one for the measured response (by default, 'y') and one for the concentration tested ('dose').  If the data does
                                              not contain these column labels (the order does not matter), then the user should select the appropriate columns using the drop down menus after uploading the 
                                              file (choices will be taken from the input file).")
                                              ),                                         
                                              br(),
                                              strong("Acceptable File Formats:"),
                                              tags$ul(
                                                   tags$li("MSExcel Files (both .xls & .xlsx extensions) : If using an Excel file, a single worksheet in that file must be dedicated to the one experiment to analyze 
                                              (no other cells should be filled).  Start the data in row 1 column A.  When an Excel file is used as input, the sheet names within the Excel workbook
                                                      will appear in the grey panel.  If there is more than one sheet in the file, you will be prompted to choose the sheet containing the data.  The github repository
                                                      contains an XLSX workbook within which several valid example datasets are provided, one in each worksheet."),
                                                   tags$li("CSV file: Follow same principles.  In this file format, values in a row are separated by commas."),
                                                   tags$li("Text File: Follow same principles.  In this file format, values in a row are separated by tabs.")
                                                   
                                              ),br(),
                                              strong("Contacts:"),
                                              tags$ul(
                                                   tags$li("Primary: Gregory Carr (carr.gj@pg.com)"),
                                                   tags$li("Secondary: Joel Chaney (chaney.jg@pg.com)")
                                              ),
                                              img(src="PG-DMS.png", 
                                                  #this file should be in the www folder, not top level
                                                  #or maybe ../D&MS_Logo_RGB-Color.png would work?
                                                  height=100)
                                         ),
                                         column(
                                              width=4,
                                              strong("Sample Data:"),
                                              tableOutput("sampleData")
                                         )
                                         ),
                                tabPanel("Data For Analysis",
                                         p("Data read in for Analysis"),
                                         div(textOutput("badData"), style = "color:red"),
                                         tableOutput("DataTab")
                                ),
                                tabPanel("Results",
                                         uiOutput("messages"),
                                         tableOutput("resultsTable"),
                                         plotOutput("plot"),
                                         br(),
                                         downloadButton('downloadResults', 'Download Excel File'),
                                         downloadButton('downloadPlot', 'Download PDF')
                                )
                          )
               )
          )
     )
)

