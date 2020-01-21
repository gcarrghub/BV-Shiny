library(shiny)

options(java.parameters = "-Xss2560k")

shinyUI(
     fluidPage(
          titlePanel(HTML("<h1><strong>Environmental Safety Bruce-Versteeg Tool</strong></h1><h4>Version 2.0</h4>"), 
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
                                         h4("Requires use of Firefox or Chrome browsers"),
                                         br(),
                                         strong("Purpose of the Tool"),
                                         p("Estimate the concentration that results in a given effect level, as a percentage of the average response of untreated controls, by methods described in 
                                              Bruce and Versteeg, ETC 1992.  This analysis is appropriate for experiments in which a continuous response is measured on each individual or 
                                              experimental unit, such as a body weight, or possibly for groups of individuals, such as counts of algae in a vessel.  The response level must decline as 
                                              toxicity (the exposure level) increases."),
                                         p(strong("This tool is not meant to be used for count data."), " For this type of data, please visit the",
                                           a(href='http://mb-qs-pp-cuda03.rd-hcanalyt001.na.pg.com:3838/ESO.Tools/logisAb/', "Logistic Abbott Tool"), 
                                         " or return to the ", a(href='http://qsportal.pg.com/ESOTools/', "full list of ESO tools.")),
                                         br(),
                                         
                                         column(
                                              width=8,
                                              strong("Directions:"),
                                              tags$ul(
                                                   tags$li("Choose the file containing data for analysis, from a single experiment, (see Data Format and Acceptable File Formats below) using the 
                                                 'Choose File' button in the grey panel. Verify data is correct on Data For Analysis tab."),
                                                   tags$li("Select the desired Effect Level using the slider (effect is DECREASE from control level) eg, 0.20 is for the concentration resulting in at 20% decrease in the response level.  
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
                                                      will appear in the grey panel.  If there is more than one sheet in the file, you will be prompted to choose the sheet containing the data."),
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

