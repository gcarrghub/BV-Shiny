library(shiny)

# don't think we need this since dropping java-based XLconnect
#options(java.parameters = "-Xss2560k")

shinyUI(fluidPage(
  titlePanel(
    HTML(
      "<h1><strong>Bruce-Versteeg Model in Environmental Safety</strong></h1><h4>Version 2.0</h4><h4>
      Running from github repository https://github.com/gcarrghub/BV-Shiny</h4>"
    ),
    windowTitle = "Environmental Safety Bruce-Versteeg Tool"
  ),
  fluidRow(column(
    width = 3,
    wellPanel(
      p("Enter the data you wish to analyze"),
      #progressInit(),
      shiny::fileInput(
        inputId = 'inputFile',
        label = 'Choose File',
        multiple = FALSE,
        placeholder = "Select a file"
      ),
      uiOutput("sheetUI"),
      uiOutput("ycolUI"),
      uiOutput("doseColUI"),
      #hr(),
      sliderInput("ECXvalue", "ECx Value", 0.05, .95, .5, step =
                    0.05),
      #hr(),
      #checkboxInput("varFixed","Assume response variance is constant"),
      radioButtons(
        "varFixed",
        "Variance assumption",
        choiceNames = c("Proportional", "Constant"),
        choiceValues = c(FALSE, TRUE),
        inline = TRUE,
        width = "100%",
        selected = FALSE
      ),
      uiOutput("zeroOpt"),
      uiOutput("zeroCond"),
      #textInput("zeroSub","Values to Substitute for Zero (when applicable)",value=NULL),
      textInput("xlab", "x-label for plot", value = "Concentration"),
      textInput("ylab", "y-label for plot", value = "Response Level"),
      checkboxInput("annot8Plot", "Annotate Plot", value =
                      FALSE),
      checkboxInput("debugPrint", "Print Debug Info (local only)", value =
                      FALSE),
      br(),
      uiOutput("button")
    )
  ),
  column(
    width = 9,
    tabsetPanel(
      id = "tabs",#type="pills",
      tabPanel(
        "Help/Documentation",
        h3("Firefox or Chrome browsers suggested."),
        tags$ul(
          tags$li("Others may work (e.g., Safari and Brave on Mac) but NOT MS IE in Windows."),
          tags$li(
          "Usually the tool will open in your default browser.  
          If you are not certain of your browser's compatibility, 
          simply copy-paste the full link/address into the address 
          bar of an open Chrome or Firefox window.  
          Your R session provides the same address in a message 
          that should look like 'Listening on http://127.0.0.1:XXXX'."
        ),
        tags$li(
          "It's even OK to have multiple sessions open that point 
          to the same address.  For example, in one window
          you could have the instructions displayed, and in the 
          other select data and options.  Or side-by-side
          analyses of different experiments."
        )),
        br(),
        strong("Purpose of the Tool:  "),
        "Estimate the concentration that results in a given effect level, 
        as a percentage of the average response of untreated controls, 
        by methods described in Bruce and Versteeg, ETC 1992.  This analysis 
        is appropriate for experiments in which a continuous response is 
        measured on each individual or experimental unit, such as a body weight, 
        or possibly for groups of individuals, such as counts of algae in a vessel.  
        The response level must decline as toxicity (the exposure level) increases.",
        tags$ul(
          tags$li(
            strong(
              "This tool is developed for continuous positive value responses 
              such as body weights (NOT counts such as from mortality endpoints)."
            )
          ),
          tags$li(
            "Proper usage is the sole responsibility of the user.  
            No warranty is made or implied, but we appreciate constructive feedback."
          )
        ),
        br(),
        column(
          width = 8,
          strong("Directions:"),
          tags$ul(
            tags$li(
              "Choose the file containing data for analysis, from a single experiment, 
              (see Data Format and Acceptable File Formats below) using the 'Choose File' 
              button in the grey panel at left. Verify data in the 'Data For Analysis' tab."
            ),
            tags$li(
              "Select the desired Effect Level using the slider (effect is %DECREASE 
              from control level) eg, 0.20 is for the concentration resulting in a 
              20% decrease in the response level.  In cases where the response level
              does not approach zero at high concentrations, the estimated effect 
              level concentration can be beyond the range of those tested."
            ),
            tags$li(
              "In many cases it makes sense to assume the measured response variability 
              increases with the average level ('Proportionate'). If it is reasonable to 
              assume the variance of the response is constant across doses, select 'Constant'."
            ),
            tags$li(
              "The 'Calculate Results' button in the lefthand panel will be available 
              after data are selected as long as some basic criteria are satisfied.  
              Both PDF and Excel versions of output can be downloaded at the bottom of results."
            ),
            tags$li(
              strong("Compute time: "),"Results may take a minute or more, depending on the hardware being used."
            ),
            tags$li(
              strong("Results: "),"The user is fully responsible for addressing validity of results.  
              In particular, pay close attention to any cases where the ECx or confidence limits take values 
              beyond the range of concentrations tested."
            ),
            tags$li(
              strong("Exiting: "),"Closing the browser window(s) does not stop the R process.  
              If launched inside Rstudio, click the red 'stop sign' button, in the basic R 
              gui press 'Esc'.  Quitting/killing/exiting from R/Rstudio has the same effect."
            )
          ),
          br(),
          strong("Data & File Formats:"),
          tags$ul(
            tags$li(
              strong("Layout: "),
              "!!!-IMPORTANT-!!! Data must be arranged in a clean, column-wise rectangular 
              layout with the column labels in the first row, and no extra space/columns 
              on the left margin, no extra columns between data columns, etc.  It should 
              look like the example here.  No annotation of data values should be used 
              (eg, asterisks); everything should be purely numeric except for the column labels.  
              Even formatting of values can be problematic, so as a general rule-of-thumb keep 
              the data file minimal, and clean."
            ),
            tags$li(
              strong("Variables: "),
              "The input data MUST contain at least two columns, one for the measured response (by default, 'y') 
              and one for the concentration tested ('dose').  If the data does not contain these column labels 
              (the order does not matter), then the user should select the appropriate columns using the drop-down 
              selections that will appear after selecting the file/sheet (choices will be those found in the first 
              line of the input file)."
            ),
            tags$li(
              strong("Zero or negative response values: "),"When the model incorporates variance proportionality,
              negative and zero data values can cause errors (note also that the model predicts only
              strictly positive response levels).  As such, when the default proportionality
              model is fit, the user will be prompted for a single substitution value for all negative
              or zero values.  While this 'on the fly' option is made available for convenience,
              it is best to avoid undocumented data edits."
            ),
            tags$li(
              strong("MSExcel (both .xls & .xlsx extensions):"), "Data for each experiment to be analyzed 
              should be in separate worksheets and follow 'Layout' principles above.  Start the data in 
              row 1 column A. When it contains multiple worksheets, the user will be prompted to select 
              one that should contain valid data.  The github repository contains an XLSX workbook within 
              which several valid example datasets are provided, one in each worksheet.  By offering this 
              xlsx capability we also need to prevent errors by filtering out sheets that do not look
              like data.  If the desired worksheet is not in the list, then we were unable to detect at 
              least two columns of numbers that could be used in this type of model so it was removed; 
              check the data versus our guidance above."
),
            tags$li(
              strong("Comma separated Values (.csv): "),"Follow same principles.  
              In this plain text file format, values in a row are separated by commas."
            ),
            tags$li(
              strong("Text (.txt): "),"Follow same principles.  
              In this plain text file format, values in a row are separated by tabs."
            )
          ),
          br(),
          strong("Contacts:"),
          tags$ul(
            tags$li("Primary: Gregory Carr (carr.gj@pg.com)"),
            tags$li("Secondary: Joel Chaney (chaney.jg@pg.com)")
          ),
          img(src = "PG-DMS.jpg",
              #as a part of the webpage display, by default this file is looked for in www, but
              #we want to reserve that folder for only temporary files
              height = 100)
        ),
        column(width = 4,
               strong("Sample Data:"),
               tableOutput("sampleData"))
      ),
      tabPanel(
        "Data For Analysis",
        p("Data read in for Analysis"),
        div(textOutput("badData"), style = "color:red"),
        fluidRow(
          column(
            width = 3,
            tableOutput("DataTab")),
          column(
            width=9,
            plotOutput("baseplot"),          strong("Directions:"),
            tags$ul(
              tags$li(
                "Choose the file containing data for analysis, from a single experiment, 
              (see Data Format and Acceptable File Formats) using the 'Choose File' 
              button in the grey panel at left. Verify data in this window."
              ),
              tags$li(
                "Select the desired Effect Level using the slider (effect is %DECREASE 
              from control level) eg, 0.20 is for the concentration resulting in a 
              20% decrease in the response level.  In cases where the response level
              does not approach zero at high concentrations, the estimated effect 
              level concentration can be beyond the range of those tested."
              ),
              tags$li(
                "In many cases it makes sense to assume the measured response variability 
              increases with the average level ('Proportionate'). If it is reasonable to 
              assume the variance of the response is constant across doses, select 'Constant'."
              ),
              tags$li(
                "The 'Calculate Results' button in the lefthand panel will be available 
              after data are selected as long as some basic criteria are satisfied.  
              Both PDF and Excel versions of output can be downloaded at the bottom of results."
              ),
              tags$li(
                strong("Compute time: "),"Results may take a minute or more, depending on the hardware being used."
              ),
              tags$li(
                strong("Results: "),"The user is fully responsible for addressing validity of results.  
              In particular, pay close attention to any cases where the ECx or confidence limits take values 
              beyond the range of concentrations tested."
              ),
              tags$li(
                strong("Exiting: "),"Closing the browser window(s) does not stop the R process.  
              If launched inside Rstudio, click the red 'stop sign' button, in the basic R 
              gui press 'Esc'.  Quitting/killing/exiting from R/Rstudio has the same effect."
              )
            ))
      )),
      tabPanel(
        "Results",
        uiOutput("messages"),
        tableOutput("resultsTable"),
        plotOutput("plot"),
        br(),
        downloadButton('downloadResults', 'Download Excel File'),
        downloadButton('downloadPlot', 'Download PDF')
      )
    )
  ))
))
