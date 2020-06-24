library(shiny)
library(dplyr)
library(grid)
library(gridExtra)
#library(XLConnect)
library(openxlsx)
library(plotrix)

if(dir.exists("www")){
        #delete leftover files if they are present from previous runs
        #this only matters when the repository is set up in rstudio
        #and run inside of rstudio in usual way.  When the tool is run
        #through a runGitHub(...) command, it creates temporary space 
        #that is removed on completion (the tool implementation is killed)
        pdfFiles <- list.files(path="www",pattern = "pdf$",full.names = TRUE)
        pngFiles <- list.files(path="www",pattern = "png$",full.names = TRUE)
        xlsxFiles <- list.files(path="www",pattern = "xlsx$",full.names = TRUE)
        files2remove <- c(pdfFiles,pngFiles,xlsxFiles)
        unlink(files2remove)
}
if(!dir.exists("www"))dir.create("www")
shinyServer(function(input, output, session, clientData) {
     values <- reactiveValues()
     values$zeros <- FALSE
     values$annotatedplot <- NULL
     values$cleanplot <- NULL
     values$stamp <- ""

     source("BVFunction2-0.R", local = TRUE)
     
     #use a reactive value that can be used to control what is shown in tabs
     #for example, to clear when new data are selected
     rv <- reactiveValues()
     rv$amsg <- "" #amsg = "a message"
     
     displayResults <- reactive({ return(rv$amsg!="" & regexpr("failed", rv$amsg) < 1) })
     clearResults <- reactive({ return(!(rv$amsg!="" & regexpr("failed", rv$amsg) < 1)) })
     #reactive(displayResults(),{if(!displayResults()){
     #        output$plot <- NULL
     #        output$resultsTable <- NULL
     #        output$messages <- NULL
     #        output$downloadPlot <- NULL
     #        output$downloadResults <- NULL
     #}})
     #session$onFlush(function(){
     #   write(strftime(Sys.time(),"%Y  %m/%d %H:%M:%S" ), file="counter/counter.txt", append = T, sep="\n")
     # }
     # )
     
     ### Check the file extension so the program will know how to read it in 
     getFileExt <- reactive({
          # Read in the .csv input data
          inFile <- input$inputFile
          if(!is.null(inFile)){
               fileExt <- sub(".*\\.","",inFile$name)
               return(fileExt)
               #updateTabsetPanel(session,"tabManager",selected="dataTab")
          } else return(NULL)
     })
     
     ### If it is xls or xlsx, select the sheet to use
     ### By default, the first (or only if the case) sheet is selected
     output$sheetUI <- renderUI({
          fileExt <- getFileExt()
          #if(debugTF)print(fileExt)
          #print(fileExt)
          inFile <- input$inputFile
          #print(inFile)
          #print(grepl("xls",fileExt))
          #print(!is.null(fileExt) && grepl("xls",fileExt))
          ### XLCONNECT here
          if( !is.null(fileExt) && grepl("xls",fileExt) ){
               ### wb <- loadWorkbook(inFile$datapath)
               ### shNames <- getSheets(wb)
                  #print(str(inFile))
                  wb <- loadWorkbook(file = inFile$datapath)
                  #print(str(wb))
                  shNames <- wb$sheet_names
                  shNames <- shNames[sapply(shNames,FUN=function(SN){
                          testSheet <- readWorkbook(wb, sheet=SN, colNames=TRUE)
                          numericCols <- sum(unlist(lapply(testSheet,is.numeric)))
                          (numericCols>1)
                  })]
                  #print(shNames)
               selectInput('shName', 'Select Sheet Containing Data', shNames)
          } else NULL
     })
     
     
     
     ## Observe is like a reactive expression. I think it is appropriate to change tabs to the data tab each time the file 
     ##  extension is changed.  Better to switch any time any of the data selection options change
     observe({
          ## Throw dataOrg() in so that anytime a selection is made that has the potential to change the data, redirect the client to the 
          ##   data tab.  This is causing shiny to crash IDK why.  I have tried inputDataFile() as well but to no avail.
          if(!is.null(getFileExt()))updateTabsetPanel(session,"tabManager",selected="dataTab")
          
     })
     
     ### read in the data, depending on type as given by file extension
     getData <- reactive({
          req(input$inputFile)
          inFile <- input$inputFile
          if(!is.null(inFile)){
               values$zeros <- FALSE
               fileExt <- sub(".*\\.","",inFile[1])
               
               ## Use grepl because readWorksheet works with both xls and xlsx versions of wb.
               if(grepl("xls",fileExt)){
                       ### XLCONNECT here
                     wb <- loadWorkbook(inFile$datapath)# same in XLconnect as openxlsx
                    shName <- input$shName
                    if(is.null(shName)){
                      indata <- NULL
                    } else {
                      ### indata <- readWorksheet(wb, sheet=shName, header=TRUE)### XLCONNECT here
                      indata <- readWorkbook(wb, sheet=shName, colNames=TRUE)### XLCONNECT here
                    }
               }
               if(fileExt=="txt"){
                    indata <- read.table(inFile$datapath,header=TRUE)
               }
               if(fileExt=="csv"){
                    indata <- read.csv(inFile$datapath,header=TRUE)
               }
               return(indata=indata)
          }
          
     })

     ### Once the data has been selected, set it up for analysis,
     ### which is to say identify the dose and response variables,
     ### and check for zeros
     dataOrg <- reactive({
          ### getData(), above, is how to read the file
          ### req(getData()) is to say don't go further 
          ### unless it returns data..
          shiny::req(getData())
          namesInp <- names(getData())
          #If the expected names are present, assume we are good to go
          if(all(c("y","dose") %in% namesInp)){
               BVdata <- getData()[,c("y","dose")]
          }
          #If not, user has to choose which vars to use
          if(!(all(c("y","dose") %in% namesInp))){
               dataDirty <- getData()
               ## Have to make sure the UI is generated before I subset.
               if(is.null(input$nameDoseCol))return(NULL)
               BVdata <- dataDirty[,c(input$nameYCol,input$nameDoseCol)]
               names(BVdata) <- c("y","dose")
          }

          ### set flag on whether zeros are present
          if(any(BVdata$y <= 0)){
               values$zeros <- TRUE
          } else {
               values$zeros <- FALSE
          }
          fBasedCritVal <- qf(0.95,1,nrow(BVdata)-1)
          BVdata <- BVdata[order(BVdata$dose),]
          uDoses <- sort(unique(BVdata$dose))
          nzDoses <- uDoses[uDoses>0]
          return(list(BVdata=BVdata,fBasedCritVal=fBasedCritVal,nzDoses=nzDoses))
     })
     
     ### This implements the changes to BVdata when Ignore or Replace is selected
     dataOrgZeroFixed <- reactive({
             #BVdata has to be present in output from dataOrg to continue
             #print(c(varFixed=as.logical(input$varFixed)))
             #source("BVFunction2-0.R", local = TRUE)
             shiny::req(dataOrg()[["BVdata"]])
             BVdata <- dataOrg()[["BVdata"]]
             ### values$zeros is set in dataOrg(), so it has to be ready here
             if(values$zeros){
                     shiny::req(is.element("zeroOptSelect",names(input)),is.element("varFixed",names(input)))
                     shiny::req(!is.null(input$zeroOptSelect),!is.null(input$varFixed))
                     #BVdata <- dataOrg()[["BVdata"]]
                     ### Important change:  If variance is constant, do not need to remove/delete <=0
                     ### So when constant is chosen, "Ignore" will mean just keep the value, 
                     ### or you could still change it with "Replace", in which case
                     ### the user will still have to choose something.
                     #if(!is.null(input$zeroOptSelect)){
                     if(input$zeroOptSelect == "Drop"){
                             BVdata <- BVdata %>% filter(y > 0)
                     }
                     if(input$zeroOptSelect == "Replace"){
                             BVdata$y <- ifelse(BVdata$y <= 0, as.numeric(input$zeroSub), BVdata$y)
                     }
                     #}
             }
             #showTab(inputId = "tabs",target = "Data For Analysis")
             output$baseplot <- renderPlot({
                     #isolate
                     ({
                             plotBV.LOG(dataOrgZeroFixed(),
                                        bestPars = NULL,
                                        ECxTarget=input$ECXvalue,
                                        goodFit = FALSE,
                                        basePlot = TRUE,
                                        ylimInput=c(0,max(na.omit(dataOrgZeroFixed()$y))),
                                        xlim=10^range(c(floor(log10(min(dataOrgZeroFixed()$dose[dataOrgZeroFixed()$dose>0])))-1,ceiling(log10(max(dataOrgZeroFixed()$dose))))),
                                        clean=TRUE,littleLogs=FALSE,xlabel = input$xlab, ylabel = input$ylab)
                             title(main="Input data scatter plot")
                     })
                     
             })
             return(BVdata)
     })
     
     ### When zeros are present, modify the lefthand data section to open
     ### a drop-down selection of choices to handle zero values 
     ### (the Ignore or Replace drop-down)
     output$zeroOpt <- renderUI({
             shiny::req(dataOrg()[["BVdata"]],input$varFixed)
             df <- dataOrg()[["BVdata"]]
             if(values$zeros & !as.logical(input$varFixed)){
                     return(radioButtons("zeroOptSelect","Action on values <= 0",
                                         choices=c("Drop","Replace"),
                                         inline=TRUE,
                                         width="100%",selected = "Replace")
                     )
                     #return(selectInput("zeroOptSelect","Select an option for handling values <=0", choices=c(" ","Ignore","Replace")))
             } else if(values$zeros & as.logical(input$varFixed)){
                     return(radioButtons("zeroOptSelect","Action on values <= 0",
                                         choices=c("Drop","Keep"),
                                         inline=TRUE,
                                         width="100%",selected = "Keep")
                     )
                     #return(selectInput("zeroOptSelect","Select an option for handling values <=0", choices=c(" ","Ignore","Replace")))
             } else {
                     return(NULL)
             }
     })
     
     ### and after the drop-down selection, either drop the zeros completely,
     ### or open a field for value entry if user has selected "Replace"
     ### When "Replace" is selected, open a text entry box for the new value
     output$zeroCond <- renderUI({
             shiny::req(dataOrg()$BVdata)
             df <- dataOrg()$BVdata
             if(any(df$y <= 0)){
                     if(length(input$zeroOptSelect)>0){#don't do following unless selection has been made....
                             if(input$zeroOptSelect!=" "){
                                     if(input$zeroOptSelect=='Replace'){
                                             # default value changed from NULL to ""
                                             # (NULL is harder to deal with, causes silent errors etc)
                                             return(textInput("zeroSub","Replacement value for those <=0",value=""))
                                     }
                             }
                     }
             }
             return(NULL)
     })
     
     
     ### When there is not a column named "y", select one as the responses
     output$ycolUI <- renderUI({
             shiny::req(getData())
             namesInFrame <- names(getData())
             if(all(c("y","dose") %in% namesInFrame)){
                     NULL
             } else {
                     namesChoices <- namesInFrame
                     #if(is.element("nameDoseCol",names(input))){
                     #       namesChoices <- namesChoices[namesChoices!=input$nameDoseCol]
                     #}
                     return(selectInput(inputId="nameYCol","Select Response Variable",
                                        namesChoices,namesChoices[1]))
             }
     })
     
     ### When there is not a column named "dose", select one as the doses
     output$doseColUI <- renderUI({
             shiny::req(getData())
             namesInFrame <- names(getData())
             if(all(c("y","dose") %in% namesInFrame)){
                     NULL
             } else {
                     namesChoices <- namesInFrame
                     if(is.element("nameYCol",names(input))){
                             #this prevents a user from selecting the same column of data for both response and dose
                             namesChoices <- namesChoices[namesChoices!=input$nameYCol]
                     }
                     return(selectInput(inputId="nameDoseCol","Select Concentration Variable",
                                        namesChoices,namesChoices[1]))
             }     
     })
     
     #Controls when the button that runs the analysis is available.  It is
     #taken away, for example, when zeros are present.
     output$button <- renderUI({
             # if no zero options stuff it means ready to go as is
             # zeroOptSelect will be NULL when there are no zeros in the data,
             # or when an action to deal with zeros has not be selected.
             # This is not foolproof.  The run button will show if, for 
             # example, the replacment is itself <= 0, or a non-numeric string.
             #print(c(zeroOptSelect=input$zeroOptSelect,zeroSub=input$zeroSub))
             if(!values$zeros){
                     return(actionButton("updateRes","Calculate Results"))
             }
             if(values$zeros){
                     #only get in here if a <=0 value is detected in the data
                     shiny::req(is.element("zeroOptSelect",names(input)))
                     if(input$zeroOptSelect!="Replace")return(actionButton("updateRes","Calculate Results"))
                     #zeroSub will be in the input list once a value is begun by the user
                     #this is facilitated now by making the default value "" instead of NULL
                     if(input$zeroOptSelect=="Replace" & is.element("zeroSub",names(input))){
                             #print(c(zeroOptSelect=input$zeroOptSelect,zeroSub=input$zeroSub))
                             #as long as it's a number >0, will allow the analysis to run
                             ready2go <- FALSE
                             is.a.number <- is.finite(as.numeric(input$zeroSub))
                             if(is.a.number){if(as.numeric(input$zeroSub)>0)ready2go <- TRUE}
                             if(!ready2go)return(p("Please enter a positive value to replace observations <=0.", style="color:red"))
                             if( ready2go)return(actionButton("updateRes","Calculate Results"))
                     }
             }
             })
     
     ### In the data tab, show the data whenever it is updated (I think)
     ### not 100% sure how this works, required to changes to dataOrgZeroFixed()
     ### where it also passes back the data if no zeros are found.
     output$DataTab <- renderTable({
          # only render when there's something to...render...
          shiny::req(dataOrg()$BVdata)
          dataOrgZeroFixed()
     })

     ### show the plot for output
     output$plot <- renderPlot({
          values$cleanplot
     })
     
     
     getShortFileName <- reactive({
          paste0("BVoutput", values$stamp)
     })
     
     getPDFfilename <- reactive({
          paste0("www/", getShortFileName(), ".pdf")
     })
     
     getExcelfilename <- reactive({
          paste0("www/", getShortFileName(), ".xlsx")
     })

     ### THE MAIN ANALYSIS CONTROL
     observeEvent(input$updateRes, {
          updateTabsetPanel(session, "tabs", "Results")
          values$stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
          
          withProgress({
               setProgress(message = "Please Wait")
               varFixed <- as.logical(input$varFixed)
               #source("BVFunction2-0.R", local = TRUE)
               if(input$debugPrint)print(paste("Working directory:",getwd()))
               setProgress(detail = "Running analysis and creating pdf")
               pdffilename <- getPDFfilename()
               pdf(pdffilename, width = 9)
               par(mai=c(1,1,1,0.1))
               #remove fit-related objects in global environment before each run
               fitOBJs <- c("bestParsEC50","bestParsECx","bestParsLL",
                              "fBasedCritVal","goodFlag","lowerCI",
                              "upperCI")
               checkOBJs <- sapply(fitOBJs,FUN = exists,envir = .GlobalEnv)
               if(any(checkOBJs))remove(list=fitOBJs[checkOBJs],envir = .GlobalEnv)
               results <- fitBV.PLL (
                    BVdata=dataOrgZeroFixed(),
                    ECXvalue=input$ECXvalue,
                    fileName=NULL,
                    verbose=input$debugPrint,
                    do3Dstart=FALSE,
                    FORCE=FALSE,
                    zeroSub=NULL,
                    varFixed=as.logical(input$varFixed),
                    ylabel = input$ylab,
                    xlabel = input$xlab)
               grid.newpage()
               grid.table(dataOrgZeroFixed(), rows = NULL)
               #addtable2plot(.5,1,dataOrgZeroFixed(),xjust=.5,yjust=0)	 
               #print("Check 1:  Sever finish fit")
               #print(results)
               #print(str(results))
               dev.off()
               setProgress(detail = "Creating table")
               #print("Check 2:  Sever create table")
               resultsTable <- data.frame(EC.level.PCT = 100*results$EC.level, 
                                          ECx = as.numeric(to3(results$ECx)), 
                                          LCL.95 = ifelse(goodFlag,yes = as.numeric(to3(results$LCL.95)),no = NA), 
                                          UCL.95 = ifelse(goodFlag,yes = as.numeric(to3(results$UCL.95)),no = NA))
               #print("Check 3:  Sever finished table")
               output$resultsTable <- shiny::renderTable(expr = {resultsTable},bordered = TRUE,na = "N/A")
               
               setProgress(detail = "Creating plot")
               
               #print("Check 2:  Sever start plot")
               #print(c(goodFlag=goodFlag,ECXvalue=input$ECXvalue))
               output$plot <- renderPlot({
                    isolate({
                         plotBV.LOG(dataOrgZeroFixed(),
                                    bestPars = bestParsECx,
                                    ECxTarget=input$ECXvalue,
                                    goodFit = goodFlag,
                                    ylimInput=c(0,max(dataOrgZeroFixed()$y,exp(lowerCI["Asym"]),exp(upperCI["Asym"]))),
                                    xlim=10^range(c(floor(c(lowerCI["xmid"],log10(min(dataOrgZeroFixed()$dose[dataOrgZeroFixed()$dose>0]))))-1,ceiling(c(upperCI["xmid"],log10(max(dataOrgZeroFixed()$dose)))))),
                                    clean=!input$annot8Plot,littleLogs=FALSE,xlabel = input$xlab, ylabel = input$ylab)
                         if(!goodFlag)title(main="Check data/results -- Model fit suggests poor fit or no trend?")
                    })
                    
               })
               
               output$messages <- renderUI({
                       shiny::req(values$zeros,input$zeroOptSelect)
                       if(values$zeros){
                               if(input$zeroOptSelect == "Drop"){
                                       negmessage <- "Some values <= 0 have been dropped from the analysis."
                               }
                               if(input$zeroOptSelect == "Keep"){
                                       negmessage <- "Some values <= 0 are included in the analysis."
                               }
                               if(input$zeroOptSelect == "Replace"){
                                       negmessage <- paste0("All values <= 0 have been replaced with ", input$zeroSub, ".")
                               }
                       } else {
                               negmessage <- "There are no response values <= 0 in the analyzed dataset."
                       }
                       if(is.na(as.numeric(resultsTable$LCL.95))){
                               lclmessage <- "The lower confidence limit could not be calculated."
                       } else {
                               lclmessage <- "The lower confidence limit calculation was successful."
                       }
                       if(is.na(as.numeric(resultsTable$UCL.95))){
                               uclmessage <- "The upper confidence limit could not be calculated."
                       } else {
                               uclmessage <- "The upper confidence limit calculation was successful."
                       }
                       return(p(negmessage, br(), lclmessage, br(), uclmessage))
                       
               })
               
               setProgress(detail = "Creating Excel")
                
               numSheetName <- "Numerical Results"
               plotSheetName <- "Plot"
               dataSheetName <- "Analyzed Data"
               xlsxfilename <- paste0("www/BVoutput", values$stamp, ".xlsx")
               
               if(FALSE){### old XLConect approach
                       wb <- loadWorkbook(xlsxfilename, create = T)### XLCONNECT here
                       createSheet(wb, numSheetName)### XLCONNECT here
                       writeWorksheet(wb, resultsTable, numSheetName)### XLCONNECT here
                       writeWorksheet(wb, date(), numSheetName, startRow=3, startCol=ncol(resultsTable)+2, header=FALSE)### XLCONNECT here
                       setColumnWidth(wb, numSheetName, 1:(ncol(resultsTable)+2), -1)### XLCONNECT here
               }
               wb <- createWorkbook(creator = "BV shiny app")
               addWorksheet(wb = wb,sheetName = numSheetName,zoom = 200)
               #options("openxlsx.numFmt" = "#")
               setColWidths(wb = wb, sheet = numSheetName, cols = 1:4, widths = 15)
               writeDataTable(wb = wb,sheet = numSheetName,x = resultsTable,withFilter=FALSE)
               #options("openxlsx.numFmt" = NULL)
                              #,
                              #tableStyle = createStyle(fontSize = 14),
                              #headerStyle = createStyle(fontSize = 14))
               writeData(wb = wb,sheet = numSheetName,x = date(), startRow=4, startCol=1)
               #####createSheet(wb, plotSheetName)### XLCONNECT here
               addWorksheet(wb = wb,sheetName = plotSheetName,zoom = 200)
               cleanPlotFilename <- paste0("www/cleanResPlot", values$stamp, ".png")
               png(cleanPlotFilename,height=6,width=8,units = "in",res = 200)#,type = "cairo")
               par(mai=c(1,1,0.1,0.1))
               plotBV.LOG(inputData = dataOrgZeroFixed(),
                          bestPars = bestParsECx,
                          ECxTarget=input$ECXvalue,
                          goodFit = goodFlag,
                          ylimInput = c(0,max(dataOrgZeroFixed()$y,exp(lowerCI["Asym"]),exp(upperCI["Asym"]))),
                          xlimInput = 10^range(c(floor(c(lowerCI["xmid"],log10(min(dataOrgZeroFixed()$dose[dataOrgZeroFixed()$dose>0]))))-1,ceiling(c(upperCI["xmid"],log10(max(dataOrgZeroFixed()$dose)))))),
                          clean=TRUE,littleLogs=FALSE,xlabel = input$xlab, ylabel = input$ylab)
               dev.off()
               ### createName(wb, name="cleanPlot", formula=paste0(plotSheetName,"!$A$1"))### XLCONNECT here
               ### addImage(wb, filename=cleanPlotFilename, name="cleanPlot", originalSize=TRUE)### XLCONNECT here
               insertImage(wb = wb,sheet = plotSheetName,file = cleanPlotFilename,
                           height = 6,width = 8)
               
               dirtyPlotFilename <- paste0("www/dirtyResPlot", values$stamp, ".png")
               png(dirtyPlotFilename,height=6,width=8,units = "in",res = 200)#,type = "cairo")
               par(mai=c(1,1,1,0.1))
               plotBV.LOG(inputData = dataOrgZeroFixed(),
                          bestPars = bestParsECx,
                          ECxTarget=input$ECXvalue,
                          goodFit = goodFlag,
                          ylimInput=c(0,max(dataOrgZeroFixed()$y,exp(lowerCI["Asym"]),exp(upperCI["Asym"]))),
                          xlim=10^range(c(floor(c(lowerCI["xmid"],log10(min(dataOrgZeroFixed()$dose[dataOrgZeroFixed()$dose>0]))))-1,ceiling(c(upperCI["xmid"],log10(max(dataOrgZeroFixed()$dose)))))),
                          clean=FALSE,littleLogs=FALSE,xlabel = input$xlab, ylabel = input$ylab)
               
               dev.off()
               ### createName(wb, name="dirtyPlot", formula=paste0(plotSheetName,"!$N$1"))### XLCONNECT here
               ### addImage(wb, filename=dirtyPlotFilename, name="dirtyPlot", originalSize=TRUE)### XLCONNECT here
               insertImage(wb = wb,sheet = plotSheetName,file = dirtyPlotFilename,startCol = 13,
                           height = 6,width = 8)
               
               ### createSheet(wb, dataSheetName)### XLCONNECT here
               ### writeWorksheet(wb, dataOrgZeroFixed(), dataSheetName)### XLCONNECT here
               addWorksheet(wb = wb,sheetName = dataSheetName,zoom = 200)
               writeDataTable(wb = wb,sheet = dataSheetName,x = dataOrgZeroFixed())
               
               saveWorkbook(wb = wb,file = xlsxfilename,overwrite = TRUE)
               ### saveWorkbook(wb, xlsxfilename)### XLCONNECT here
               rv$amsg <- "complete"
               
          })
     })
     
     #observeEvent(dataOrg(), {
     #        #observeEvent(dataOrgZeroFixed(), {
     #     updateTabsetPanel(session,"tabs", "Data For Analysis")
     #})
     
     ### (I think) whenever the data change, update the data tab and clear out results tab
     inputChanges <- reactive({list(dataOrgZeroFixed(),dataOrg(),input$ECXvalue,input$varFixed)})
     observeEvent(inputChanges(), {
             #print(rv$amsg)
             rv$amsg <- ""
             output$plot <- NULL
             output$resultsTable <- NULL
             output$messages <- renderUI({
                     p("Do not download files here unless data table/plot are displayed.
                       Otherwise these files may be for a previously run analysis.")})
             updateTabsetPanel(session,"tabs", "Data For Analysis")
     })
     #based on rv$amsg, only download a file when an analysis has been completed,
     #and no changes to data or analysis params have been selected.  So the one
     #watchout is that if you are doing multiple runs, you need to download the
     #output before doing anything else.
     observeEvent({rv$amsg},{
             #print(c(rv.amsg=rv$amsg))
             if(rv$amsg==""){
                     output$downloadPlot <- downloadHandler(
                             filename=paste0(getShortFileName(), ".pdf"),
                             content=function(file){} 
                     )
                     output$downloadResults <- downloadHandler( 
                             filename=paste0(getShortFileName(), ".xlsx"),
                             content=function(file){}
                     )
             }
             if(rv$amsg=="complete"){
                     output$downloadPlot <- downloadHandler(
                             filename=paste0(getShortFileName(), ".pdf"),
                             content=function(file){
                                     file.copy(getPDFfilename(), file)
                             }   
                     )
                     output$downloadResults <- downloadHandler( 
                             filename=paste0(getShortFileName(), ".xlsx"),
                             content=function(file){
                                     file.copy(getExcelfilename(), file)
                             }   
                     )
             }
             })
     if(FALSE){
             output$downloadPlot <- downloadHandler(
                     filename=paste0(getShortFileName(), ".pdf"),
                     content=function(file){
                             file.copy(getPDFfilename(), file)
                     }   
             )
             ### create the download link for the excel file
             output$downloadResults <- downloadHandler( 
                     filename=paste0(getShortFileName(), ".xlsx"),
                     content=function(file){
                             file.copy(getExcelfilename(), file)
                     }   
             )
     }

        ### the example data that are displayed with instructions for the tool
     output$sampleData <- renderTable({ 
          data.frame(y=c(120.9,118,134,121.2,118.6,120.4,82.6,62.8,
                         81.6,49.3,41.6,41.3,12.7,14.7,14.7,4.93,4,4.4),
                doses=c(0,0,0,5,5,5,10,10,10,20,20,20,40,40,40,80,80,80))
     }, include.rownames=FALSE)
})
