library(shiny)
library(dplyr)
library(grid)
library(gridExtra)
#library(XLConnect)
library(openxlsx)

if(!dir.exists("www"))dir.create("www")

shinyServer(function(input, output, session, clientData) {
     values <- reactiveValues()
     values$zeros <- FALSE
     values$annotatedplot <- NULL
     values$cleanplot <- NULL
     values$stamp <- ""

     #session$onFlush(function(){
     #   write(strftime(Sys.time(),"%Y  %m/%d %H:%M:%S" ), file="counter/counter.txt", append = T, sep="\n")
     # }
     # )
     
     ### Data read in 
     getFileExt <- reactive({
          # Read in the .csv input data
          inFile <- input$inputFile
          if(!is.null(inFile)){
               fileExt <- sub(".*\\.","",inFile$name)
               return(fileExt)
               #updateTabsetPanel(session,"tabManager",selected="dataTab")
          } else return(NULL)
     })
     
     
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
     
     getData <- reactive({
          req(input$inputFile)
          inFile <- input$inputFile
          if(!is.null(inFile)){
               
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

     dataOrg <- reactive({
          req(getData())
          
          namesInp <- names(getData())
          #print(namesInp)
          if(all(c("y","dose") %in% namesInp)){
               BVdata <- getData()[,c("y","dose")]
          }
          if(!(all(c("y","dose") %in% namesInp))){
               dataDirty <- getData()
               ## Have to make sure the UI is generated before I subset.
               if(is.null(input$nameDoseCol))return(NULL)
               BVdata <- dataDirty[,c(input$nameYCol,input$nameDoseCol)]
               names(BVdata) <- c("y","dose")
          }

          
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
     
     dataOrgZeroFixed <- reactive({
          req(dataOrg()[["BVdata"]])
          BVdata <- dataOrg()[["BVdata"]]
          if(!is.null(input$zeroOptSelect)){
               if(input$zeroOptSelect == "Ignore"){
                    
                    BVdata <- BVdata %>% filter(y > 0)
               }
               if(input$zeroOptSelect == "Replace"){
                    
                    BVdata$y <- ifelse(BVdata$y <= 0, as.numeric(input$zeroSub), BVdata$y)
               }
          }
          return(BVdata)
     })
     
     output$zeroOpt <- renderUI({
          req(dataOrg()[["BVdata"]])
          df <- dataOrg()[["BVdata"]]
          if(values$zeros){
               return(selectInput("zeroOptSelect","Select an option for handling values <=0", choices=c(" ","Ignore","Replace")))
          } else {
               return(NULL)
          }
          
     })
     
     output$zeroCond <- renderUI({
          req(dataOrg()$BVdata)
          df <- dataOrg()$BVdata
          
          if(any(df$y <= 0)){
               if(input$zeroOptSelect=='Replace'){
                    return(textInput("zeroSub","Value to Substitute for <=0",value=NULL))
               }
          }
          return(NULL)
     })
     
     output$ycolUI <- renderUI({
          req(getData())
          namesInp <- names(getData())
          if(all(c("y","dose") %in% namesInp)){
               NULL
          } else {
               namesInFrame <- names(getData())
               return(selectInput(inputId="nameYCol","Select Response Variable",namesInFrame,namesInFrame[1]))
               
          }
     })
     
     
     output$doseColUI <- renderUI({
          req(getData())
          namesInp <- names(getData())
          if(all(c("y","dose") %in% namesInp)){
               NULL
          } else {
               namesInFrame <- names(getData())
               return(selectInput(inputId="nameDoseCol","Select Concentration Variable",namesInFrame,namesInFrame[2]))
               
          }     
     })
     
     output$button <- renderUI({

          if(is.null(input$zeroOptSelect)){
               return(actionButton("updateRes","Calculate the Results"))
          } else {
               #if(zeros){
               if(values$zeros){
                    if(input$zeroOptSelect=="Ignore"){
                         return(actionButton("updateRes","Calculate the Results"))
                    } else if(input$zeroOptSelect=="Replace"){
                         if(input$zeroSub==""){
                              return(p("Please enter a value to replace observations <=0.", style="color:red"))
                         } else if(input$zeroSub!=""){
                              return(actionButton("updateRes","Calculate the Results"))
                         }
                    } else {
                         return(p("Please enter an option for handling values <=0.", style="color:red"))
                    }	
               }
               if(!values$zeros){
                    
                    return(actionButton("updateRes","Calculate the Results"))
               } 	
          }
     })
     
     output$DataTab <- renderTable({
          dataOrgZeroFixed()
     })
     
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

     observeEvent(input$updateRes, {
          updateTabsetPanel(session, "tabs", "Results")
          values$stamp <- format(Sys.time(), "%Y%m%d%H%M%S")
          
          withProgress({
               setProgress(message = "Please Wait")
               varFixed <- input$varFixed
               source("BVFunction2-0.R", local = TRUE)
               
               setProgress(detail = "Running analysis and creating pdf")
               pdffilename <- getPDFfilename()
               pdf(pdffilename, width = 9)
               par(mai=c(1,1,1,0.1))
               results <- fitBV.PLL (
                    BVdata=dataOrgZeroFixed(),
                    ECXvalue=input$ECXvalue,
                    fileName=NULL,
                    verbose=FALSE,
                    do3Dstart=FALSE,
                    FORCE=FALSE,
                    zeroSub=NULL,
                    varFixed=input$varFixed,
                    ylabel = input$ylab,
                    xlabel = input$xlab)
               grid.newpage()
               grid.table(dataOrgZeroFixed(), rows = NULL)
               #addtable2plot(.5,1,dataOrgZeroFixed(),xjust=.5,yjust=0)	 
               print("Check 1:  Sever finish fit")
               print(results)
               print(str(results))
               dev.off()
               setProgress(detail = "Creating table")
               print("Check 2:  Sever create table")
               resultsTable <- data.frame(EC.level.PCT = 100*results$EC.level, 
                                          ECx = as.numeric(to3(results$ECx)), 
                                          LCL.95 = ifelse(goodFlag,yes = as.numeric(to3(results$LCL.95)),no = NA), 
                                          UCL.95 = ifelse(goodFlag,yes = as.numeric(to3(results$UCL.95)),no = NA))
               print("Check 3:  Sever finished table")
               output$resultsTable <- shiny::renderTable(expr = {resultsTable},bordered = TRUE,na = "N/A")
               
               setProgress(detail = "Creating plot")
               
               print("Check 2:  Sever start plot")
               print(c(goodFlag=goodFlag,ECXvalue=input$ECXvalue))
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
                    if(values$zeros){
                         if(input$zeroOptSelect == "Ignore"){
                              negmessage <- "Values <= 0 have been ignored in the analysis."
                         } else if(input$zeroOptSelect == "Replace"){
                              negmessage <- paste0("Values <= 0 have been replaced with ", input$zeroSub, ".")
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
               png(cleanPlotFilename,height=6,width=8,units = "in",res = 200,type = "cairo")
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
               png(dirtyPlotFilename,height=6,width=8,units = "in",res = 200,type = "cairo")
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

          })
     })
     
     observeEvent(dataOrgZeroFixed(), {
          updateTabsetPanel(session,"tabs", "Data For Analysis")
     })
     
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
     
     output$sampleData <- renderTable({ 
          data.frame(y=c(120.9,118,134,121.2,118.6,120.4,82.6,62.8,81.6,49.3,41.6,41.3,12.7,14.7,14.7,4.93,4,4.4),
                          doses=c(0,0,0,5,5,5,10,10,10,20,20,20,40,40,40,80,80,80))
     }, include.rownames=FALSE)
})
