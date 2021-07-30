library(shiny)
library(DT)
library(shinyFiles)


# Define UI ----
  ui <- fluidPage(
    
    tags$style(
      "#control {
            overflow: auto;
            max-height: 50vh;
        }
      #log {
        overflow: auto;
        display:flex; 
        flex-direction:column-reverse;
        max-height: 20vh;
      }"
    ),
    
    titlePanel("Instrument Coordinator"),
    
    sidebarLayout(
      sidebarPanel(strong('Sample Table:'),
                   br(),
                   shinyFilesButton('file', label = 'Load', title=paste('Load a new sample list:'), multiple=FALSE, filetypes='.csv'),
                   br(),
                   actionButton('edit', 'Edit'),
                   br(),
                   actionButton('import', 'Import'),
                   br(),
                   br(),
                   strong('Run Control:'),
                   br(),
                   actionButton('run', 'Start'),
                   br(),
                   actionButton('stop', 'Stop'),
                   br(),
                   div(style="display: inline-block;vertical-align:top;", actionButton('goto', 'Go to next...')),
                   div(style="display: inline-block;vertical-align:top;", numericInput("gotonumber", label='', value ='', width=80)),
                   br(),
                   actionButton('kill', 'Kill'),
                   br(),
                   actionButton('exit', 'Exit'),
                   br(),
                   br(),
                   strong("Instrument Status:"),
                   htmlOutput('inststatus'),
                   width=2
                   ),
      
      mainPanel(h3("Sample Table:"),
                DTOutput("control"),
                br(),
                h3('Log:'),
                div(style="display: inline-block;vertical-align:top;", checkboxInput("verboselog", label = "Verbose log output", value = TRUE)),
                div(style="display: inline-block;vertical-align:top;", actionButton('savelog', 'Save Log File')),
                verbatimTextOutput('log'),
                br()
                 
                
                    
                
      )
    )
  )

  # Define server logic ----
  server <- function(input, output, session) {
    
    volumes <- c(Batches = 'C:/Users/Public/InstrumentCoordinator/Batches', C_Drive='C:/')
    shinyFileChoose(input, "file", roots = volumes, session = session)
    
    
    
    
    # Monitor changes to BatchControl.dat, and update reactive object if necessary
    control <- reactiveFileReader(1000, session=NULL, filePath='./BatchControl.dat', readFunc=read.csv)
    inststatus <- reactiveFileReader(1000, session=NULL, filePath='./InstrumentStatus.dat', readFunc=read.csv)
    
    #The observers below are for writing out desired system states to the backend, via batchcontrol.dat, so that it can respond appropriately. 
    
    #Start run observer
    StartObserver <- observeEvent(input$run, {
      controlfile <- read.csv('./Batchcontrol.dat')
      controlfile$Running[1] <- 1
      controlfile$FinishCurrentThenStop[1] <- 0
      controlfile$Kill[1] <- 0
      write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
      logfile <- file('./coordinator.log', open='a')
      writeLines(paste('\nGUI',Sys.time(), ": Start run."), logfile)
      close(logfile)
    })
    
    #Killswitch Observer
    KillObserver <- observeEvent(input$kill, {
      controlfile <- read.csv('./Batchcontrol.dat')
      controlfile$Kill[1] <- 1
      controlfile$Running[1] <- 0
      write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
      logfile <- file('./coordinator.log', open='a')
      writeLines(paste('\nGUI',Sys.time(), ": Kill immediately."), logfile)
      close(logfile)
    })
    
    #StopAfterCurrent
    StopObserver <- observeEvent(input$stop, {
      controlfile <- read.csv('./Batchcontrol.dat')
      controlfile$FinishCurrentThenStop[1] <- 1
      controlfile$NextSample <- NA
      write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
      logfile <- file('./coordinator.log', open='a')
      writeLines(paste('\nGUI',Sys.time(), ": Stop after current sample."), logfile)
      close(logfile)
    })
    
    
    #Edit sample table. 
    EditObserver <- observeEvent(input$edit, {
      controlfile <- read.csv('./Batchcontrol.dat')
      controlfile$FinishCurrentThenStop[1] <- 1
      controlfile$NextSample <- NA
      write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
      file.show(control()$BatchFile)
      logfile <- file('./coordinator.log', open='a')
      writeLines(paste('\nGUI',Sys.time(), ": Open batch table for editing. This stops the run after the current sample. \n WARNING: Do NOT edit any timestamped samples (running/already run samples)."), logfile)
      close(logfile)
    })
    
    #Write out log file
    SaveLogObserver <- observeEvent(input$savelog, {
      logfile <- file('./coordinator.log', open='a')
      writeLines(paste0('\nGUI ',Sys.time(), " : Logfile saved as ./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'), logfile)
      file.copy('./coordinator.log', paste0("./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'))
      close(logfile)
    })
    
    #Change sample table source.
    FileChangeObserver <- observe({
      #Parse new filename
      newfile <- parseFilePaths(volumes, input$file)
      #If a new file has been selected, update BatchControl.dat to tell the backend to load a new sample file. 
      if(length(newfile$datapath>0)){
        if(!(control()$BatchFile==newfile$datapath))
        {
          controlfile <- read.csv('./Batchcontrol.dat')
          controlfile$BatchFile[1] <- newfile$datapath
          controlfile$LoadBatch[1] <- 1
          controlfile$Running[1] <- 0
          controlfile$FinishCurrentThenStop[1] <- 0
          controlfile$Kill[1] <- 0
          write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
          logfile <- file('./coordinator.log', open='a')
          writeLines(paste('\nGUI',Sys.time(), ": New batch table loaded at ", newfile$datapath), logfile)
          close(logfile)
        }
      }
    })
    
    #Skip to sample...
    GotoNextObserver <- observeEvent(input$goto, {
      controlfile <- read.csv('./Batchcontrol.dat')
      if(isolate(input$gotonumber) %in% 1:(dim(data())[1])){
        if(!is.na(data()$timestamp[isolate(input$gotonumber)])){
          controlfile$NextSample <- isolate(input$gotonumber)
          write.csv(controlfile, './Batchcontrol.dat', row.names = FALSE)
        }
      }
      
    })
    
    
    
    #Monitor changes to file pointed to by batchcontrol.dat and update as necessary. 
    data <- reactiveFileReader(1000, session=NULL, filePath=reactive(control()$BatchFile), readFunc=read.csv)
    
    #Monitor changes to log file (e.g. writes by backend)
    statuslog <- reactiveFileReader(1000, session=NULL, filePath='./coordinator.log', readFunc=function(x){a <- file('./coordinator.log', open='r')
      b <- readLines(a)
      close(a)
      return(b)})
    
    #Determine instrument status
    output$inststatus <- renderText({
      paste(
      ifelse(inststatus()$Backend==1, "<DIV STYLE='color:green'><B>Coordinator Backend</B></DIV><BR>", "<DIV STYLE='color:red'>Controller Backend</DIV><BR>"),
      ifelse(inststatus()$Autosampler==1, "<DIV STYLE='color:green'><B>Autosampler Serial</B></DIV><BR>", "<DIV STYLE='color:red'>Autosampler Serial</DIV><BR>"),
      ifelse(inststatus()$Shimadzu==1, "<DIV STYLE='color:green'><B>Shimadzu Software</B></DIV><BR>", "<DIV STYLE='color:red'>Shimadzu Software</DIV><BR>"),
      ifelse(inststatus()$Picarro==1, "<DIV STYLE='color:green'><B>Picarro Software</B></DIV><BR>", "<DIV STYLE='color:red'>Picarro Software</DIV><BR>"),
      ifelse(inststatus()$PPSys==1, "<DIV STYLE='color:green'><B>PP Systems IRGA Serial</B></DIV><BR>", "<DIV STYLE='color:red'>PP Systems IRGA Serial</DIV><BR>"),
      ifelse(inststatus()$EA==1, "<DIV STYLE='color:green'><B>EA Serial</B></DIV><BR>", "<DIV STYLE='color:red>EA Serial</DIV><BR>"),
      ifelse(inststatus()$Liaison==1, "<DIV STYLE='color:green'><B>Liaison Serial</B></DIV><BR>", "<DIV STYLE='color:red'>Liaison Serial</DIV><BR>")
      )
    })
    
    #Determine status of all samples and render batch table. 
    output$control <- renderDT({
      #Determine the status of all samples
      Status <- character(dim(data())[1])
      #Anything with a timestamp is complete... 
      Status[!(data()$timestamp=='')] <- 'COMPLETE'
      #...Except if it's running
      if(control()$Running==1){
        Status[control()$CurrentSample] <- "RUNNING"
        if(!(is.na(control()$NextSample))){
          Status[control()$NextSample] <- "NEXT"
        }
      } else if(control()$Kill==1) {
        Status[control()$CurrentSample] <- "KILLED"
      }else {
        if(!(is.na(control()$NextSample))){
          Status[control()$NextSample] <- "NEXT"
        }
      }
      
      #Bind the sample list and their statuses and output.
      cbind(Status, data())
    },
    options=list(pageLength=1000, lengthMenu=1000, searching=FALSE, paging=FALSE, ordering=FALSE))
    
    output$log <- renderText({statuslog()}, sep='\r\n')
  

    
  }

# Run the app ----
shinyApp(ui = ui, server = server)
