library(shiny)
library(DT)
library(shinyFiles)
library(shinyBS)
library(processx)

source('./Source/write_log.R')

print(getwd())

file.copy('./Control/BatchControlDefaultState.dat', './Control/BatchControl.dat', overwrite=TRUE)
file.copy('./Control/InstrumentStatusDefaultState.dat', './Control/InstrumentStatus.dat', overwrite=TRUE)

px <- processx:::get_tool("px")

#Clears log and writes header.
{
  file.remove('./coordinator.log')
  file.create('./coordinator.log')
  file.remove('./batcherror.txt')
  file.create('./batcherror.txt')
  coordlog <- file('./coordinator.log', open='w')
  writeLines('Instrument Coordinator v.0.1
by Richard Marinos, 2021
GNU GPL v.3.0
_____________________________', con=coordlog)
  close(coordlog)
}

#Starts the backend as a separate process.
proc <- process$new(paste0(Sys.getenv('R_HOME'), '/bin/Rscript.exe'), c('--vanilla', paste0(getwd(), '/Source/backend_launcher.R'), getwd()), stderr = paste0(getwd(), '/batcherror.txt'), stdout=paste0(getwd(), '/batchout.txt'))



# Define UI ----
  ui <- fluidPage(
    
    #Make table and log scrolling, and make log always show bottom of log. 
    tags$style(
      "#batchtable {
            overflow: auto;
            max-height: 40vh;
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
                   bsTooltip('file', 'Loads a new .csv batch file, for running samples. The .csv must follow the format of Template.csv (with any additional columns as desired)'),
                   br(),
                   actionButton('edit', 'Edit'),
                   bsTooltip('edit', 'Edit the current batch file in Excel. This will stop the run until you click "Start" again. Do NOT edit any samples with timestamps (e.g. running/finished samples.)'),
                   br(),
                   actionButton('import', 'Import'),
                   bsTooltip('import', "Jeez write your own software if you need it that bad."),
                   br(),
                   br(),
                   strong('Run Control:'),
                   br(),
                   actionButton('run', 'Start'),
                   bsTooltip('run', "Start a sample run."),
                   br(),
                   actionButton('stop', 'Stop'),
                   bsTooltip('stop', "Stop/pause a sample run after the current sample finishes."),
                   br(),
                   splitLayout(actionButton('goto', 'Go to next...'), numericInput("gotonumber", label=NULL, value ='', width=80)),
                   bsTooltip('goto', "Set the number to the left as the next sample to run. This number is the INDEX of the sample in the table to the right."),
                   actionButton('kill', 'Kill'),
                   bsTooltip('kill', "Immediately halt execution of the current sample and stop the sample run."),
                   br(),
                   actionButton('exit', 'Exit'),
                   bsTooltip('exit', "Exit the Instrument Coordinator. This action writes out the logfile.  This button is inactive if a run is in progress."),
                   br(),
                   br(),
                   strong("Instrument Status:"),
                   htmlOutput('inststatus'),
                   bsTooltip('inststatus', "Status of devices connected to the instrument coordinator. The backend automatically tries to connect to devices and reports success here. "),
                   actionButton('reconnect', 'Reconnect Serial'),
                   bsTooltip('reconnect', "Closes and attempts to reconnect to serial devices."),
                   width=2
                   ),
      
      mainPanel(h3("Sample Table:"),
                div(style="display: inline-block;vertical-align:top;",'Run Status:'), 
                div(style="display: inline-block;vertical-align:top;", htmlOutput('CoordinatorStatus')),
                textOutput('BatchFile'),
                DTOutput("batchtable"),
                br(),
                h3('Log:'),
                div(style="display: inline-block;vertical-align:top;", checkboxInput("verboselog", label = "Verbose log output", value = FALSE)),
                div(style="display: inline-block;vertical-align:top;", actionButton('savelog', 'Save Log File')),
                verbatimTextOutput('log')
                
              
                 
                
                    
                
      )
    )
  )

  # Define server logic ----
  server <- function(input, output, session) {
    
    volumes <- c(Batches = 'C:/Users/Public/InstrumentCoordinator/Batches', C_Drive='C:/')
    shinyFileChoose(input, "file", roots = volumes, session = session)
    
    
    
    # Monitor changes to BatchControl.dat, and update reactive object if necessary
    control <- reactiveFileReader(1000, session=NULL, filePath='./Control/BatchControl.dat', readFunc=read.csv)
    inststatus <- reactiveFileReader(1000, session=NULL, filePath='./Control/InstrumentStatus.dat', readFunc=read.csv)
    
    #The observers below are for writing out desired system states to the backend, via batchcontrol.dat, so that it can respond appropriately. 
    
    #Start run observer
    StartObserver <- observeEvent(input$run, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      controlfile$Running[1] <- 1
      controlfile$FinishCurrentThenStop[1] <- 0
      controlfile$Kill[1] <- 0
      controlfile$Error[1] <- 0
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      write_log('GUI', 'Start run.')
    })
    
    #Killswitch Observer
    KillObserver <- observeEvent(input$kill, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      #Kill status observed by GUI and run_batch() 
      controlfile$Kill[1] <- 1
      controlfile$Running[1] <- 0
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      #Dummy file written as kill switch, observed by run_method() only.  
      write.csv(data.frame(a=1), './killswitch.dat')
      write_log('GUI','Kill immediately.')

    })
    
    #StopAfterCurrent
    StopObserver <- observeEvent(input$stop, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      controlfile$FinishCurrentThenStop[1] <- 1
      controlfile$NextSample <- NA
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      write_log('GUI','Stop after current sample.')
    })
    
    
    #Edit sample table. 
    EditObserver <- observeEvent(input$edit, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      controlfile$FinishCurrentThenStop[1] <- 1
      controlfile$NextSample <- NA
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      file.show(control()$BatchFile)
      write_log('GUI',"Open batch table for editing. WARNING: Do NOT edit any timestamped samples (running/already run samples.)")
    })
    
    
    VerboseObserver <- observeEvent(input$verboselog, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      if(input$verboselog==TRUE){
        controlfile$VerboseLog[1] <- 1
        write_log('GUI',"The option of verbose logging, which provides exquisitely granular detail of all backend activities, has been activated, per a duly executed request by the user, and shall continue until such time as the user requests that it be halted.")
      } else {
        controlfile$VerboseLog[1] <- 0
        write_log('GUI',"Verbose logging off.")
      }
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
    })
    
    #Write out log file
    SaveLogObserver <- observeEvent(input$savelog, {
      write_log('GUI',paste0("Logfile saved as ./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'))
      file.copy('./coordinator.log', paste0("./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'))
    })
    
    #Change sample table source.
    FileChangeObserver <- observe({
      #Parse new filename
      newfile <- parseFilePaths(volumes, input$file)
      #If a new file has been selected, update BatchControl.dat to tell the backend to load a new sample file. 
      if(length(newfile$datapath>0)){
        if(!(control()$BatchFile==newfile$datapath))
        {
          controlfile <- read.csv('./Control/BatchControl.dat')
          controlfile$BatchFile[1] <- newfile$datapath
          controlfile$LoadBatch[1] <- 1
          controlfile$Running[1] <- 0
          controlfile$FinishCurrentThenStop[1] <- 0
          controlfile$Kill[1] <- 0
          controlfile$Complete[1] <- 0
          write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
          write_log('GUI', paste("New batch table loaded at ", newfile$datapath))
        }
      }
    })
    
    #Skip to sample...
    GotoNextObserver <- observeEvent(input$goto, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      if(isolate(input$gotonumber) %in% 1:(dim(data())[1])){
        if(is.na(data()$timestamp[isolate(input$gotonumber)])){
          controlfile$NextSample <- isolate(input$gotonumber)
          write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
          write_log('GUI', paste("Next sample set as row #", isolate(input$gotonumber)))
        }
      }
      
    })
    
    ExitObserver <- observeEvent(input$exit, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      controlfile$Exit <- 1
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      write_log('GUI', 'Exit')
      file.copy('./coordinator.log', paste0("./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'))
      session$close()
    })
    
    ExitObserver <- observeEvent(input$reconnect, {
      controlfile <- read.csv('./Control/BatchControl.dat')
      controlfile$ReconnectSerial <- 1
      write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
      write_log('GUI', 'Serial connections closed and reinitialized.')
    })
    
    
    
    #Monitor changes to file pointed to by batchcontrol.dat and update as necessary. 
    data <- reactiveFileReader(1000, session=NULL, filePath=reactive(control()$BatchFile), readFunc=read.csv)
    
    #Reads for backend error messages and outputs them, sets backend flag to 0.
    backenderror <- reactiveFileReader(1000, session=NULL, filePath=reactive('./batcherror.txt'), readFunc=function(x){a <- file(x, open='r')
    b <- readLines(a)
    close(a)
    return(b)})
    
    observe({if(length(backenderror())>0){write_log('BAK', paste('CRASHED ON ERROR:',backenderror()))}})
    
    #Monitor changes to log file (e.g. writes by backend)
    statuslog <- reactiveFileReader(1000, session=NULL, filePath='./coordinator.log', readFunc=function(x){a <- file(x, open='r')
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
      ifelse(inststatus()$EA==1, "<DIV STYLE='color:green'><B>EA Serial</B></DIV><BR>", "<DIV STYLE='color:red'>EA Serial</DIV><BR>"),
      ifelse(inststatus()$Liaison==1, "<DIV STYLE='color:green'><B>Liaison Serial</B></DIV><BR>","<DIV STYLE='color:red'> Liaison Serial</DIV><BR>")
      )
    })
    
    #Determine current batch file loaded.
    output$BatchFile <- renderText({paste('Path:', control()$BatchFile)})
    
    #determine overall program status for printing in GUI
    output$CoordinatorStatus <- renderText({
      if(control()$Running[1]==1) {
        if(control()$FinishCurrentThenStop[1]==0){
          write_log('GUI', 'Running')
          '<DIV STYLE="background-color:green; color:white"><B>RUNNING</B></DIV><BR>'
        } else {
          write_log('GUI', 'Finish Current')
          '<DIV STYLE="background-color:green; color:white"><B>FINISHING CURRENT</B></DIV><BR>'
          
        }
      } else {
        if(control()$Kill[1]==1){
          write_log('GUI', 'Kill')
          '<DIV STYLE="background-color:black; color:white"><B>KILLED</B></DIV><BR>'
        } else if (control()$Error[1]==1){
          write_log('GUI', 'Stopped')
          '<DIV STYLE="background-color:yellow; color:black"><B>ERROR</B></DIV><BR>'
        } else {
          write_log('GUI', 'Stopped')
          '<DIV STYLE="background-color:red; color:white"><B>STOPPED</B></DIV><BR>'
        }
      }
    })
    
     #Determine status of all samples and render batch table. 
    output$batchtable <- renderDT({
      #Determine the status of all samples
      Status <- character(dim(data())[1])
      #Anything with a timestamp is complete... 
      Status[!(data()$timestamp=='')] <- 'COMPLETE'
      #...Except if it's running
      if(control()$Running==1){
        if(!(is.na(control()$CurrentSample))){
          Status[control()$CurrentSample] <- "RUNNING"
        }
        if(!(is.na(control()$NextSample))){
          Status[control()$NextSample] <- "NEXT"
        }
      } else if(control()$Kill==1) {
        if(!(is.na(control()$CurrentSample))){
          Status[control()$CurrentSample] <- "KILLED"
        }
      } else {
        if(!(is.na(control()$NextSample))){
          Status[control()$NextSample] <- "NEXT"
        }
      }
      
      
      
      #Bind the sample list and their statuses and output.
      cbind(Status, data())
    },
    options=list(pageLength=1000, lengthMenu=1000, searching=FALSE, paging=FALSE, ordering=FALSE))
    
    #Log text output.
    output$log <- renderText({statuslog()}, sep='\r\n')
    
    #Do the same stuff as pressing the exit button, if window closed out instead. 
    onStop(function(){
    controlfile <- read.csv('./Control/BatchControl.dat')
    controlfile$Exit <- 1
    write.csv(controlfile, './Control/BatchControl.dat', row.names = FALSE)
    write_log('GUI', 'Exit')
    file.copy('./coordinator.log', paste0("./Logs/", gsub(' ','',gsub(':','',format(Sys.time(), "%Y-%b-%d_%X"))), '-coordinator.log'))})
    
}

# Run the app ----
shinyApp(ui = ui, server = server)
