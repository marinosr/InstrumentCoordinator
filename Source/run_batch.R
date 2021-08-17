#This function moves through the sample list loaded through the GUI.

run_batch <- function(){
  
  #Clear exit flag from last time program ran.
  control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
  control$Exit <- 0
  write.csv(control, './Control/BatchControl.dat', row.names=FALSE)
  
  
  #Initialize serial connections
  serialconns <- check_serial_connections(open_serial_connections(read.csv('./Control/serialconnections.csv')))
  
  keepgoing <- TRUE
  
  while(keepgoing){
    
    
    #Set Backend active flag.
    instruments <- read.csv('./Control/InstrumentStatus.dat')
    if(instruments$Backend == 0){
      instruments$Backend <- 1
      write.csv(instruments, './Control/InstrumentStatus.dat', row.names=FALSE)
      write_log('BAK','Sequencing backend online.')
    }
    
    #Read in control parameters.
    control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
    #Make copy of control parameters at start of loop for later comparison. 
    control.original <- control
    
    #Load batch file if instructed to, then revert sample # to 0, and clear control parameters. 
    if(control$LoadBatch==1){
      batch <- read.csv(control$BatchFile, comment.char = '#', strip.white=TRUE, blank.lines.skip = TRUE)
      control$LoadBatch <- 0
      control$CurrentSample <- 0
      control$NextSample <- NA
      control$Running <- 0
      control$Complete <- 0
      control$Error <- 0
      control$Kill <- 0
      control$FinishCurrentThenStop <- 0 
    } else {
      #Otherwise just read the batch file in. (This imports the timestamps of the latest samples run.)
      batch <- read.csv(control$BatchFile, comment.char = '#', strip.white=TRUE, blank.lines.skip = TRUE)
    }
    
    if(control$ReconnectSerial==1){
      close_serial_connections()
      serialconns <- open_serial_connections()
      control$ReconnectSerial <- 0
    }
    
    #If there's a batch table to operate on...
    if(exists('batch')){
      unrunsamples <- which(is.na(batch$timestamp))
      needtorun <- unrunsamples[unrunsamples >= control$CurrentSample]
      #Go to the next sample if one is specified
      if(!is.na(control$NextSample)) {
        control$CurrentSample <- control$NextSample
        control$NextSample <- NA
      } else if(length(needtorun)==0) {
        print('Conditional 2')
        #Or throw the complete flag if there are no more samples without timestamps, which occur past the current sample. (This conditional lets you skip samples and not have the autosampler cycle back to run them)
        control$CurrentSample <- NA
        control$NextSample <- NA
        control$Running <- 0
        control$Complete <- 1
        tryCatch(write.csv(batch, control$BatchFile, row.names=FALSE), error=function(cond){write_log('BAK','The timestamp for the final sample in the completed batch could not be written. Was the CSV open?')})
      } else {
        #Otherwise move to the next sample that doesn't have a timestamp.
        #This should dwell the loop on the current sample until it is run and issued a timestamp.
        control$CurrentSample <- min(needtorun)
      }
    }
    
    
    #Write out changes to batch file. 
    if(FALSE %in% (control.original==control)){
      write.csv(control, './Control/BatchControl.dat', row.names=FALSE)
      control.original <- control
    }
    
    
    #If running, run the method on the current sample. Write timestamp to batch table, and try to write out batch file, with error recovery.
    if(control$Running==1){
      print(control)
      message <- paste0('Running row #', control$CurrentSample, 
                        ' (ID:',
                        batch$ID[control$CurrentSample],
                        ' Name:',
                        batch$name[control$CurrentSample],
                        ') in position',
                        batch$position[control$CurrentSample],
                        ' by method ',
                        batch$method[control$CurrentSample]
      )
      print(message)
      write_log('BAK', message)
      samplestatus <- run_method(methodpath=batch$method[control$CurrentSample], 
                                 sampleposition=ifelse(is.na(batch$position[control$CurrentSample]), 0, batch$position[control$CurrentSample]),
                                 verbose=control$VerboseLog)
      print(paste('Sample Status', samplestatus))
      
      
      
      
      #Timestamp the result if method completed successfully. Otherwise raise error flag for the GUI. 
      if(samplestatus==0){
        batch$timestamp[control$CurrentSample] <- Sys.time()
        write_log('BAK', paste ('Row #', control$CurrentSample, 'complete.'))
      } else if (samplestatus==1){
        #throw_error('BAK','Run stopped on error.')
      }
      
      
      
      #Check to see if "FinishCurrentThenStop was thrown by GUI while run_method() was running. 
      control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
      if(control$FinishCurrentThenStop == 1){
        control$Running <- 0
        control$FinishCurrentThenStop <- 0
        write.csv(control, './Control/BatchControl.dat', row.names = FALSE)
      }
      
      tryCatch(write.csv(batch, control$BatchFile, row.names=FALSE), 
               error=function(cond){write_log('BAK','The timestamp could for the current sample could not be written. Is the .csv open in another program? Will try again after next sample.')})
    } else {getwd
    }
    
    
    
    if(control$Exit==1){
      close_serial_connections()
      keepgoing <- FALSE
    }
    
    #Pause 1 second to not just crank on the CPU.
    Sys.sleep(3)
  }
}



