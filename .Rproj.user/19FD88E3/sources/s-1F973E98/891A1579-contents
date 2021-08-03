#This function moves through the sample list loaded through the GUI.

run_batch <- function(){
  
  keepgoing <- TRUE
  
  while(keepgoing){
    
  #NEED CODE FOR SERIAL CONNECTION HERE.
    
  #Set Backend active flag.
  inststatus <- read.csv('./InstrumentStatus.dat')
  inststatus$Backend <- 1
  write.csv(inststatus, './InstrumentStatus.dat')
    
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
  }
  
  #Go to the next sample if one is specified
  if(!is.na(control$NextSample)) {
      control$CurrentSample <- control$NextSample
      control$NextSample <- NA
  } else if(length(which(which(is.na(batch$timestamp)) >= control$CurrentSample)) == 0) {
    #Or throw the complete flag if there are no more samples without timestamps, which occur past the current sample. (This conditional lets you skip samples and not have the autosampler cycle back to run them)
    control$CurrentSample <- NA
    control$NextSample <- NA
    control$Running <- 0
    control$Complete <- 1
    tryCatch(write.csv(batch, control$BatchFile), error=function(cond){write_log('BAK','The timestamp for the final sample in the completed batch could not be written. Was the CSV open?')})
  } else {
    #Otherwise move to the next sample that doesn't have a timestamp.
    #This should dwell the loop on the current sample until it is run and issued a timestamp.
    control$CurrentSample <- min(which(which(is.na(batch$timestamp)) >= control$CurrentSample))
  }

  #Write out changes to batch file. 
  if(!(control.original==control)){
    write.csv(control, './Control/BatchControl.dat')
    control.original <- control
  }

  #If running, run the method on the current sample. Write timestamp to batch table, and try to write out batch file, with error recovery.
  if(control$Running==1){
    write_log('BAK', paste0('Running row #', control$CurrentSample, ' (ID:', batch$ID[control$CurrentSample], ' Name:', batch$name[control$CurrentSample],') in position', batch$position[control$CurrentSample], ' by method ', batch$method[control$CurrentSample]))
    samplestatus <- run_method(methodpath=batch$method[control$CurrentSample], 
                               sampleposition=ifelse(is.na(batch$position[control$CurrentSample]), 0, batch$position[control$CurrentSample]),
                               verbose=control$VerboseLog)
    
    #Timestamp the result if method completed successfully. Otherwise raise error flag for the GUI. 
    if(samplestatus==0){
    batch$timestamp[control$CurrentSample] <- Sys.time()
    write_log('BAK', paste ('Row #', control$CurrentSample, 'complete.'))
    } else if (samplestatus=1){
      throw_error('BAK','Run stopped on error.')
    }
    
    #Check to see if "FinishCurrentThenStop was thrown by GUI while run_method() was running. 
    control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
    if(control$FinishCurrentThenStop == 1){
      control$Running <- 0
      control$FinishCurrentThenStop <- 0
      write.csv(control, './Control/BatchControl.dat')
    }
                        
    tryCatch(write.csv(batch, control$BatchFile), 
             error=function(cond){write_log('BAK','The timestamp could for the current sample could not be written. Is the .csv open in another program? Will try again after next sample.')})
  }
  

  
  if(control$Exit==1){
    keepgoing <- FALSE
  }
  
  #Pause 1 second to not just crank on the CPU.
  Sys.sleep(1000)
  }
}
  


