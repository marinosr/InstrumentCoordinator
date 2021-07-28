run_batch <- function(){
  
  keepgoing <- TRUE
  
  while(keepgoing){
  #Read in control parameters.
  control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
  control.original <- control
  
  #Load batch file if instructed to.
  if(control$LoadBatch==1){
  batch <- read.csv(control$BatchFile, comment.char = '#', strip.white=TRUE, blank.lines.skip = TRUE)
  control$LoadBatch <- 0
  control$CurrentSample <- 1
  }
  
  #Set current sample to the lowest sample index that doesn't have a timestamp, past the current sample, 
  #but override if control says to skip to a different sample. 
  #If no more samples to run, set running flag to 0.
  if(length(which(which(is.na(batch$timestamp)) >= control$CurrentSample)) > 0){
  control$CurrentSample <- min(which(which(is.na(batch$timestamp)) >= control$CurrentSample))
  if(control$MoveToSample>0){
    control$CurrentSample <- control$MoveToSample
    control$MoveToSample <- 0
  }
  }else {
    control$Running <- 0
  }
  
  #If running, run the method on the current sample. Write timestamp to batch table, and try to write out batch file, with error recovery.
  if(control$Running==1){
    batch$data[control$CurrentSample] <- run_method(methodpath=batch$method, samplename=paste(batch$ID,batch$name,sep='-'), sampleposition=batch$position)
    batch$timestamp[control$CurrentSample] <- Sys.time()
    tryCatch(write.csv(batch, control$BatchFile), error=function(cond){message('Warning: Error accessing batch file CSV. Is the batch file open in another program? Close it! Timestamps will be written on next iteration.')})
  }
  
  #If exit triggered, stop loop.
  if(control$Exit==1){
    keepgoing <- FALSE
  }
  
  #Write out modified control parameters. 
  if(!(control.original==control)){
  write.csv(control, './Control/BatchControl.dat')
  }
  #Pause 1 second to not just crank on the CPU.
  Sys.sleep(1000)
  }
}
  


