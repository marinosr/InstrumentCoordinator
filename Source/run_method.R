run_method <- function(methodpath, #an absolute path to the method file location
                       serialconnections, #List of serial connections as initialized by run_batch()
                       sampleposition=0, #Argument used if AS is being used, for sample # lookup. 
                       verbose=0){  #Used if the autosampler has to look up a position.
  
  #Load method.
  method <- parse_method(methodpath)
  if(is.null(method)){ #Return error flag to run_batch() if method can't be parsed. 
    return(1)
  }
  #Immediate killswitch triggered by the creation of killswitch.dat (Note: The Kill flag in BatchControl.dat is only used by the GUI.)
  killflag <- 0
  errorflag <- 0
  samplestatus <- 0
  
  
  #If autosampler needed, load position coordinate information. 
  if('AS' %in% method$SEQUENCE$device){
    locations <- read.csv(method$LOCATIONS, sep='', strip.white = TRUE, blank.lines.skip=TRUE, comment.char='#')
  }
  
  #Grab start time, in seconds.
  starttime <- as.numeric(Sys.time())
  
  #Flag for whether a sequence step has been completed
  method$SEQUENCE$completed <- 0
  
  # #Check if serial connections exist and are active.
  # {
  # if('AS' %in% method$device){
  #   if(!exists(serialconnections$AS)){
  #     error('Method requires autosampler, but no serial connection to autosampler exists.')
  #   }
  #   if(!serial::isOpen(serialconnections$AS)){
  #     error('The serial connection to the autosampler is closed.') 
  #   }
  # }
  # if('LIA' %in% method$device){
  #   if(!exists(serialconnections$AS)){
  #     error('Method requires liaison, but no serial connection to liaison exists.')
  #   }
  #   if(!serial::isOpen(serialconnections$AS)){
  #     error('The serial connection to the liaison is closed.') 
  #   }
  # }
  # if('EA' %in% method$device){
  #   if(!exists(serialconnections$AS)){
  #     error('Method requires EA, but no serial connection to EA exists.')
  #   }
  #   if(!serial::isOpen(serialconnections$AS)){
  #     error('The serial connection to the EA is closed.') 
  #   }
  # }
  # }
  
  
  #Iterate while there are uncompleted steps in the method.
  while((0 %in% method$SEQUENCE$completed) & errorflag==0) {
   
    #Allows another program to kill method sequence by creating killsignal.dat
     if(file.exists('./killsignal.dat')){
       file.remove('./killsignal.dat')
       write_log('BAK', 'Sequence killed by user request.')
       errorflag <- 1
       killflag <- 1
       samplestatus <- 1
       }
    
    #Insert code to read serial buffers here. 
    serialin=list()
  
    #If verbose logging selected, write out any received serial communication to the log.  
    if(verbose=1){
      mapply(function(name, command) {write_log(name, paste('Rx:',command))}, names(serialin), serialin)
    }
    
    
    #Clear the serial out buffer. 
    serialout=list()
    
    #Determine index of next step. 
    nextstep <- which(method$SEQUENCE$completed==0)[1]
    #See if it's time to run the next step yet. 
    if((as.numeric(Sys.time())-starttime)<= method$SEQUENCE$t[nextstep]){
      
      if(method$SEQUENCE$device[nextstep]=='AS'){
        #Formulate gcode command to send to AS
        serialout$AS <- translate_to_AS_gcode(command = method$SEQUENCE[nextstep,], locations=locations, sampleposition=sampleposition, method=method)
        if(is.null(serialout$AS)){errorflag <- 1}
        }
      } else if(method$SEQUENCE$device[nextstep]=='PC'){
        PCdone <- handle_PC_task(command = method$SEQUENCE[nextstep,], samplename=samplename, sampleposition=sampleposition, serialin=serialin)
        if(PCdone==0){
          method$SEQUENCE$completed[nextstep] <- 1
        } else if (PCdone==1) {
          errorflag <- 1
        }
      } else if (method$SEQUENCE$device[nextstep]=='SHM') {
        print ('No Shimadzu commands implemented yet')
      } else if (method$SEQUENCE$device[nextstep]=='PIC') {
        print ('No Picarro commands implemented yet')
      } else if (method$SEQUENCE$device[nextstep]=='EA') {
      } else if (method$SEQUENCE$device[nextstep]=='LIA') {
        print ('No liaison commands implemented yet')
      }
      
      method$SEQUENCE$completed[nextstep] <- 1
    }
  
  if(errorflag == 1) {samplestatus <- 1}
  if(killflag == 1) {samplestatus <- 1}
  return(samplestatus)
}
