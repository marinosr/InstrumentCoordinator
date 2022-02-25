run_method <- function(methodpath, #an absolute path to the method file location
                       serialconnections, #List of serial connections as initialized by run_batch()
                       sampleposition=0, #Argument used if AS is being used, for sample # lookup. 
                       verbose=0){  #Used if the autosampler has to look up a position.
  
  #Load method.
  method <- parse_method(methodpath)
  if(is.null(method)){ #Return error flag to run_batch() if method can't be parsed. Careful this is silent and very hard to debug.
    return(1)
  }
  
  #Immediate killswitch triggered by the creation of killswitch.dat (Note: The Kill flag in BatchControl.dat is only used by the GUI.)
  killflag <- 0
  errorflag <- 0
  samplestatus <- 0
  commandlogged <- 0
  
  busy <- FALSE
  
  
  #If autosampler needed, load position coordinate information. 
  if('AS' %in% method$SEQUENCE$device){
    locations <- read.csv(method$LOCATIONS, sep=',', strip.white = TRUE, blank.lines.skip=TRUE, comment.char='#')
  }
  
  if('SHM' %in% method$SEQUENCE$device){
    AutoItLocations <- read.csv(method$AUTOITSCRIPTS, sep=',', strip.white = TRUE, blank.lines.skip=TRUE, comment.char='#')
  }
  
  #Grab start time, in seconds.
  starttime <- as.numeric(Sys.time())
  
  #Flag for whether a sequence step has been completed
  method$SEQUENCE$completed <- 0
  
  neededserial <- method$SEQUENCE$device[method$SEQUENCE$device %in% c('AS','LIA','EA', 'IGA')]
  notfoundserial <- neededserial[!(neededserial %in% names(serialconnections))]
  if(length(notfoundserial)>1){
    write_log('BAK', paste('The specified method requires the devices:', notfoundserial, 'but no connections to the devices exist.'))
    errorflag <- 1
  }
  
  #Iterate while there are uncompleted steps in the method.
  while((0 %in% method$SEQUENCE$completed) && errorflag==0) {
    
    #Allows another program to kill method sequence by creating killsignal.dat
    if(file.exists('./killsignal.dat')){
      file.remove('./killsignal.dat')
      write_log('BAK', 'Sequence killed by user request.')
      errorflag <- 1
      killflag <- 1
      samplestatus <- 1
    }
    
    #Insert code to read and write serial buffers here. 
    serialin <- read_serial_connections(serialconnections)
    
    if('AS' %in% names(serialin)){
      if(length(grep("ok", serialin$AS))>0){
        busy <- FALSE
      }
    }
    
    #If verbose logging selected, write out any received serial communication to the log.  
    if(verbose==1){
      mapply(function(name, reply) {if(nchar(reply)>0){
        write_log(name, paste('Rx:', name, ' - ', reply))
        }
        }, names(serialin), serialin)
    }
    
    
    #Clear the serial out buffer. 
    serialout=list()
    
    #Determine index of next step. 
    nextstep <- which(method$SEQUENCE$completed==0)[1]
    #See if it's time to run the next step yet. 
    if(busy==FALSE){
    if((as.numeric(Sys.time())-starttime) >= method$SEQUENCE$t[nextstep]){
      
      #Write out method line if verbose logging enabled. 
      if(verbose==1){
        if(commandlogged == 0){
        write_log('BAK',(paste(method$SEQUENCE[nextstep,], collapse=', ')))
        commandlogged <- 1
        }
      }
      
      #Handling for the next method line sending a command to the autosampler. 
      if(method$SEQUENCE$device[nextstep]=='AS'){
        #Formulate gcode command to send to AS
        serialout$AS <- translate_to_AS_gcode(command = method$SEQUENCE[nextstep,], locations=locations, sampleposition=sampleposition, method=method)
        busy <- TRUE
        #If translate_to_AS_gcode() fails, it should return NULL.
        if(is.null(serialout$AS)){errorflag <- 1}

        
      #Handling for PC tasks done through R. 
      } else if(method$SEQUENCE$device[nextstep]=='PC'){
        PCdone <- handle_PC_task(command = method$SEQUENCE[nextstep,], samplename=samplename, sampleposition=sampleposition, serialin=serialin)
        if(PCdone==0){
        } else if (PCdone==1) {
          errorflag <- 1
        }
        
        #Handling for the next method line interfacing with the Shimadzu. 
      } else if (method$SEQUENCE$device[nextstep]=='SHM') {
        autoitpath <- AutoItLocations$Value[AutoItLocations$Key=='AUTOITPATH']
        autoitscript <- paste0(getwd(), AutoItLocations$Value[AutoItLocations$Key==method$SEQUENCE[nextstep,command]])
        print(autoitscript)
        processreturn <- processx::run(autoitpath, autoitscript)
        if(!(processreturn$status==0)){
          errorflag <- 1
        }
      }
        
      } else if (method$SEQUENCE$device[nextstep]=='PIC') {
      } else if (method$SEQUENCE$device[nextstep]=='EA') {
      } else if (method$SEQUENCE$device[nextstep]=='LIA') {
        serialout$LIA <- translate_to_LIA_code(method$SEQUENCE[nextstep,])
        if(is.null(serialout$AS)){errorflag <- 1}
      }
      
      write_serial_connections(serialout, serialconnections)
      
      #If the command is a PC command, wait to see if the command is finished. Otherwise assume that the command executed and move on. 
      if(errorflag==0){
        if(method$SEQUENCE$device[nextstep]=='PC'){
          if(PCdone==0){
            method$SEQUENCE$completed[nextstep] <- 1
            commandlogged <-0
          }
        }else {
        method$SEQUENCE$completed[nextstep] <- 1
        commandlogged <- 0
        }
      }
    }
    
  
    if(verbose==1){
      if(length(serialout)>0){
      mapply(function(name, reply) {if(nchar(reply)>0){
        write_log(name, paste('Tx:', name, ' - ', reply))
      }
      }, names(serialout), serialout)
      }
    }

    
    
  Sys.sleep(.01)
  
  
  #If killed by user or error, throw non-zero exit status. 
  if(errorflag == 1) {samplestatus <- 1}
  if(killflag == 1) {samplestatus <- 1}
  }
  
  return(samplestatus)
}
