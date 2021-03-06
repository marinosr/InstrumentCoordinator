handle_PC_task <- function(command, samplename, sampleposition, serialin) {
  #Wait for the autosampler to say that it is ready for another command (after sending it WAITCOMPLETE (M400))
  if (command$command == 'ASWAITREADY'){
    if(length(grep('READY', serialin$AS))==1){
      return(0)
    } else { return(-1)}
  } else if (command$command == 'PAUSE'){
    Sys.sleep(command$argument)
      return(0)
  } else{
    (write_log('BAK', paste('Error in handle_PC_task(): PC task', command, 'is not a valid command.')))
    return(1)
    }
}