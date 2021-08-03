handle_PC_task <- function(command, samplename, sampleposition, serialin) {
  #Wait for the autosampler to say that it is ready for another command (after sending it WAITCOMPLETE (M400))
  if (command$command == 'ASWAITREADY'){
    if(length(grep('READY', serialin$AS))==1){
      return(1)
    } else { return(0)}
  } else if (command$command == 'PAUSE'){
    Sys.sleep(command$argument*1000)
      return(1)
  } else(write_log('BAK', paste('PC task', command, 'is not a valid command.')))
}