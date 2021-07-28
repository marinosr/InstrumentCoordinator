handle_PC_task <- function(command, samplename, sampleposition, serialin) {
  if (command$command == 'ASWAITREADY'){
    if(length(grep('READY', serialin$AS))==1){
      return(1)
    } else { return(0)}
  }
  if (command$command == 'PAUSE'){
    print(paste('Waiting', command$argument, 'seconds.'))
    Sys.sleep(command$argument*1000)
      return(1)
  }
}