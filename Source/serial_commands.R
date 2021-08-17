suppressMessages(library(serial))

open_serial_connections <- function(serialparams){

write_log('BAK', paste('Initializing serial connections:', serialparams$name[serialparams$port %in% listPorts()]))

#Create any serial connections whose COM port is listed. 
serialconns <- tryCatch({
serialconns <- apply(serialparams[serialparams$port %in% listPorts(),], 1, function(x){serialConnection(name=x['name'], port=x['port'], mode=x['mode'])})
lapply(serialconns, open)
names(serialconns) <- serialparams$name[serialparams$port %in% listPorts()]
return(serialconns)
}, error=function(cond){write_log('BAK', paste('Initialization of serial ports failed on error: ', cond, 'Suggest you check connections.'))
  return(NULL)})

return(serialconns)
}


#This needs to be expanded in the future to do handshaking. 
check_serial_connections <- function(serialconns){
  statuses <- read.csv('./Control/InstrumentStatus.dat')
  
  #Statuses for all serial connected instruments should be 0 by default. 
  statuses[,c('Autosampler','Liaison','PPSys','EA')] <- 0
  opennames <- ''

  if('AS' %in% names(serialconns)){
    if(isOpen(serialconns$AS)){
      statuses$Autosampler <- 1
      opennames <- c(opennames, 'AS')

    }
  }
  if('LIA' %in% names(serialconns)){
    if(isOpen(serialconns$LIA)){
      statuses$Liaison <- 1
      opennames <- c(opennames, 'LIA')
    }
  }
  if('EA' %in% names(serialconns)){
    if(isOpen(serialconns$EA)){
      statuses$EA <- 1
      opennames <- c(opennames, 'EA')
    }
  }
  if('IGA' %in% names(serialconns)){
    if(isOpen(serialconns$IGA)){
      statuses$PPSys <- 1
      opennames <- c(opennames, 'IGA')
    }
  }
  
  write.csv(statuses, './Control/InstrumentStatus.dat')
  
  #Return updated list of open serial connections. 
  return(serialconns[names(serialconns) %in% opennames])
}

read_serial_connections <- function(serialconns){
  rxmessages <- lapply(serialconns, read.serialConnection)
  names(rxmessages) <- names(serialconns)
  return(rxmessages)
}

write_serial_connections <- function(txmessages, serialconns){
  mapply(function(message,name,serialconnections){
  write.serialConnection(serialconnections[[which(names(serialconnections)==name)]], paste0(message,'\n'))
  }, txmessages, names(txmessages), MoreArgs=list(serialconnections=serialconns))
}

close_serial_connections <-function(serialconns){
  lapply(serialconns, close)  
}








