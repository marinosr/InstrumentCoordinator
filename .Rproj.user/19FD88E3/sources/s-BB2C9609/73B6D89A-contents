write_log <- function(device, message){
  logfile <- file('../coordinator.log', open='a')
  writeLines(paste(device,Sys.time(), ": ", message), logfile)
  close(logfile)
}