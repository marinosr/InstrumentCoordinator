throw_error <- function(device, message){
write_log(device, message)
control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
control$Error <- 1
control$Running <- 0
print('Error Thrown')
write.csv(control, './Control/BatchControl.dat', row.names = FALSE)
}