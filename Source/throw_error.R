throw_error <- function(message){
write_log(message)
control <- read.csv('./Control/BatchControl.dat', comment.char = '#')
control$Error <- 1
control$Running <- 0
write.csv(control, './Control/BatchControl.dat')
}