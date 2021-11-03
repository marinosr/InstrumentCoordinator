#This just loads the necessary functions, given their proper paths by the UI, and launches the batch manager backend. 

args = commandArgs(trailingOnly=TRUE)
setwd(args)

source('./Source/parse_method.R')
source('./Source/handle_PC_task.R')
source('./Source/translate_to_AS_gcode.R')
source('./Source/translate_to_LIA_code.R')
source('./Source/run_method.R')
source('./Source/run_batch.R')
source('./Source/throw_error.R')
source('./Source/write_log.R')
source('./Source/serial_commands.R')

run_batch()

