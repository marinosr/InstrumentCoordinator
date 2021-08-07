#This just loads the necessary functions, given their proper paths by the UI, and launches the batch manager backend. 

args = commandArgs(trailingOnly=TRUE)
setwd(args)

source('./parse_method.R')
source('./handle_PC_task.R')
source('./translate_to_AS_gcode.R')
source('./run_method.R')
source('./run_batch.R')
source('./throw_error.R')
source('./write_log.R')

run_batch()

