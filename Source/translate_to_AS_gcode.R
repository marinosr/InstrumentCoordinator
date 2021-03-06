translate_to_AS_gcode <- function(command, locations, sampleposition, method){

#This is a kludge from when command$argument was always numeric.Necessary for lookup/comparisons below. 
if(!is.na(as.numeric(command$argument))){
      command$argument <- as.numeric(command$argument)
}
  
  #lookup x y z coordinates (if necessary)
  
  #Initialize coordinates as dummy something so can test if coordinates have been obtained. 
  coordinates <- matrix()
  
  #If sample is specified but no position specified in argument, go to the present sample position identified by run_method()
  #This allows for a method to specify another sample to draw from besides the main one to be run, e.g. for a spike that needs to be added. 
  if(command$command=='SAMPLE' & (is.na(command$argument) | command$argument=='')){
    command$argument <- sampleposition
  }
  
  #Match coordinates to command and argument i, or just command if no argument is present in sequence
  if(command$command %in% locations$command){
    if(!(is.na(command$argument) | command$argument=='')){
      if(dim(locations[locations$argument == command$argument & locations$command == command$command,])[1]>0) {
        coordinates <- locations[locations$argument == command$argument & locations$command == command$command,]
      } else {
        write_log('BAK',paste('Error in translate_to_AS_gcode(): Command/argument combination is not validly specified in locations table:', command$command, ',', command$argument))
      }
    } else {
      possiblecoordinates <- locations[locations$command==command$command,]
      if(dim(possiblecoordinates)[1]>1) {
        write_log('BAK',paste('Error in translate_to_AS_gcode(): Multiple locations in lookup table match this command:', command$command, 'and an argument must be specified to pick one.'))
      } else {
        coordinates <- locations[locations$command==command$command,]
      }
    }
  }
  
 #If coordinates successfully retreived, construct a g code command to move there. Always retracts z axis, does XY movements, then extends Z, for needle safety.  
  if(dim(coordinates)[2]>1){
    serialout <- paste('G0 Z0', paste('G0 X', coordinates$x, ' Y', coordinates$y, sep=''), paste('G0 Z', coordinates$z, sep=''), sep='\n')
  } else {
 #If no coordinates retrieved, probably a special command, handled below. 
    serialout <- switch(command$command,
                        'GCODE' = command$argument,
                        'NOKEEPALIVE' = 'M113 S0',
                        'PAUSE' = paste0('G4 S', command$argument),
                        'ENABLEPLUNGER' = 'M302 P1',
                        'HOME' = 'G28', #standard home command
                        'ZEROPLUNGER' = 'G0 E1\nG92 E0',
                        'SYRINGEVOL' = if(command$argument <= method$MAXSYRINGEVOL){
                          if(command$argument == 0){ #If zero syringe volume is wanted, overstep the zero mark by 1mm (causing motor missed step), then set coords to zero. This is to minimize chance of unpurged volume in syringe. 
                            print('G0 E0')
                          }else{
                          paste0('G0 E-', command$argument*(method$MAXSYRINGESTROKE/method$MAXSYRINGEVOL)) #Z axis, negative is up. 
                          }
                          } else {
                            write_log('BAK', 'Desired syringe volume exceeds maximum volume!')
                            return(NULL)
                          }, #Fill syringe with amount of gas specified in argument (this translates it to mm movement)
                        'WAITCOMPLETE' = 'M400', #Wait till all movements are complete to proceed.
                        'GCSOLENOID' = ifelse(command$argument == 1, paste0('M42 P',method$GCSOLENOIDPIN,' S1'), paste0('M42 P',method$GCSOLENOIDPIN,' S0')), #Toggles solenoid purge valve
                        'PRINTREADY' = 'M118 AS READY\n',
                        'CARRIERSOLENOID'= ifelse(command$argument == 1, paste0('M42 P',method$CARRIERSOLENOIDPIN,' S1'), paste0('M42 P',method$CARRIERSOLENOIDPIN,' S0'))
                        )
  }
  
if(is.null(serialout)){
  write_log('BAK', paste('Error in translate_to_AS_gcode(): Command', command, 'not recognized.' ))
}
  return(serialout)
}