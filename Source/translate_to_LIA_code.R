translate_to_LIA_code <- function (command){
  if (command$command == 'ISREADY'){
    return('R')
  } else if (command$command == 'SWITCH_BAG'){
    return('V4S')
  } else if (command$command == 'BAG_ONE'){
    return('V41')
  } else if (command$command == 'BAG_TWO'){
    return('V40')
  } else if (command$command == 'VENT_SAMPLE'){
    return('V31')
  } else if (command$command == 'NO_VENT_SAMPLE'){
    return('V30')
  } else if (command$command == 'PURGE_CYCLE'){
    return('V21')
  } else if (command$command == 'COLLECTION_CYCLE'){
    return('V20')
  } else if (command$command == 'VACUUM_ON'){
    return('V11')
  } else if (command$command == 'PURGE_GAS_ON'){
    return('V10')
  }else {
    return(NULL)
  }
}
