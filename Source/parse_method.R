parse_method <- function(methodpath) {
methodlist <- tryCatch({
  
  #Wrap everything in warning suppression, due to some spurious NA test warnings.
  #This is dangerous, maybe should change.
    #Read method file in raw. 
    fileconn <-file(methodpath, open='r')
    rawmethod <- readLines(fileconn)
    close(fileconn)
    if(length(rawmethod)==0){
      write_log('BAK', paste( "Method:", methodpath, 'not found. Check the path.'))
      return(NULL)
    }
    #Remove comments from each line.
    a <- sapply(rawmethod, function(x){
      hashtaglocation <- regexpr('#',x)[1]
      if (hashtaglocation == -1){
        return(x)
      } else if (hashtaglocation == 1){
        return('')
      } else {
        return(substr(x,1,hashtaglocation-1))
      }
    }, 
    USE.NAMES = FALSE)
    
    #Remove white space
    b <- gsub(' ', '', a)
    b <- b[!(b=='')]
    
    #Parse global method variables and sort into list. 
    methodvariables <- b[(which(b=='***SETUP***')+1):(which(b=='***SEQUENCE***')-1)]
    if(length(methodvariables)>0){
      methodlist <- lapply(methodvariables, function(x){unlist(strsplit(x,'='))[2]})
      names(methodlist) <- sapply(methodvariables, function(x){unlist(strsplit(x,'='))[1]})
      #Convert number variables to numeric instead of strings. 
      methodlist <- lapply(methodlist, function(x){
        if(!is.na(as.numeric(x))){
          as.numeric(x)
        } else x
      })
    } else {
      methodlist <- list()
    }
    
    #Parse sequence into data.frame
    methodlist$SEQUENCE <- read.csv(text=paste(b[(which(b=='***SEQUENCE***')+1):length(b)], collapse='\n'))
  
  
  return(methodlist)
  },

  error=function(cond){write_log('BAK', paste('Error in parse_method() when parsing method:', methodpath,  '\n Specifically:', cond, '\n Check the syntax of your method file.'))
    return(NULL)}
)

  return(methodlist)
}

