#' @title  Check series
#' 
#' @description  Check series in BETS dataset
#' 
#' @param ts A \code{ts object}
#' @param message A \code{character}
#' 
#' @importFrom  stats start end
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


check.series = function(ts,message = NULL){
  
  if(is.list(ts)){
    s = sum(sapply(ts, function(x){anyNA(x)}))
    
    if(s != 0){
      msg(paste("There is at least one series with NAs.",message))
      return(FALSE)
    }
    
    l = length(ts[[1]])
    st = start(ts[[1]])
    e = end(ts[[1]])
    
    for(i in 2:length(ts)){
      
      if(l != length(ts[[i]])){
        msg(paste("Not all series have the same length.",message))
        return(FALSE)
      }
      
      exp = all.equal(st,start(ts[[i]]))
      
      if(!isTRUE(exp)){
        msg(paste("Not all series have the same starting period.",message))
        return(FALSE)
      }
      
      exp = all.equal(e,end(ts[[i]]))
      
      if(!isTRUE(exp)){
        msg(paste("Not all series have the same ending period.",message))
        return(FALSE)
      }
    }

  }
  else if(class(ts) == "ts") {
    
    if(anyNA(ts)){
      msg(paste("This series contains NAs.",message))
      return(FALSE)
    }
  }
  else{
    msg(paste("Argument is not a time series or a list of time series.",message))
    return(FALSE)
  }
  
  return(TRUE)
}
