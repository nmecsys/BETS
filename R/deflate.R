#' @title Deflate a time series
#' 
#' @description  Deflate a time series using a deflator series. The deflator can be an index, a percentage or a point percentage series.
#' 
#' @param ts A \code{ts} object. The time series to be deflated.
#' @param deflator A \code{ts} object. The deflator series.
#' @param type A \code{character}. Can be either \code{'index'}, \code{'point.perc'} (for point percentage) or \code{'perc'} (for percentage).
#' 
#' @return The deflated series.
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export

deflate = function(ts, deflator, type = "index"){
  
  freq_ts = frequency(ts)
  freq_def = frequency(deflator)
  
  
  if(freq_ts != freq_def){
    return("ERROR")
  }
  
  if(length(ts) == length(deflator)){
    
    if(type == "index"){
      deflator = 100/deflator
    } else if(type == "point.perc"){
      deflator= 1/deflator
    } else {
      deflator = 1/(deflator/100 + 1)
    }
    
    s = as.numeric(ts)
    def = as.numeric(deflator)
    
    
  } else{
    
    start_ts = start(ts)
    start_def = start(deflator)
    end_ts = end(ts)
    end_def = end(deflator)
    
    if(start_ts[1] > start_def[1]){
      deflator = window(deflator, start = start_ts, frequency = freq_ts)
    } else if(start_ts[1] < start_def[1]){
      ts = window(ts, start = start_def, frequency = freq_ts)
    } else {
      
      if(start_ts[2] > start_def[2]){
        deflator = window(deflator, start = start_ts, frequency = freq_ts)
      } else if(start_ts[2] < start_def[2]){
        ts = window(ts, start = start_def, frequency = freq_ts)
      }
    }
    
    if(end_ts[1] > end_def[1]){
      ts = window(ts, end = end_def, frequency = freq_ts)
    } else if(end_ts[1] < end_def[1]){
      deflator = window(deflator, end = end_ts, frequency = freq_ts)
    } else {
      
      if(end_ts[2] > end_def[2]){
        ts = window(ts, end = end_def, frequency = freq_ts)
      } else if(end_ts[2] < end_def[2]){
        deflator = window(deflator, end = end_ts, frequency = freq_ts)
      }
    }
    
    if(type == "index"){
      deflator = 100/deflator
    } else if(type == "point.perc"){
      deflator= 1/deflator
    } else {
      deflator = 1/(deflator/100 + 1)
    }
    
    s = as.numeric(ts)
    def = as.numeric(deflator)
  
  }
  
  return(ts(s*def, start = start(ts), frequency = freq_ts))
}