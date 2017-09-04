#' @title Get a BETS series as a data.frame.
#' 
#' @description By default, \code{\link{BETS.get}} returns a \code{\link[stats]{ts}} object. However, there are many situations in which is more convenient to work with a data.frame. So, \code{get.data.frame} receives the code of a BETS series and returns a \code{\link[base]{data.frame}} containing the data of the corresponding series. Alternatively, a \code{ts} can be supplied, in which case the BETS databases will not be searched. 
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database.
#' @param ts An \code{ts} object. A time series to be formatted as a data.frame. 
#' 
#' @return A \code{data.frame}. The first column contains the dates. The second, its values.
#'
#' @import sqldf
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

get.data.frame = function(code, ts = NULL) {
  
  abbr = "values"
  
  if(is.null(ts)){
    
    result = BETS.search(code = code, view = F)
    
    if(nrow(result) != 0){
      
      abbr = abbreviate(result[1,2])
      freq = trimws(as.character(result[1,3]))
      
      if(freq == "A"){
        database = "ts_anuais"
      }
      else if(freq == "Q"){
        database = "ts_trimestrais"
      }
      else if(freq == "M"){
        database = "ts_mensais"
      }
      else if(freq == "W"){
        database = "ts_semanais"
      }
      else if(freq == "D"){
        database = "ts_diarias"
      }
      
      query = paste("select data, valor from ", database, " where serie like " ,"\'", code ,"\'",sep="")
      series = sqldf(query)
      
      if(nrow(series) == 0){
        return(msg(.MSG_NOT_AVAILABLE))  
      }
      
      if(is.factor(series[,1])){
        series[,1] = as.vector(series[,1])
      }
      
      series[,1] = as.date(series[,1])
    }
    else {
      return(msg(.MSG_NOT_FOUND))
    }
  }
  else {
    t = as.Date(ts)
    y = as.data.frame(ts)
    
    series = data.frame(t, y)
  }

  colnames(series) = c("dates",abbr)
  
  return(series)
  
}
