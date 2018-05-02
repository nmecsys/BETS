#' @title Get a complete time series from a BETS database
#' 
#' @description Extracts a complete time series from either the Central Bank of Brazil (BCB), the Brazilian Institute of Geography and Statistics (IBGE) or the Brazilian Institute of Economics (FGV/IBRE).
#' 
#' @param code A \code{character}. The unique code that references the time series. This code can be obtained by using the \code{\link{BETSsearch}} function.
#' @param data.frame A \code{boolean}. True if you want the output to be a data frame. True to \code{ts} output.
#' @param from A \code{character} or a \code{Data} object. Starting date of the time series (format YYYY-MM-DD).
#' @param to A \code{character} or a \code{Data} object. Ending date of the time series (format YYYY-MM-DD).
#' @param frequency An \code{integer}. The frequency of the time series. It is not needed. It is going to be used only if the metadata for the series is corrupted. 
#' 
#' 
#' @keywords get
#' @import RMySQL
#' @import DBI


get.series = function(code, from = "", to = "", data.frame = FALSE, frequency = NULL){
  
  # date_to = strsplit(to,split="-")
  # to = paste0(date_to[[1]][3],"/",date_to[[1]][2],"/",date_to[[1]][1]) 
  # 
  # date_from = strsplit(from,split="-")
  # from = paste0(date_from[[1]][3],"/",date_from[[1]][2],"/",date_from[[1]][1])
  
  
  if(!grepl("ST_",code)){
     
    if(from != ""){
        from = format(as.Date(from), "%d/%m/%Y")  
    } 
      
    if(to != ""){
        to = format(as.Date(to), "%d/%m/%Y")
    }
    
    
    code = as.numeric(code)
    aux = get.series.bacen(code, from = from, to = to)[[1]]
    
    x <- get.series.bacen(code, from = from, to = to)[[1]]
    y <- get.series.bacen(code, from = from, to = to)[[1]]
    
    if(nrow(aux)<nrow(x)){
      aux <- x
    }else if(nrow(aux)<nrow(y)){
      aux <- y
    }
    
    if(nrow(aux) == 0){
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series is empty in the BACEN databases"))))
    }
    
    sch = suppressMessages(BETSsearch(code = code, view = F))
    freq = NA
    
    if(class(sch) == "data.frame"){
      freq = trimws(sch[1,4])
    }
    
    no.meta = F
    
    if(is.na(freq)){
      msg(paste("There is no corresponding entry in the metadata table.\n\n", .WARN_SOFT), warn = TRUE)
      no.meta = T
      freq = ""
    }
    
    if(freq == "A"){
      freq = 1
    }
    else if(freq == "Q" || freq == "T"){
      freq = 4
    }
    else if(freq == "M"){
      freq = 12
    }
    else if(freq == "W" || freq == "S"){
      freq = 52
    }
    else if(freq == "D"){
      freq = 365
    }
    else {
      
      if(!no.meta){
        msg(paste("Malformed metadata. The value", freq, "is not valid for 'periodicity'\n\n", .WARN_SOFT), warn = TRUE)
      }
       
      if(is.null(frequency)){
        data.frame = T
      }
      else {
        freq = frequency 
      }
    }
    
  } else {
    
    freq = 365 
    conn = connection()
    
    aux = DBI::dbGetQuery(conn,paste0("select date, value from bets.IPC where code = '",code,"' order by date asc"))
    
    invisible(dbDisconnect(conn))
    
    if(nrow(aux) == 0){
      return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series is empty in the FGV database"))))
    }
    
    #- Falta filtrar por datas!
  }
  
  aux1 = as.numeric(aux[,2])
  
  try = FALSE
  
 if(grepl("-",aux[1,1])){
  try = tryCatch({
    aux2 = as.Date(aux[,1], format = "%Y-%m-%d")},
    error = function(err) {
      return(TRUE)
    }
  )}else{
    try = tryCatch({
      aux2 = as.Date(aux[,1], format = "%d/%m/%Y")},
      error = function(err) {
        return(TRUE)
      })
  }
  
  
  suppressWarnings(if(try==TRUE){
    
    return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Date formatting is inadequate."))))
  })
  
  
  if(freq != 365 &&!data.frame){
      
      #year = as.numeric(format(k,"%Y"))
      #month = as.numeric(format(k,"%m"))
      #day = as.numeric(format(k,"%d"))
      
    start = get.period(aux2[1],freq)
    #start = get.period(c(year,month,day),freq)
    #print(start)
    ts <- ts(aux1, start = start, freq = freq)
  }else {
      
    ts = data.frame(date = aux2, value = aux1)
  }
  
  
  return(ts)
}
