<<<<<<< HEAD:R/saveSas.R
#' @title Export a time series to SAS
#' 
#' @description Writes a time series to a .sas (SAS) file.
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database. 
#' @param data A \code{data.frame} or a \code{ts}. Contains the data to be written. If \code{data} is supplied, the BETS database will not be searched. 
#' @param file.name A \code{character}. The name of the output file. The default is 'series.sas'.
#' 
#' @return None
#' 
#' @examples 
#' 
#'  #Exchange rate - Free - United States dollar (purchase)
#'  #us.brl <- get(3691)
#'  #require(seasonal)
#'  #us.brl.seasonally_adjusted <- seas(us.brl)
#'  #saveSas(data = us.brl.seasonally_adjusted,file.name="us.brl.seasonally_adjusted")
#'  # Or
#'  #saveSas(code=3691,file.name="us.brl")
#' 
#' @importFrom foreign write.foreign
#' @export 


saveSas=function(code = NULL, data = NULL, file.name="series"){
  
  ret = save(code, data, file.name, "sas")
  
  if(class(ret) == "list"){
    write.foreign(ret$data, datafile = ret$file, codefile = tempfile(), package="SAS")
  }
  
}


=======
#' @title Export a time series to SAS
#' 
#' @description Writes a time series to a .sas (SAS) file.
#' 
#' @param code An \code{integer}. The unique identifier of the series within the BETS database. 
#' @param data A \code{data.frame} or a \code{ts}. Contains the data to be written. If \code{data} is supplied, the BETS database will not be searched. 
#' @param file.name A \code{character}. The name of the output file. The default is 'series.sas'.
#' 
#' @return None
#' 
#' @examples 
#' 
#'  #Exchange rate - Free - United States dollar (purchase)
#'  #us.brl <- BETS.get(3691)
#'  #require(seasonal)
#'  #us.brl.seasonally_adjusted <- seas(us.brl)
#'  #BETS.save.sas(data = us.brl.seasonally_adjusted,file.name="us.brl.seasonally_adjusted")
#'  # Or
#'  #BETS.save.sas(code=3691,file.name="us.brl")
#' 
#' @importFrom foreign write.foreign
#' @export 


BETS.save.sas=function(code = NULL, data = NULL, file.name="series"){
  
  ret = BETS.save(code, data, file.name, "sas")
  
  if(class(ret) == "list"){
    write.foreign(ret$data, datafile = ret$file, codefile = tempfile(), package="SAS")
  }
  
}


>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9:R/BETS.save.sas.R
