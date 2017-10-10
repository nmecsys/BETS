#' @title Get a complete time series from a BETS database
#' 
#' @description Extracts a complete time series from either the Central Bank of Brazil (BCB), the Brazilian Institute of Geography and Statistics (IBGE) or the Brazilian Institute of Economics (FGV/IBRE).
#' 
#' @param code A \code{character}. The unique code that references the time series. This code can be obtained by using the \code{\link{BETS.search}} function.
#' @param data.frame A \code{boolean}. True if you want the output to be a data frame. True to \code{ts} output.
#' @param from A \code{character} or a \code{Data} object. Starting date of the time series (format YYYY-MM-DD).
#' @param to A \code{character} or a \code{Data} object. Ending date of the time series (format YYYY-MM-DD).
#' @param frequency An \code{integer}. The frequency of the time series. It is not needed. It is going to be used only if the metadata for the series is corrupted. 
#' 
#' @return A \code{\link[stats]{ts}} (time series) object containing the desired series.
#' 
#' @note Due to the significant size of the databases, it could take a while to retrieve the values. However, it shouldn't take more than 90 seconds. 
#' 
#' @examples 
#'
#'  # Anual series: GDP at constant prices, in R$ (brazilian reais)
#'  #BETS.get(1208)
#'  
#'  # International reserves - Cash concept 
#'  #int.reserves <- BETS.get("3543")
#'  #plot(int.reserves)
#'  
#'  # Exchange rate - Free - United States dollar (purchase)
#'  #us.brl <- BETS.get(3691)
#'  
#'  # multiple requests - 1
#'  BETS.get(code = c(1,108,109))
#'  
#'  # multiple requests - 2
#'  BETS.get(code = c(10777,110,112),from = c("31-10-2001","",""),to = c("31-10-2016","",""))
#'  
#' @seealso \code{\link[stats]{ts}}, \code{\link[BETS]{BETS.search}} and \code{\link[seasonal]{seas}}
#' 
#' @keywords get
#' @import RMySQL
#' @import DBI
#' @export 

BETS.get = function(code,from = "",to="",data.frame = FALSE, frequency = NULL){
    n = length(code)
    aux = length(from)
    aux2 = length(to)
    frequency_fild = c("M","A","D","Q","w")
    if(n > 1){
        ts = list()
        nomes = c()
        for(i in 1:n){nomes[i] = paste0("ts_",code[i])}
        if(aux > 1 && aux2 > 1){
            
            for(i in 1:n){
                ts[[i]] = get_series(code[i],from=from[i],to[i],data.frame=FALSE,frequency=NULL)
            }
        }else if(aux == 1 && aux2 == 1){
            
            for(i in 1:n){
                ts[[i]] = get_series(code=108,from="",to="",data.frame=FALSE,frequency=NULL)
            }
        }else if(aux >1 && aux2>1){
            for(i in 1:n){
                ts[[i]] = get_series(code[i],from=from[i],to=to[i],data.frame=FALSE,frequency=NULL)
            }
        }    
        names(ts)<- nomes
        return(ts)
    }else{
        if(data.frame && (frequency %in% frequency_fild)) {
            return(get_series(code,from,to,data.frame=TRUE,frequency = frequency))
        }else{
            return(get_series(code,from,to,data.frame=FALSE,frequency = NULL))
        }
    }
}