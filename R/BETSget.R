#' @title Get a complete time series from a BETS database
#' 
#' @description Extracts a complete time series from either the Central Bank of Brazil (BCB), the Brazilian Institute of Geography and Statistics (IBGE) or the Brazilian Institute of Economics (FGV/IBRE).
#' 
#' @param code A \code{character} or an \code{integer}. The unique code that references the time series. This code can be obtained by using the \code{\link{search}} function. More than one code can be provided at once, through a vector. In this case, be careful with the dates, i.e, parameters \code{from} and \code{to}. They must either be the same length as \code{code}, containing the date limits in order, or an isolated date, but nothing in between. See the examples section. 
#' @param data.frame A \code{boolean}. True if you want the output to be a data frame. True to \code{ts} output.
#' @param from A \code{character} or a \code{Data} object. Starting date of the time series (format YYYY-MM-DD). Can be a vector of dates/characters if the length of the parameter \code{code} is greater than 1.
#' @param to A \code{character} or a \code{Data} object. Ending date of the time series (format YYYY-MM-DD). Can be a vector of dates/characters if the length of the parameter \code{code} is greater than 1.
#' @param frequency An \code{integer}. The frequency of the time series. It is not needed. It is going to be used only if the metadata for the series is corrupted. 
#' 
#' @return A \code{\link[stats]{ts}} (time series) object containing the desired series.
#' 
#' @note Due to the significant size of the databases, it could take a while to retrieve the values. However, it shouldn't take more than 90 seconds. 
#' 
#' @examples 
#'
#'  # Anual series: GDP at constant prices, in R$ (brazilian reais)
#'  #BETSget(1208)
#'  
#'  # International reserves - Cash concept 
#'  #int.reserves <- get("3543")
#'  #plot(int.reserves)
#'  
#'  # Exchange rate - Free - United States dollar (purchase)
#'  #us.brl <- get(3691)
#'  
#'  # Multiple requests
#'  # BETSget(code = c(10777,4447),from = "2001-01-01", to = "2016-10-31")
#'  # BETSget(code = c(10777,4447),from = c("2001-10-31",""),to = c("2016-10-31",""))
#'  
#'  # f <- c("2001-10-31","1998-09-01")
#'  # t <- c("2014-10-31","2015-01-01")
#'  # BETSget(code = c(10777,4447), from = f, to = t)
#'  
#'  # BETSget(code = c(10777,4447),from = "2001-10-31", to = c("2014-10-31","2015-01-01"))
#'  # BETSget(code = c(10777,4447),from = c("2002-10-31","1997-01-01"), to = "2015-01-01")
#'  
#'  
#' @seealso \code{\link[stats]{ts}}, \code{\link[BETS]{BETSsearch}} and \code{\link[seasonal]{seas}}
#' 
#' @keywords get
#' @import RMySQL
#' @import DBI
#' @export 

BETSget = function(code,from = "", to = "",data.frame = FALSE, frequency = NULL){
    
    n = length(code)
    f = length(from)
    t = length(to)
    
    if(n > 1){
        
        ts = list()
        nms = c()
        
        if(f == 1 && t == 1){
            for(i in 1:n){
                ts[[i]] = get.series(code[i], from = from, to = to, data.frame = data.frame, frequency = frequency)
            }
        } else if(f == n && t == 1){
            for(i in 1:n){
                ts[[i]] = get.series(code[i], from = from[i], to = to, data.frame = data.frame, frequency = frequency)
            }
        } else if(f == 1 && t == n){
            for(i in 1:n){
                ts[[i]] = get.series(code[i], from = from, to = to[i], data.frame = data.frame, frequency = frequency)
            }
        } else if(f == n && t == n){
            for(i in 1:n){
                ts[[i]] = get.series(code[i], from = from[i], to = to[i], data.frame = data.frame, frequency = frequency)
            } 
        }
        
        for(i in 1:n){
            nms[i] = paste0("ts_",code[i])
        }
        
        names(ts)<- nms
        return(ts)
        
    } else{
        return(get.series(code, from, to, data.frame = data.frame, frequency = frequency))
    }
}