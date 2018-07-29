#' @title Normalize a time series
#' 
#' @description  Normalizes a time series, either by stardization or by mapping to values between 0 and 1. 
#' 
#' @param series A \code{ts} object or a \code{ts list}. The series to be normalized. 
#' @param mode A \code{character}. The normalization method. Set this parameter to 'maxmin' to map series values to values between 0 and 1. Alternatively, set this parameter to 'scale' to standardize (substract the mean and divide by the standard deviation).
#' 
#' @return A \code{ts} object or a \code{ts list}. The normalized series. 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @importFrom  stats as.ts end fitted frequency na.omit plot.ts qnorm qt sd start time ts uniroot window 
#' 
#' @export


normalize = function(series, mode="scale"){
  
  if(mode == "maxmin"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x-min(x))/(max(x)-min(x))}))
    }
    else{
      return((series - min(series))/(max(series) - min(series)))
    }
    
  }
  
  else if(mode == "scale"){
    
    if(is.list(series)){
      return(lapply(series, function(x){(x - mean(x))/sd(x)}))
    }
    else {
      return((series - mean(series))/sd(series))
    }
  }
  
  return("ERROR")
}
