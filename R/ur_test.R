#' @title  Perform unit root tests
#' 
#' @description This function uses the package 'urca' to perform unit root tests on a pre-defined time series. Unlike urca functions, it returns a meaningful table summarizing the results. 
#' 
#' @param ... Arguments passed on to urca functions
#' @param mode A \code{character}. The type of the test. Set it to 'ADF' for Augmented Dickey-Fuller, 'KPSS' for KPSS or 'PP' for Phillips-Perron.
#' @param level A \code{character}. The confidence level. Can be either '1pct' (not for KPSS), '2.5pct', '5pct' or '10pct'
#' 
#' @return A \code{list} object. The first element is a \code{data.frame} with the test statistics, the critical values and the test results. The second, the model residuals. 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @import urca

ur_test = function(..., mode = "ADF", level = "5pct"){
  
  if(mode == "ADF"){
    df <- ur.df(...)
  } else if(mode == "KPSS"){
    df <- ur.kpss(...)  
  } else if(mode == "PP"){
    df <- ur.pp(...)
  } else {
    return(invisible(msg(paste("mode = ",mode, " - ",.MSG_PARAMETER_NOT_VALID))))
  }
 
  cval = as.matrix(df@cval[,level])
  stat = t(df@teststat)
  res = vector(mode = "logical")
  
  for(i in 1:length(stat)){
    
    if(mode == "KPSS"){
        
        if(stat[i] > cval[i]){
            res = c(res, "yes")
        }
        else {
            res = c(res, "no")
        }
        
    } else {
        
        # If the test statistic is less (this test is non symmetrical so we do 
        # not consider an absolute value) than the critical value, 
        # then the null hypothesis of tau2 = 0 is rejected and no unit root is present.
        
        if(stat[i] > cval[i]){
            res = c(res, "no")
        }
        else {
            res = c(res, "yes")
        }
    }
    
  }
  
  res = as.matrix(res)
  results = data.frame(statistic = stat,crit.val = cval,  rej.H0 = res)
  return(list(results = results, residuals = df@res))
}