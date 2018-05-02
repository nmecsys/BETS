#' @title Create dynamic reports with a full analysis of a set of time series
#' 
#' @description Generate automatic reports with a complete analysis of a set of time series. For now, only a SARIMA analysis (Box & Jenkins approach) is possible. In a near future, a GRNN (General Regression Neural Network) analysis will be released. Soon after, Holt-Winters, GRNN, Multilayer Perceptron, Fuzzy Logic and Box-Cox analysis will become available.
#' 
#' @param mode A \code{character}.The type of the analysis. So far, 'SARIMA', 'GRNN' and 'HOLT-WINTERS' are available.
#' @param ts A \code{integer}, a \code{ts} object or a \code{list} of \code{integer}s and \code{ts} objects. Either the ID of the series in the BETS database or a time series object (any series, not just BETS's). If a \code{list} is provided, a report is generated for each series in this list, which can be mixed with IDs and time series objects.
#' @param parameters A \code{list}. The parameters of the report. See the 'details' section for more information.
#' @param report.file A \code{character}. A path and a name for the report file (an .html file). If there is more than one series, this name will be used as a prefix. If this parameter is not provided, the report will be saved inside the 'reports' folder, under the BETS installation directory. 
#' @param series.saveas A \code{character}. The format of the file on which the series and the predictions should be written. Possible values are 'none' (default), 'sas', 'dta', 'spss', 'csv', 'csv2' . Is is saved under the same directory as the report file.
#' 
#' @details 
#' 
#' \bold{SARIMA Report Parameters}
#' 
#' \itemize{
#' \item{\code{cf.lags}: An \code{integer}. Maximum number of lags to show on the ACFs e PACFs}
#' \item{\code{n.ahead}: An \code{integer}. Prevision horizon (number of steps ahead)}
#' \item{\code{inf.crit}: A \code{character}. Information criterion to be used in model selection.}
#' \item{\code{dummy}: A \code{ts} object. A dummy regressor. Must also cover the forecasting period.}
#' \item{\code{ur.test}: A \code{list}. Parameters of \code{\link[BETS]{ur_test}} }
#' \item{\code{arch.test}: A \code{list}. Parameters of \code{\link[BETS]{arch_test}} }
#' \item{\code{box.test}: A \code{list}. Parameters of \code{\link[stats]{Box.test}} }
#' }
#' 
#' \bold{GRNN Report Parameters}
#' 
#' \itemize{
#' \item{\code{auto.reg}: A \code{boolean}. Is the dependant variable auto-regressive?}
#' \item{\code{present.regs}: A \code{boolean} Include non-lagged series among regressors? }
#' \item{\code{lag.max}: A \code{integer} Regressors' maximum lag}
#' \item{\code{regs}: A \code{list}. Regressors codes or time series}
#' \item{\code{start.train}: Training set starting period }
#' \item{\code{end.train}: Training set ending period}
#' \item{\code{start.test}: Testing set starting period }
#' \item{\code{end.test}: Testing set ending period }
#' \item{\code{sigma.interval}: A \code{numeric} vector. Sigma inteval}
#' \item{\code{sigma.step}: A \code{numeric} value. Sigma step}
#' \item{\code{var.names}: A \code{character} vector. Variable names}
#' }
#' 
#' \bold{HOLT-WINTERS Report Parameters}
#' 
#' \itemize{
#' \item{\code{alpha}: Smooth factor of the level component. If numeric, it must be within the half-open unit interval (0, 1]. A small value means that older values in x are weighted more heavily. Values near 1.0 mean that the latest value has more weight. NULL means that the HoltWinters function should find the optimal value of alpha. It must not be FALSE or 0.}
#' \item{\code{beta}: Smooth factor of the trend component. If numeric, it must be within the unit interval [0, 1]. A small value means that older values in x are weighted more heavily. Values near 1.0 mean that the latest value has more weight. NULL means that the HoltWinters function should find the optimal value of beta. The trend component is omitted if beta is FALSE or 0.}
#' \item{\code{gamma}: Smooth factors of the seasonal component. If numeric, it must be within the unit interval [0, 1]. A small value means that older values in x are weighted more heavily. Values near 1.0 mean that the latest value has more weight. NULL means that the HoltWinters function should find the optimal value of gamma. The seasonal component will be omitted if gamma is FALSE or 0. This must be specified as FALSE if frequency(x) is not an integer greater than 1.}
#' \item{\code{additive}: A single character string specifying how the seasonal component interacts with the other components. "additive", the default, means that x is modeled as level + trend + seasonal and "multiplicative" means the model is (level + trend) * seasonal. Abbreviations of "additive" and "multiplicative" are accepted.}
#' \item{\code{l.start}: The starting value of the level component.}
#' \item{\code{b.start}: The starting value of the trend component}
#' \item{\code{s.start}: The starting values of seasonal component, a vector of length frequency(x)}
#' \item{\code{n.ahead}: Prevision horizon (number of steps ahead)}
#' }
#' 
#' For more information about these parameters, see also \code{\link{HoltWinters}}. Most parameters are the same and we just reproduced their documentation here.
#' 
#' @return One or more .html files (the reports) and, optionally, data files (series plus predictions).
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @examples 
#' ##-- SARIMA
#' 
#' # parameters = list(lag.max = 48, n.ahead = 12 ) 
#' # report(ts = 21864, parameters = parameters)
#' 
#' # report(ts = 4447, series.saveas = "csv")
#' 
#' # series = list(BETSget(4447), BETSget(21864))
#' # parameters = list(lag.max = 20, n.ahead = 15 ) 
#' # report(ts = series, parameters = parameters)
#' 
#' # series = list(4447, 21864)
#' # report(ts = series, parameters = parameters)
#' 
#' # parameters = list( 
#' # cf.lags = 25,
#' # n.ahead = 15,
#' # dummy = dum,
#' # arch.test = list(lags = 12, alpha = 0.01),
#' # box.test = list(type = "Box-Pierce")
#' # )
# 
#' # report(ts = window(BETSget(21864), start= c(2002,1) , end = c(2015,10)), 
#' #parameters = parameters)
#' 
#' # dum <- dummy(start= c(2002,1) , end = c(2017,1) , 
#' #from = c(2008,9) , to = c(2008,11))
#' 
#' # parameters = list( 
#' #    cf.lags = 25,
#' #    n.ahead = 15,
#' #    dummy = dum
#' # )
#' 
#' # report(ts = window(BETSget(21864), start= c(2002,1) , end = c(2015,10)), 
#' #parameters = parameters)
#'
#' 
#' ##-- GRNN
#' 
#' # params = list(regs = 4382)
#' # report(mode = "GRNN", ts = 13522, parameters = params)
#' 
#' ##-- HOLT-WINTERS
#' 
#' # params = list(alpha = 0.5, gamma = TRUE)
#' # report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)
#' 
#' # params = list(gamma = T, beta = TRUE)
#' # report(mode = "HOLT-WINTERS", ts = 21864, series.saveas = "csv", parameters = params)
#' 
#' @export
#' @import rmarkdown


report <- function(mode = "SARIMA", ts = 21864, parameters = NULL, report.file= NA, series.saveas = "none"){
  
  if(class(ts) == "list" || class(ts) == "numeric" || class(ts) == "integer"){
    vec = ts
  } else if(class(ts) == "ts"){
    vec = list(ts)
  } else {
    return(msg("ts - ", .MSG_PARAMETER_NOT_VALID))
  }
  
  if(is.na(report.file)){
    
    dir = paste0(system.file(package="BETS"),"/reports")
    dir.create(dir)
    report.file = paste0(dir,"/analysis")
  }
  
  i = 1

  for(ts in vec){ 
    
    
    name = paste0("analysis_",mode,".Rmd")
    file = system.file(package="BETS", name)
    
    if(class(ts) == 'ts'){
      id = paste0("custom_", i)
      i = i + 1
    }
    else{
      id = ts
    }
    
    rep.file = paste0(report.file, "_", mode, "_", id)
    
    if(series.saveas != "none"){
      series.file = paste0(rep.file,".",series.saveas) 
    }
    else{
      series.file = NA
    }
    
    rep.file = paste0(rep.file,".html")
    
    if(!(ts == 21864 && is.null(parameters))){
      parameters$ts = ts
    }

    parameters$series.file = series.file
    rmarkdown::render(file, params = parameters)
    
    file = gsub(".Rmd", ".html", file)
  
    file.copy(file, rep.file, overwrite = T)
    file.remove(file)
    system2("open", rep.file)
  }
}