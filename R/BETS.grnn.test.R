#' @title Test a set of General Regression Neural Networks
#' 
#' @description Given new values of the independent variables, tests a list of trained GRNNs and picks the best net, based on an accuracy measure between the forecasted and the actual values. 
#' 
#' @param results The object returned by \link[BETS]{BETS.grnn.train}. 
#' @param test.set A \code{ts list}. The first element must be the actual values of the dependent variable. The others, the new values of the regressors.
#' 
#' @return A \code{list} object representing the best network (according to forecasting MAPE). Its fields are:
#' 
#' \itemize{
#' \item{\code{mape}: The forecasting MAPE }
#' \item{\code{model}: The network object (returned by \link[grnn]{grnn})}
#' \item{\code{sigma}: The sigma parameter}
#' \item{\code{id}: The id number of the network, as given by \link[BETS]{BETS.grnn.train} }
#' \item{\code{mean}: The predicted values }
#' \item{\code{x}: The original series } 
#' \item{\code{fitted}: The fitted values }
#' \item{\code{actual}: The actual values (to be compared with the predicted values)}
#' \item{\code{residuals}: Difference between the fitted values and the series original values }
#' \item{\code{regressors}: The regressors used to train the network }
#' }
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' @importFrom utils head combn
#' @export
#' @import grnn forecast

BETS.grnn.test = function(results, test.set){
  
  select = TRUE 
  
  if(length(test.set) < 2 || !check.series(test.set, "Series list: test.")){
    return(NULL)
  }
  
  test.n_elem = length(test.set[[1]])
  test.n_series = length(test.set) 
  actual = test.set[[1]]
  
  test_mt = matrix(nrow = test.n_elem, ncol = test.n_series)
  
  for(i in 1:test.n_series){
    test_mt[,i] = test.set[[i]]
  }
  
  res = vector(mode = "list")
  res$mape = 1.797693e+308
  
  if(select){
    
    for(i in 1:length(results)){
      
      regs = results[[i]]$regressors
      sub_test = as.matrix(test_mt[,regs])
      
      preds = vector(mode = "numeric")
      
      for(r in 1:nrow(sub_test)){
        preds[r] = guess(results[[i]]$net, t(as.matrix(sub_test[r,])))
      }
      
      if(!any(is.nan(preds))){
        acc = accuracy(preds,actual)[5]
      }
      else{
        acc = res$mape
      }
      
      if(acc < res$mape){
        res$model = results[[i]]$net
        res$mape = acc 
        res$id = results[[i]]$id
        res$sigma = results[[i]]$sigma
        res$mean = ts(preds,start = start(actual), end=end(actual), frequency = frequency(actual))
        res$x = results[[i]]$series
        res$fitted = results[[i]]$fitted
        res$actual = actual
        res$residuals = results[[i]]$residuals
        res$regressors = results[[i]]$regressors
      }
    }
    
  }
  
  return(res)
}