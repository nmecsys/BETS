<<<<<<< HEAD:R/t_test.R
#' @title Test the significance of the parameters of an ARIMA model
#' 
#' @description  Performs the t test on every parameter of an ARIMA model. This model can be an \link[forecast]{Arima} or an \link[stats]{arima}. 
#' 
#' @param model An \link[forecast]{Arima} or an \link[stats]{arima} object. The model for which the parameters must be tested.
#' @param nx An \code{integer}. The number of exogenous variables
#' @param alpha A \code{numeric} value between 0 and 1. The significance level.
#' 
#' 
#' @examples 
#' require(forecast)
#' data("AirPassengers")
#' fit.air<- Arima(AirPassengers,order = c(1,1,1), seasonal = c(1,1,1), method ="ML",lambda=0)
#' summary(fit.air)
#' 
#' # Significance test for the model SARIMA(1,1,1)(1,1,1)[12]
#' t_test(model = fit.air)
#'
#' 
#' @return A \code{data.frame} containing the standard erros, the t-statistic, the critical values and whether the null hypothesis should be rejected or not, for each model parameter. 
#' @importFrom stats qt
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}, Daiane Marcolino \email{daiane.mattos@fgv.br}
#' 
#' @export



t_test <- function(model, nx = 0, alpha = 0.05){

  coef <- model$coef
  se <- sqrt(diag(model$var.coef))
  t <- abs(coef/se)
  
  crit = qt(1 - alpha/2, length(model$x) - sum(model$arma[c(1,2,3,4,6,7)]) - nx)
  ok <- t > crit
  resul <- data.frame(Coeffs = coef, Std.Errors = se, t = t, Crit.Values = crit, Rej.H0 = ok )
  return(resul)
  
}
=======
#' @title Test the significance of the parameters of an ARIMA model
#' 
#' @description  Performs the t test on every parameter of an ARIMA model. This model can be an \link[forecast]{Arima} or an \link[stats]{arima}. 
#' 
#' @param model An \link[forecast]{Arima} or an \link[stats]{arima} object. The model for which the parameters must be tested.
#' @param nx An \code{integer}. The number of exogenous variables
#' @param alpha A \code{numeric} value between 0 and 1. The significance level.
#' 
#' 
#' @examples 
#' require(forecast)
#' data("AirPassengers")
#' fit.air<- Arima(AirPassengers,order = c(1,1,1), seasonal = c(1,1,1), method ="ML",lambda=0)
#' summary(fit.air)
#' 
#' # Significance test for the model SARIMA(1,1,1)(1,1,1)[12]
#' BETS.t_test(model = fit.air)
#'
#' 
#' @return A \code{data.frame} containing the standard erros, the t-statistic, the critical values and whether the null hypothesis should be rejected or not, for each model parameter. 
#' @importFrom stats qt
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}, Daiane Marcolino \email{daiane.mattos@fgv.br}
#' 
#' @export



BETS.t_test <- function(model, nx = 0, alpha = 0.05){

  coef <- model$coef
  se <- sqrt(diag(model$var.coef))
  t <- abs(coef/se)
  
  crit = qt(1 - alpha/2, length(model$x) - sum(model$arma[c(1,2,3,4,6,7)]) - nx)
  ok <- t > crit
  resul <- data.frame(Coeffs = coef, Std.Errors = se, t = t, Crit.Values = crit, Rej.H0 = ok )
  return(resul)
  
}
>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9:R/BETS.t_test.R
