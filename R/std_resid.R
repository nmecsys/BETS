#' @title Plot standardized residuals
#' 
#' @description Uses a model object to create a plot of standardized residuals. This model can be an \link[forecast]{Arima} or an \link[stats]{arima}. In a near future, this function will also accept objects returned by \link[BETS]{grnn.train}.
#' 
#' @param model An \link[forecast]{Arima} or an \link[stats]{arima} object. The model.
#' @param alpha A \code{numeric} between 0 and 1. The significance level.
#' 
#' @return Besides showing the plot, this function returns a \code{numeric vector} containing the standardized residuals.
#' 
#' @importFrom stats sd qnorm
#' @importFrom  graphics abline
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export


std_resid = function(model, alpha = 0.05){
  resid <- resid(model)
  rp <- (resid - mean(resid))/sd(resid)
  plot(rp, col = "royalblue", ylim = c(-0.5 + min(rp),0.5 + max(rp)), ylab = "Standard Residuals")
  abline(h = c(-qnorm(1 - alpha/2),qnorm(1- alpha/2)), col = "gray", lty = 2)
  return(rp)
}
