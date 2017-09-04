#' @title  Plot the ACF or the PACF of a time series
#' 
#' @description  Plot correlograms using plot.ly and several other options that differ theses plots from \link[forecast]{forecast}s ACF and PACF.
#' 
#' @param ts An object of type \code{ts} or \code{xts}. The time series for which the plot must be constructed.  
#' @param lag.max A \code{numeric} value. The number of lags to be shown in the plot.
#' @param type A \code{character}. Can be either 'correlation' (for the ACF) or 'partial' (for the PACF).
#' @param style A \code{character}. Set this parameter to 'normal' if you want it made with ggplot2 or to 'plotly' if you want to be a \link[plotly]{plotly} object.
#' @param ci A \code{numeric} value. The confidence interval to be shown in the plot.
#' @param mode A \code{character}. Set this parameter to 'bartlett' if you want the variance to be calculated according to \href{https://en.wikipedia.org/wiki/Correlogram#Statistical_inference_with_correlograms}{Bartlett's formula}. Otherwise, it is going to be simply equal to \code{1/sqrt(N)}. 
#' @param knit A \code{boolean}. If you're using this function to exhibit correlograms on a R dynamic report, set this parameter to true.
#' 
#' @return A plot and a \code{vector} containing the correlations.
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @export
#' @importFrom plotly plotly_build
#' @import forecast 
#' @importFrom ggplot2 ggplot geom_segment scale_x_continuous geom_point geom_step labs aes

BETS.corrgram = function(ts, lag.max = 12, type = "correlation", mode = "simple", ci = 0.95, style = "plotly", knit = F){
  
  ## Validation
  
  if(!is.numeric(ci) || ci <= 0 || ci >= 1){
    stop("Parameter 'ci' (confidence interval) must be a real number between 0+ and 1-")
  }
  
  if(type != "correlation" && type != "partial"){
    stop("Unknown value for parameter 'type'")
  }
  
  if(mode != "simple" && mode != "bartlett"){
    stop("Unknown value for parameter 'mode'")
  }
  
  if(!is.integer(lag.max)){
    if(is.numeric(lag.max)){
      lag.max = round(lag.max)
    }
    else{
      stop("Parameter 'lag.max' must be an integer")
    }
  }
  
  alpha = (1 - ci)/2
  
  z = -qnorm(alpha, 0, 1)
  
  if(type == "correlation"){
    out <- forecast::Acf(ts, plot=F, lag.max = lag.max)
    yaxis = "Correlation"
    corrs <- out$acf[-1, , ]
    lags = out$lag[-1, , ]
  }
  else {
    out <- forecast::Pacf(ts, plot=F, lag.max = lag.max)
    yaxis = "Partial Correlation"
    corrs <- out$acf[ , , ]
    lags = out$lag[ , , ] + 1
  }
  
  step = frequency(ts)
  lim = lag.max - lag.max%%step
  ticks = seq(0,lim,step)
  ticks[1] = 1
  
  var <- vector(mode = "numeric")
  
  N = length(ts)
  var[1] = 1/sqrt(N)
  sum = 0  
  
  if(mode == "bartlett"){
    
    for(i in 2:length(lags)){
      for(j in 1:(i-1)){
        sum = sum + (var[j])^2
      }
      var[i] = sqrt((1 + 2*sum)/N)
    }
  } 
  else {
    
    for(i in 2:length(lags)){
      var[i] = var[1]
    }
  }
  
  var = z*var
  data <- as.data.frame(cbind(lags,corrs,var))
  
  gp <- ggplot(data, aes(x = lags, y= corrs)) +
    geom_segment(aes(x=lags, xend=lags, y=0, yend=corrs), data=data, size = 0.5) +
    scale_x_continuous(breaks = ticks) + 
    geom_point(size = 0.5) +
    geom_step(data=data, mapping=aes(x=lags, y=var), color="red", linetype="dashed", size=0.3) +
    geom_step(data=data, mapping=aes(x=lags, y=-var), color="red", linetype="dashed", size=0.3) + 
    labs(list(x="Lag", y= yaxis))
  
  if(style == "plotly"){
    p <- plotly_build(gp)
    
    p$x$data[[1]]$text <- paste("Lag:", lags)
    p$x$data[[2]]$text <- paste0(yaxis, ": ", round(corrs,3), " <br> Lag: ", lags)
    p$x$data[[3]]$text <- paste("CI upper bound:", round(var,3))
    p$x$data[[4]]$text <- paste("CI lower bound:", -round(var,3))
   
    if(knit){
      return(p)
    }
    else{
      print(p)
    }
  }
  else {
    p <- gp 
    
    if(knit){
      return(p)
    }
    else{
      par(cex.axis = 0.75, cex.lab = 0.8, mar = c(5.1, 4.1, 0.5, 2.1))
      plot(p)
    }
  }
  
  return(invisible(corrs))
}






