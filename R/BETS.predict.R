#' @title Get the predicted values of a model and visualize it
#' 
#' @description This function is built upon \link[forecast]{forecast}. Besides the model predictions, it returns an accuracy measure table (calculated by the \link[forecast]{accuracy} function) and a graph showing the original series, the predicted values and the actual values. 
#' 
#' @param ... arguments passed on to \link[forecast]{forecast}. If the model is a neural network, these arguments will be passed on to \link[BETS]{BETS.grnn.test}.
#' @param actual A \code{numeric vector}. The actual values (to be compared with predicted values).
#' @param main A \code{character}. The name of the prediction plot.
#' @param ylab A \code{character}. The Y axis label.
#' @param xlim A \code{numeric vector}. The limits of the X axis.
#' @param style A \code{character}. Can be either 'dygraphs' (the \link[dygraphs]{dygraph} function will be use to make the plot, which is going to be HTML based) or 'normal' (standard R functions will be used to make the plot)
#' @param unnorm A \code{numeric vector}. If predictions must be unnormalized, set the first element of this vector to the mean and the second, to the standard deviation.
#' @param legend.pos A \code{character}. The position of the legend. Possible values are standard R plot values, i.e., "topright', "bottomleft', etc.
#' @param knit A \code{boolean}. Set this parameter to \code{TRUE} if 
#' 
#' @return Besides the prediction plot, this function returns an object whose fields are:
#' 
#' \itemize{
#' \item{\code{accuracy}: An object returned by \link[forecast]{accuracy}. It is a table containing several accuracy measures}
#' \item{\code{predictions}: A \code{numeric vector} containing the predicted values. }
#' }
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @importFrom zoo as.Date
#' @importFrom  stats as.ts end fitted frequency na.omit plot.ts qnorm qt sd start time ts uniroot window 
#' @importFrom graphics abline arrows axis barplot legend lines mtext par points text
#' @export
#' @import forecast dygraphs

BETS.predict = function(..., actual = NULL, main = "", ylab = "", xlim = NULL, style = "dygraphs", unnorm = NULL, legend.pos = "topright", knit = F){
  
  l = list(...)
  
  if(is.null(l$object)){
      model = l[[1]]
  }
  else{
    model = l$object
  }
  
  if(class(model)[1] == "arima" || class(model)[1] == "Arima" || class(model)[1] == "ARIMA" || class(model) == "HoltWinters"){
    preds = forecast(...)
  }
  else {
    preds = BETS.grnn.test(...)
  }
  
  if(!is.null(unnorm)){
    preds$x = preds$x*unnorm[2] + unnorm[1]
    preds$mean = preds$mean*unnorm[2] + unnorm[1]
    preds$fitted = preds$fitted*unnorm[2] + unnorm[1]
    
    if(!is.null(actual)){
      actual = actual*unnorm[2] + unnorm[1]
    }
    
    if(!is.null(preds$lower)){
      preds$lower = preds$lower*unnorm[2] + unnorm[1]
      preds$upper = preds$upper*unnorm[2] + unnorm[1]
    }
  }
  
  if(style == "dygraphs"){
    
    dt = as.ts(cbind(fit = preds$mean, upr =  preds$upper[,2], lwr = preds$lower[,2]))
    
    if(is.null(actual)){
      dt = cbind(hist = preds$x, md = dt)
      
      p = dygraph(dt, main = main) %>%
        dySeries("hist", label = "Actual") %>%
        dySeries(c("md.lwr", "md.fit", "md.upr"), label = "Predicted") %>%
        dyRangeSelector(strokeColor = "gray", fillColor = "gray") %>%
        dyAxis("y", label = ylab)
    }
    else {
      dt = cbind(hist = preds$x, md = dt, act = actual)
      
      p = dygraph(dt, main = main) %>%
        dySeries("hist", label = "Series") %>%
        dySeries("act", label = "Actual") %>%
        dySeries(c("md.lwr", "md.fit", "md.upr"), label = "Predicted") %>%
        dyRangeSelector(strokeColor = "gray", fillColor = "gray") %>%
        dyAxis("y", label = ylab)
    }
    
    if(knit == F){
      print(p)
    }
    else {
      return(p)
    }
    
  }
  else if(style == "normal") {

    max = max(c(preds$x,preds$mean)) 
    max = ceiling(max + 0.1*max)
    min = min(c(preds$x, preds$mean))
    min = floor(min - 0.1*min)
    step = floor((max - min)/4)
    
    y_last = preds$x[length(preds$x)]
    x_last = as.Date(preds$x)[length(preds$x)]
    
    if(!is.null(preds$lower)){
       series = preds
    }
    else{
      series = preds$x
    }
    
    plot(series, main = main, ylab = ylab, xlim = xlim, yaxt = "n", xaxp  = c(1900, 2500, 600))
    abline(v = seq(1900,2500,1), col = "gray60", lty = 3)
    axis(side = 2, at = seq(min,max,step))
    par(new = TRUE)
    
    if(!is.null(actual)){
      lines(actual, col = "firebrick3", lwd = 2)
      y_ac = actual[1]
      x_ac = as.Date(actual)[1]
    }
    
    y_pr = preds$mean[1]
    
    #lines(x = c(x_last, x_ac), y = c(y_last, y_ac), col = "firebrick3", lw = 2)
    #lines(x = c(x_last, x_ac), y = c(y_last, y_pr), col = "royalblue", lw = 2)
    
    if(is.null(preds$lower)){
      lines(preds$mean, col = "royalblue", lwd = 2)
    }
    
    if(!is.null(actual)){
      legend(legend.pos,col=c("firebrick3","royalblue"),
             lty=1,legend=c("Actual","Predicted"), cex = 0.7) 
    }
  }
  
  if(!is.null(actual)){
    
    results = vector(mode = "list")
    acc = accuracy(preds$mean, actual)
    results$accuracy = acc 
    results$predictions = preds
    results$forecasting.errors = preds$mean - actual
  }
  else {
    results = preds
  }
  
  return(invisible(results))
}