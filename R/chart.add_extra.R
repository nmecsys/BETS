#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param ts A \code{ts}. the ts object.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' @param col A \code{character}. Color.
#' @param arr.size A \code{}.
#' @param arr.pos A \code{}.
#' @param leg.pos A \code{}.
#' @param leg.text A \code{}.
#' @param main.type A \code{}.
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom zoo as.Date
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' @importFrom graphics strheight strwidth
#' @importFrom utils tail
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

chart.add_extra= function(ts, ylim = NULL, xlim = NULL, col = "firebrick3", arr.size = NULL, arr.pos = "v", leg.pos = "top", leg.text = "", main.type = "lines"){
  
  freq = 0 
  
  if(class(ts) != "data.frame"){
    freq = frequency(ts)
    series = ts
    dt = as.Date(ts)[length(ts)]
  }
  else {
    series = as.ts(ts[,2])
    dt = as.Date(ts[nrow(ts),1])
  }
  
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  last[3] = as.integer(format(dt, "%d"))
  
  if(main.type == "bar"){
      xbar <- barplot(as.vector(series), plot = F)
      lines(x = xbar, y = as.vector(series),lwd = 2.5, lty = 1, col = col, xpd = T)
      x0 = xbar[nrow(xbar),1]
  } else {
     lines(series, lwd = 2.5, lty = 2, col = col, xpd = T) 
      x0 = last[1] + last[2]/12 + last[3]/30
  }
  
  val = round(series[length(series)],2)
  d = 0 
  
  if(nchar(val) >= 4){
    d = strwidth(val)/nchar(val)
  }
  
  if(is.null(xlim)){
    xlim = par("usr")[1:2]
  }
  
  if(is.null(ylim)){
    ylim = par("usr")[3:4]
  }

  x.spam = xlim[2] - xlim[1]
  y.spam = ylim[2] - ylim[1]
  
  l = strheight("a")
  
  if(leg.pos == "top"){
    legend("topleft", leg.text, lty = 6, lwd = 2, col= col, bty = "n", cex = 0.9)
    text(xlim[1] + 0.2*x.spam, ylim[2] - 0.06*y.spam - l, cex = 0.9)
  } else if(leg.pos == "bottom") {
    legend("bottomleft", leg.text, lty = 6, lwd = 2, col= col, bty = "n", cex = 0.9)
    text(xlim[1] + 0.2*x.spam, ylim[1] + 0.06*y.spam + l, cex = 0.9)
  }
  
  if(arr.pos == "v" || arr.pos == "h"){
    points(x0, val, pch = 21, cex = 1.25, lwd = 2, bg = col, col = "darkgray")
  }
  
  if(arr.pos == "v"){
    
    if(is.null(arr.size)){ 
      arr.size = y.spam/2
    }
    
    if(val > (ylim[1] + ylim[2])/2){
      x1 = x0
      y0 = val - arr.size 
      y1 = val - 0.02*y.spam
      h =  -strheight(val)
    } else {
      x1 = x0
      y0 = val + arr.size 
      y1 = val + 0.02*y.spam
      h = strheight(val)
    }
    
    text(x1 - d, y0 + h, as.character(val), cex = 1.1, font = 2)
    arrows(x0 = x0, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
    
    
  } else if(arr.pos == "h") {
    
    if(is.null(arr.size)){ 
      arr.size = y.spam/12
    }
    
    y0 = val
    y1 = val
    x1 = x0 - 0.02*x.spam
    x0 = x0 - arr.size
    
    text(x0 - strwidth(val) + d, y0, as.character(val), cex = 1.1, font = 2)
    arrows(x0 = x0, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
    
  }
 
  return(c(xlim,ylim))
}