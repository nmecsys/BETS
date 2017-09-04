#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @param ylim A \code{numeric vector}. Y axis limits.
#' @param xlim  A \code{numeric vector}. x axis limits.
#' @param type A \code{character}. The type of of plot (lines).
#' @param title A \code{character}. The plot title.
#' @param subtitle A \code{character}. The plot subtitle.
#' @param ts A \code{ts}. the ts object.
#' @param col A \code{character}. Color.
#' @param arr.size A \code{vector}.
#' @param arr.pos  A \code{vector}.
#' @param leg.pos  A \code{vector}.
#' @param trend    A \code{boolean}.
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

chart.add_basic = function(ts, xlim = NULL, ylim = NULL, type = "lines", title = "", subtitle = "", col = "firebrick4", arr.size = NULL, arr.pos = "v", leg.pos = "top", trend = FALSE){
  
  freq = 0 
  
  if(class(ts) != "data.frame"){
    freq = frequency(ts)
    series = ts
    dt = as.Date(ts)[length(ts)]
    labs = NULL
  }
  else {
    series = as.ts(ts[,"value"])
    dates = as.Date(ts[,"date"])
    dt = tail(dates,1)
    s = seq(1,nrow(ts),by = floor(nrow(ts)/8))
    labs = dates[s]
  }
  
  if(trend){
    requireNamespace("mFilter")
    hp = fitted(mFilter::hpfilter(series))
  }
  
  last = vector(mode = "numeric")
  last[1] = as.integer(format(dt, "%Y"))
  last[2] = as.integer(format(dt, "%m"))
  last[3] = as.integer(format(dt, "%d"))
  
  if(freq == 12){
    aval = paste0("Last available data: ",format(dt, "%b"),"/", format(dt,"%Y"))
  } else if(freq == 1) {
    aval = paste0("Last available data: ", format(dt,"%Y"))
  } else {
    aval = paste0("Last available data: ", last[1], "/", last[2], "/", last[3])
  }
  
  
  if(frequency(ts) != 1){
     m = c(7.1,4.1,3.1,2.1)
  } else {
     m = c(3.1,4.1,3.1,2.1) 
  }
  
  x0 = last[1] + last[2]/12 + last[3]/30
  val = round(series[length(series)],2)
  d = 0 
  
  par(font.lab = 2, cex.axis = 1.2, bty = "n", las = 1, mar= m)
 
  if(type == "lines"){
    
    if(is.null(labs)){
      plot(series, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = title, col = col, ylim = ylim, xlim = xlim)
    } else {
      plot.ts(x = dates, y = series, type = "l", xaxt = "n", ylim = ylim, lwd = 2.5, lty = 1, xlab = "", ylab = "", main = title, col = col)
      axis(1, at = labs, labels = labs, las=1, cex.axis = 0.75)
    }
  }
  else {
    xbar = barplot(as.vector(series), names.arg = as.vector(time(series)), xlab = "", ylab = "", main = title, col = col, ylim = ylim,  xpd = FALSE)
    if(trend == F){
        x0 = xbar[nrow(xbar),1] 
        s = 0
    } else {
        s = strwidth(val)/2
    }
  }
  
  mtext(subtitle)
  
  if(trend){
    
    if(type == "lines"){
      lines(hp, lty = 6, col = "darkgray", lwd = 2)
    } else {
      par(new = TRUE)
      plot(hp, lty = 6, col = "darkgray", lwd = 2, xaxt="n",yaxt = "n",xlab = "",ylab = "", ylim = ylim)
    }
  }
  
  if(nchar(val) >= 4 && type != "bar"){
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
  
  l = 0 
  
  if(trend){
    
    l = strheight(aval)
    
    if(leg.pos == "top"){
      legend("topleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
    } else if(leg.pos == "bottom") {
      legend("bottomleft", "Trend (HP Filter)", lty = 6, lwd = 2, col="darkgrey", bty = "n", cex = 0.9)
    }
  }
  
  if(leg.pos == "top"){
    text(xlim[1] + 0.2*x.spam, ylim[2] - 0.06*y.spam - l, aval, cex = 0.9)
  } else if (leg.pos == "bottom"){
    text(xlim[1] + 0.2*x.spam, ylim[1] + 0.06*y.spam + l, aval, cex = 0.9)
  } 
 
  if(type == "lines"){
    
    points(x0, val, pch = 21, cex = 1.25, lwd = 2, bg = col, col = "darkgray")
    
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
      
    } else {
      
      if(is.null(arr.size)){ 
        arr.size = y.spam/12
      }
      
      y0 = val
      y1 = val
      x1 = x0 - 0.02*x.spam
      x0 = x0 - arr.size
      
      text(x0 - strwidth(val) + d, y0, as.character(val), cex = 1.1, font = 2)
    }
    
    arrows(x0 = x0, x1 = x1, y0 = y0, y1 = y1, length = c(0.01*x.spam, 0.00006*y.spam), lwd = 2)
  }
  else {
    # text(x0 - strwidth(val)/2, val - sign(val)*0.7*strheight(val), as.character(val), cex = 0.9, font = 2)
      text(x0 - s, val/2, as.character(val), cex = 0.9, font = 2)
  }
  
  return(c(xlim,ylim))
}