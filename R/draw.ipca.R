#' @title Create a chart of the National Consumer Price Index time series
#' 
#' @description  Creates a plot of series 13522 (NCPI), along with series 4466 (NCPI core)
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#'
#' @importFrom zoo as.Date
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' @importFrom graphics  strwidth
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


draw.ipca = function(){
  
  ipca = BETSget(13522)
  core = BETSget(4466)
  start = c(2006,1)
  
  if(is.null(start)){
    
    start = vector(mode = "logical")
    
    if(start(ipca) > start(core)){
      start = start(ipca)
    }
    else if(start(ipca) == start(core)) {
      start[1] = start(ipca)[1]
      start[2] = max(start(ipca)[2],start(core)[2])
    }
    else {
      start = start(core)
    }
  }
  
  ipca = window(ipca, start = start)
  
  core_acc = vector(mode = "numeric")
  
  for(i in 12:length(core)){
    sum = 0 
    for(j in 1:11){
      sum = sum + core[i-j]
    }
    core_acc[i-11] = sum 
  }
  
  par(mar = c(7,4,4,2))
  
  core_acc = ts(core_acc, start = c(1996,12), frequency = 12)
  core = window(core_acc, start = start)
  
  lims = chart.add_basic(ts = ipca, title = "National Consumer Price Index (IPCA)", subtitle = "Cumulative 12-Month Percentage", arr.pos = "h", leg.pos = "none")
  chart.add_extra(core, ylim = lims[3:4], xlim = lims[1:2], leg.pos = "none")
  abline(a = 4.5, b = 0, lty = 3, lwd = 3, col = "darkgray")
  
  legend("topleft", c("IPCA", "Core"), lty=c(1,2), lwd=c(2.5,2.5),col=c("firebrick4", "firebrick3"), bty = "n", cex = 0.9)
  text(lims[2] - 3*strwidth("Target"), 4.1, "Target", cex = 0.9)
  
  chart.add_notes(list(ipca = ipca, core = core), names = c("IPCA","Core"), ylim = lims[3:4], xlim = lims[1:2])

}