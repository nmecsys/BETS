#' @title Create a chart of the Production Indicators time series
#' 
#' @description  Creates a plot of series 21859
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' @importFrom seasonal seas
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.indprod = function(){
  
  indprod = seasonal::final(seas(BETS.get(21859)))
  start = c(2006,1)
  
  if(!is.null(start)){
    indprod = window(indprod, start = start)
  }
  
  lims = chart.add_basic(ts = indprod, title = "Industrial Production", subtitle = "Seasonally Adjusted. Index (2012 = 100)", col = "chocolate1")
  chart.add_notes(indprod, ylim = lims[3:4], xlim = lims[1:2])

}