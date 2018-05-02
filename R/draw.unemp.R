<<<<<<< HEAD
#' @title Create a chart of the Open Unemployment Rate time series
#' 
#' @description  Creates a plot of series 10777
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom zoo as.Date
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.unemp = function(){
  
  unemp = BETSget(24369)
  # start = c(2006,1)
  # 
  # if(!is.null(start)){
  #   unemp = window(unemp, start = start)
  # }
  
  lims = chart.add_basic(ts = unemp, ylim = c(4,14), title = "Unemployment Rate (PNAD-C)", subtitle = "Metropolitan Regions", col = "royalblue", trend = TRUE, leg.pos = "bottom")
  chart.add_notes(unemp, ylim = lims[3:4], xlim = lims[1:2])
=======
#' @title Create a chart of the Open Unemployment Rate time series
#' 
#' @description  Creates a plot of series 10777
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom zoo as.Date
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.unemp = function(){
  
  unemp = BETS.get(24369)
  # start = c(2006,1)
  # 
  # if(!is.null(start)){
  #   unemp = window(unemp, start = start)
  # }
  
  lims = chart.add_basic(ts = unemp, ylim = c(4,14), title = "Unemployment Rate (PNAD-C)", subtitle = "Metropolitan Regions", col = "royalblue", trend = TRUE, leg.pos = "bottom")
  chart.add_notes(unemp, ylim = lims[3:4], xlim = lims[1:2])
>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9
}