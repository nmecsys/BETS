<<<<<<< HEAD
#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom zoo as.Date
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.ulc = function(){
  
  cut   = BETSget(11777)
  start = c(2006,1)
  
  if(!is.null(start)){
    cut = window(cut, start = start)
  }
  
  lims = chart.add_basic(ts = cut, title = "Unitary Labor Cost", subtitle = "US$ - June 1994 = 100", col = "firebrick4", arr.size = 25)
  chart.add_notes(cut, ylim = lims[3:4], xlim = lims[1:2])

=======
#' @title Create a chart of the Unitary Labor Cost time series
#' 
#' @description  Creates a plot of series 11777
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom zoo as.Date
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows

draw.ulc = function(){
  
  cut = BETS.get(11777)
  start = c(2006,1)
  
  if(!is.null(start)){
    cut = window(cut, start = start)
  }
  
  lims = chart.add_basic(ts = cut, title = "Unitary Labor Cost", subtitle = "US$ - June 1994 = 100", col = "firebrick4", arr.size = 25)
  chart.add_notes(cut, ylim = lims[3:4], xlim = lims[1:2])

>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9
}