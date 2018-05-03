#' @title Create a chart of the Time Deposits time series 
#' 
#' @description  Creates a plot of series 14
#' 
#' @importFrom grDevices dev.new dev.off  pdf  png
#' @importFrom utils read.csv2
#' @importFrom stats ts plot.ts
#' @importFrom graphics axis text  points  mtext arrows
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory. 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.cdb= function(){

  cdb = BETSget(code = 14, data.frame = TRUE)
  
  if(!is.null(start)){
    
    # if(start[2] < 9){
    #   start[2] = paste0("0",start[2])
    # }
    # if(start[3] < 9){
    #   start[3] = paste0("0",start[3])
    # }
    # 
    # init = as.Date(paste0(start[1],"-",start[2],"-",start[3]), format = "%Y-%m-%d")
    init = as.Date("2006-01-01")
    
    i = which(cdb[,"date"] >= init)
    
    if(length(i) != 0){
      cdb = cdb[i,]
    }
  }
  
  lims = chart.add_basic(ts = cdb, title = "Time Deposits (CDB/RDB-Preset)", subtitle = "Daily Returns (%)", col = "darkolivegreen", leg.pos = "bottom")
  chart.add_notes(ts(cdb[,"value"], frequency = 365), ylim = lims[3:4], xlim = lims[1:2],dec = 4)
  
  
}