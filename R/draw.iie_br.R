#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom forecast ma
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.iie_br = function(){
  
  #iiebr = paste0(system.file(package="BETS"), "/incerteza_fgv.csv")
  iiebr = BETSget("ST_iiebr")
  #data <- read.csv2(iiebr, stringsAsFactors = F)
  
  iiebr = ts(iiebr[,2], start = c(2000,1), frequency = 12)
  iiebr.ma = ma(iiebr,6)
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  a <- list(
    x = as.Date(iiebr)[length(iiebr)],
    y = iiebr[length(iiebr)],
    text = paste0("<b>",iiebr[length(iiebr)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 50,
    ax = 0,
    font = list(size = 22)
  )
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(iiebr), y = iiebr, name = "IIE-Br", 
                line = list(color = "#908989"), width = 700, height = 450) %>% 
        add_trace(y = iiebr.ma, x = as.Date(iiebr.ma), name = "MA 6 periods", line = list(color = "#bd081c", dash = "dash")) %>%
        layout(title = "<b>UNCERTAINTY INDEX</b><br>IIE-Br (FGV/IBRE)", 
                 yaxis = list(tickfont = list(size = 22), titlefont = list(size = 22)),
                 xaxis = list(tickfont = list(size = 22)),
                 margin = m,
                 titlefont = list(size = 19),
                 annotations = a,
                 legend = list(orientation = 'h', x = 0.3))
  
  return(p)
}