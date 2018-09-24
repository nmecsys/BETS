#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.cap_utl = function(){
  
  file = file.path(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = suppressWarnings(
    data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
  )
  
  inx = ts(sond[148:nrow(sond),c(5,9,13)],start = c(2013,4),frequency = 12)
  inx = ts(round((inx[,1] + inx[,2] + inx[,3])/3,2), start = c(2013,4), frequency = 12)
  
  t = "<b>CAPACITY UTILIZATION</b>"
  t = paste0(t,"<br><span style = 'font-size:15px'>Seasonally Adjusted</span>")
  
  len = length(inx)
  
  t <- list(
    x = 0.5,
    y = 1.17,
    text = t,
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  a <- list(
    x = as.Date(inx)[len],
    y = inx[len],
    text = paste0("<b>",inx[len],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -50,
    ax = 0,
    font = list(size = 22)
  )
  
  m <- list(
    t = 60,
    pad = 1,
    b = 60,
    r = 15
  )
  
  dates = as.Date(inx)
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(width = 700, height = 450) %>%
    add_lines(x = dates, y = inx, name = "Coincident") %>%
    layout(title = t, 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 17)),
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,t),
           shapes = 
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(inx)[len], xref = "x",
                  y0 = 70, y1 = 85, yref = "y"))
  
  
  return(p)
}