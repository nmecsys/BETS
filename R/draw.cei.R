#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.cei = function(){
  
  file = paste0(system.file(package="BETS"), "/cei.csv")
  lei= read.csv2(file, stringsAsFactors = F)[,2]
  lei = window(ts(as.numeric(lei), start = c(1996,1),frequency = 12),start = c(2000,6))
  
  t = "<b>COINCIDENT ECONOMIC INDICATOR</b>"
  t = paste0(t,"<br><span style = 'font-size:15px'>The Conference Board with FGV/IBRE</span>")
  
  t <- list(
    x = 0.5,
    y = 1.17,
    text = t,
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  l = length(lei)
  
  a <- list(
    x = as.Date(lei)[l],
    y = lei[l],
    text = paste0("<b>",lei[l],"</b>"),
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
    pad = 1
  )
  
  dates = as.Date(lei)
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(width = 700, height = 450) %>%
    add_lines(x = dates, y = lei) %>%
    layout(title = t, 
           yaxis = list(tickfont = list(size = 22),range = c(60,115)),
           xaxis = list(tickfont = list(size = 17)),
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,t),
           shapes = list(
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2001-01-01", x1 = "2002-01-01", xref = "x",
                  y0 = 60, y1 = 115, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2003-01-01", x1 = "2003-07-01", xref = "x",
                  y0 = 60, y1 = 115, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2008-10-01", x1 = "2009-04-01", xref = "x",
                  y0 = 60, y1 = 115, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(lei)[l], xref = "x",
                  y0 = 60, y1 = 115, yref = "y")))
  
  
  return(p)
}