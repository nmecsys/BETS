#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom grDevices rgb
#' @importFrom utils read.csv2
#' @import plotly 

draw.animal_spirits = function(){
  
  sond = paste0(system.file(package="BETS"), "/sondagem_fgv.csv")
  fec = paste0(system.file(package="BETS"), "/fecomercio.csv")
  
  data <- read.csv2(sond, stringsAsFactors = F)
  data2 <- read.csv2(fec, stringsAsFactors = F)
  
  ISA = (data[,3] + as.numeric(data2[,2]))/2
  IE = (data[,4] + as.numeric(data2[,3]))/2 
  P = IE - ISA
  
  as = ts(round(P[-length(P)],2), start = c(2001,1), frequency = 12)
  
  a <- list(
    x = as.Date(as)[length(as)],
    y = as[length(as)],
    text = paste0("<b>",as[length(as)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 22)
  )
  
  t <- list(
    x = 0.5,
    y = 1.18,
    text = "<b>ANIMAL SPIRITS</b><br><span style = 'font-size:17'>Index</span>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  m <- list(
    t = 60,
    pad = 1
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(as), y = as, name = "Animal Spirits", width = 700, height = 450) %>% 
    layout(title = "", 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 22)),
           margin = m,
           annotations = list(a,t),
           shapes = list(
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2001-01-01", x1 = "2001-12-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2003-01-01", x1 = "2003-06-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2008-10-01", x1 = "2009-03-31", xref = "x",
                  y0 = -20, y1 = 45, yref = "y"),
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(as)[length(as)], xref = "x",
                  y0 = -20, y1 = 45, yref = "y")))
  
  return(p)
}