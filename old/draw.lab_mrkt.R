#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 

draw.lab_mrkt = function(){
  
  file = paste0(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = suppressWarnings(
    data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
  )
  
  inx = ts(sond[90:nrow(sond),21:22],start = c(2008,6),frequency = 12)
  
  t = "<b>LABOR INDICATORS - LEADING AND COINCIDENT</b>"
  t = paste0(t,"<br><span style = 'font-size:15px'>Seasonally Adjusted</span>")
  
  len = nrow(inx)
  
  if(is.na(inx[len,1])){
    len = len - 1
  }
  
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
    y = inx[len,1],
    text = paste0("<b>",inx[len,1],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -50,
    ax = 0,
    font = list(size = 22)
  )
  
  b <- list(
    x = as.Date(inx)[len],
    y = inx[len,2],
    text = paste0("<b>",inx[len,2],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 50,
    ax = 0,
    font = list(size = 22)
  )
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  dates = as.Date(inx)
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(width = 700, height = 450) %>%
    add_lines(x = dates, y = inx[,1], name = "Coincident") %>%
    add_lines(x = dates, y = inx[,2], name = "Leading") %>%
        layout(title = t, 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 17)),
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,b,t),
           legend = list(orientation = 'h', x = 0.27),
           shapes = list(
             list(type = "rect",
                fillcolor = rg, line = list(color = rg), opacity = 0.2,
                x0 = "2008-10-01", x1 = "2009-03-31", xref = "x",
                y0 = 55, y1 = 115, yref = "y"),
            list(type = "rect",
                fillcolor = rg, line = list(color = rg), opacity = 0.2,
                x0 = "2014-07-01", x1 = as.Date(inx)[nrow(inx)], xref = "x",
                y0 = 55, y1 = 113, yref = "y")))
  
  
  return(p)
}