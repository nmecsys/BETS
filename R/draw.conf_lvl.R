#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.conf_lvl = function(){
  
  file = file.path(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
  
  emp = (sond[,2] + sond[,6] + sond[,10] + sond[,16])/4
  emp = round(emp[!is.na(emp)],2)
  emp = ts(emp, start = c(2010,7), frequency = 12)
  
  cons = ts(sond[115:nrow(sond),17],start = c(2010,7),frequency = 12)
  
  last_val = emp[length(emp)]
  last_date = as.Date(emp)[length(emp)]
  
  a <- list(
    x = last_date,
    y = last_val,
    text = paste0("<b>",last_val,"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 60,
    ax = 0,
    font = list(size = 22)
  )
  
  last_val = cons[length(cons)]
  last_date = as.Date(cons)[length(cons)]
  
  b <- list(
    x = last_date,
    y = last_val,
    text = paste0("<b>",last_val,"</b>"),
    xref = "x",
    yref = "y2",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -60,
    ax = 0,
    font = list(size = 22)
  )
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(emp), y = emp, name = "Enterprises", width = 700, height = 450) %>%
    add_lines(x = as.Date(cons), y = cons, name = "Consumers") %>%
    layout(title = "<b>CONFIDENCE INDEX - ENTERPRISES AND CONSUMERS</b>", 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 15)),
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,b),
           legend = list(orientation = 'h', x = 0.17, y = -0.33),
           shapes = list(type = "rect",
                         fillcolor = rg, line = list(color = rg), opacity = 0.2,
                         x0 = "2014-07-01", x1 = as.Date(emp)[length(emp)], xref = "x",
                         y0 = 60, y1 = 115, yref = "y")
           )
  
  return(p)
}