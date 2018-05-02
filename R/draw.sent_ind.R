#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.sent_ind = function(){
  
  file = paste0(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
  
  emp = (sond[,2] + sond[,6] + sond[,10] + sond[,16])/4
  emp = round(emp[!is.na(emp)],2)
  emp = ts(emp, start = c(2010,7), frequency = 12)
  
  cons = ts(sond[115:nrow(sond),17],start = c(2010,7),frequency = 12)
  
  tot = (cons + emp)/2
  
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
  
  last_val = cons[length(tot)]
  last_date = as.Date(cons)[length(tot)]
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(tot), y = tot, width = 700, height = 450) %>%
    layout(title = "<b>ECONOMIC SENTIMENT INDICATOR</b>", 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 15)),
           margin = m,
           titlefont = list(size = 19),
           annotations = a,
           shapes = list(type = "rect",
                         fillcolor = rg, line = list(color = rg), opacity = 0.2,
                         x0 = "2014-07-01", x1 = as.Date(emp)[length(emp)], xref = "x",
                         y0 = 60, y1 = 115, yref = "y")
    )
  
  return(p)
}