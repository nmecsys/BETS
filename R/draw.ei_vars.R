#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.ei_vars = function(){
  
  ei = file.path(system.file(package="BETS"), "/ei_vars.csv")
  data <- read.csv2(ei, stringsAsFactors = F)
  
  lei = ts(data[,2], start = c(2016,8),frequency = 12)
  lei_vars = ts(data[,3], start = c(2016,8), frequency = 12)
  cei = ts(data[,4], start = c(2016,8),frequency = 12)
  cei_vars = ts(data[,5], start = c(2016,8), frequency = 12)
  
  e = length(lei)
  s = e - 2
  
  lei = ts(data[s:e,2],end = end(lei), frequency = 12)
  lei_vars = ts(data[s:e,3],end = end(lei), frequency = 12)
  cei = ts(data[s:e,4],end = end(lei), frequency = 12)
  cei_vars = ts(data[s:e,5],end = end(lei), frequency = 12)
  
  m <- list(
    t = 60,
    l = 100,
    pad = 1
  )
  
  t <- list(
    x = 0.5,
    y = 1.2,
    text = "<b>ECONOMIC INDICATORS - LEADING AND COINCIDENT</b><br><span style = 'font-size:16px'>The Conference Board with FGV/IBRE</span>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  
  dates = as.Date(lei)
  ym = as.yearmon(dates)
  
  p = plot_ly(x = as.numeric(lei), y = as.Date(lei), name = "LEI", type = "bar", orientation = 'h', width = 700, height = 450, marker = list(color = 'rgb(171,104,87)')) %>% 
    add_trace(x = as.numeric(cei), y = as.Date(cei), name = "CEI", type = "bar", orientation = 'h', marker = list(color = 'rgb(114,147,203)')) %>%
    layout(title = "", 
           yaxis = list(tickfont = list(size = 20), tickvals = dates, ticktext=as.character(ym), showline = F, zeroline = F),
           xaxis = list(title = "", showgrid = FALSE, showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
           margin = m,
           annotations = t,
           titlefont = list(size = 19),
           showlegend = T,
           legend = list(orientation = 'h', x = 0.35))
  
  for(i in 1:3){
    
    text = paste0("<b>",lei[i],"</b>")
    y = dates[i] - 7
    x = as.numeric(lei[i])/2
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18, color = "#FFFFFF"))
    
    sig = ""
    if(cei[i] > 0) sig = "+"
    
    text = paste0("<b>",cei[i],"</b>")
    y = dates[i] + 7
    x = as.numeric(cei[i])/2
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18, color = "#FFFFFF"))
    
    sig = ""
    if(lei_vars[i] > 0) sig = "+"
    
    text = paste0("<b>",sig, lei_vars[i],"%</b>")
    y = dates[i] - 7
    x =  as.numeric(lei[i]) + 6.5
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18))
    
    sig = ""
    if(cei_vars[i] > 0) sig = "+"
    
    text = paste0("<b>",sig,cei_vars[i],"%</b>")
    y = dates[i] + 7
    x =  as.numeric(cei[i]) + 6.5
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18))
    
  }
  
  return(p)
}

