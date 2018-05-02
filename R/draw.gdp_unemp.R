#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


draw.gdp_unemp = function(){
  
  gdp = window(BETSget(22109), start = c(2012,2))
  #ipca = window(aggregate(BETSget(433), nfrequency = 4), start = c(2012,2))
  #gdp = deflate(ts = gdp, deflator = ipca, type = "perc")
  
  # unemp = suppressWarnings(ts(BETSget(24369)[,2], start = c(2012,6),frequency = 12))
  # 
  unemp = BETSget(24369)
  
  ind = grepl("-03-|-06-|-09-|-12-", as.Date(unemp))
  unemp = ts(unemp[ind],start = c(2012,2), frequency = 4)
  unemp = window(unemp, end = end(gdp))
  
  last_val = gdp[length(gdp)]
  last_date = as.Date(gdp)[length(gdp)]
  
  a <- list(
    x = last_date,
    y = last_val,
    text = paste0("<b>",last_val,"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 22)
  )
  
  last_val = unemp[length(unemp)]
  last_date = as.Date(unemp)[length(unemp)]
  
  b <- list(
    x = last_date,
    y = last_val,
    text = paste0("<b>",last_val,"</b>"),
    xref = "x",
    yref = "y2",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 22)
  )
  
  ay <- list(
    overlaying = "y",
    side = "right",
    zeroline = FALSE,
    showgrid = FALSE,
    tickfont = list(size = 22)
  )
  
  m <- list(
    t = 50,
    pad = 1,
    r = 60
  )
  
  dates = as.Date(gdp)
  quarters = as.yearqtr(dates)
  
  p = plot_ly(mode = "lines", type = "scatter", x = as.Date(gdp), y = gdp, name = "Quaterly GDP (Index)", width = 700, height = 450) %>%
    add_lines(yaxis = "y2", x = as.Date(unemp), y = unemp, name = "Unemployment Rate") %>%
    layout(title = "<b>GDP x UNEMPLOYMENT RATE</b>", 
           yaxis = list(tickfont = list(size = 22)),
           xaxis = list(tickfont = list(size = 15), tickangle = 60, tickvals = dates, ticktext=as.character(quarters)),
           yaxis2 = ay,
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,b),
           legend = list(orientation = 'h', x = 0.3, y = -0.33))
  
  return(p)
}