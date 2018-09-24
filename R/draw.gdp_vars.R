#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr 
#' @importFrom forecast ma
#' @import plotly 
#' @importFrom seasonal seas
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.gdp_vars = function(){
  
  gdp_comp = file.path(system.file(package="BETS"), "/mon_pib_vars.csv")
  data <- read.csv2(gdp_comp, stringsAsFactors = F)
  
  gdp = window(ts(as.numeric(data[,2]), start = c(2000,12), frequency = 12),start = c(2013,1))
  gdp_cons = window(ts(as.numeric(data[,3]), start = c(2000,12), frequency = 12),start = c(2013,1))
  gdp_gov = window(ts(as.numeric(data[,4]), start = c(2000,12), frequency = 12),start = c(2013,1))
  gdp_bffk = window(ts(as.numeric(data[,5]), start = c(2000,12), frequency = 12),start = c(2013,1))
  
  m <- list(
    t = 60,
    pad = 1
  )
  
  t <- list(
    x = 0.5,
    y = 1.25,
    text = "<b>GDP COMPONENTS VARIATION</b><br><span style = 'font-size:13'>Accumulated variation 12 Months - GDP Monitor (FGV/IBRE)</span>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  if(gdp[length(gdp)] < 0){
    y0 = 0 
  } else {
    y0 = gdp[length(gdp)]
  }
  
  a <- list(
    x = as.Date(gdp)[length(gdp)],
    y = y0,
    text = paste0("<b>",gdp[length(gdp)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -40,
    ax = 0,
    font = list(size = 22)
  )
  
  #dates = as.Date(gdp)
  #quarters = as.yearqtr(dates)
  
  p = plot_ly(type = "bar", x = as.Date(gdp), y = gdp, name = "GDP", width = 700, height = 450) %>% 
    add_trace(y = gdp_gov, x = as.Date(gdp), name = "Gov. Exp.", type = "scatter", mode = "lines") %>%
    add_trace(y = gdp_cons, x = as.Date(gdp), name = "Hous. Exp.", type = "scatter", mode = "lines") %>%
    add_trace(y = gdp_bffk, x = as.Date(gdp), name = "GFFK", type = "scatter", mode = "lines", line = list(color = "#908989")) %>%
    layout(title = '', 
           yaxis = list(tickfont = list(size = 20)),
           xaxis = list(tickfont = list(size = 15),tickangle = 60),
           #xaxis = list(tickfont = list(size = 15), tickangle = 60, tickvals = dates, ticktext=as.character(quarters), showgrid = T),
           margin = m,
           titlefont = list(size = 19),
           annotations = list(a,t),
           legend = list(orientation = 'h', x = 0.15, y = -0.33)
    )
  
  return(p)
}