#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.gdp_mon = function(){
  
  gdp_comp = paste0(system.file(package="BETS"), "/mon_pib_comps.csv")
  data <- read.csv2(gdp_comp, stringsAsFactors = F)
  
  gdp = window(ts(as.numeric(data[,2]), start = c(2000,1), frequency = 12),start = c(2013,1))
  
  last_year = end(gdp)[1]
  last_month = end(gdp)[2]
  years = (last_year-2):last_year
  
  dates = vector(mode = "numeric")
  ia_vars = vector(mode = "numeric")
  lm_vars = vector(mode = "numeric")
  
  last = length(gdp)
  
  for(i in 0:2){
    ia_vars[i+1] = round(((gdp[last-i]-gdp[last-i-1])/gdp[last-i-1])*100,2)
    lm_vars[i+1] = round(((gdp[last-12*i]-gdp[last-(i+1)*12])/gdp[last-12*i])*100,2)
    dates[i+1] = paste0(years[i+1],"-01-",last_month)
  }
  
  ia_vars = ts(ia_vars, start = last_year-2, frequency = 1)
  lm_vars = ts(lm_vars, start = last_year-2, frequency = 1)
  
  m <- list(
    t = 60,
    l = 100,
    pad = 1
  )
  
  t <- list(
    x = 0.5,
    y = 1.2,
    text = "<b>GDP VARIATION</b><br><span style = 'font-size:16px'>GDP Monitor (FGV/IBRE)</span>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  ym = as.yearmon(dates)
  
  p = plot_ly(x = as.numeric(ia_vars), y = years, name = "Interanual Variation", type = "bar", orientation = 'h', width = 700, height = 450, marker = list(color = 'rgb(171,104,87)')) %>% 
    add_trace(x = as.numeric(lm_vars), y = years, name = "Monthly Variation", type = "bar", orientation = 'h', marker = list(color = 'rgb(114,147,203)')) %>%
    layout(title = "", 
           yaxis = list(tickfont = list(size = 20), tickvals = ym, ticktext = as.character(ym), showline = F, zeroline = F),
           xaxis = list(title = "", showgrid = FALSE, showline = FALSE, showticklabels = FALSE, zeroline = FALSE),
           margin = m,
           annotations = t,
           titlefont = list(size = 19),
           showlegend = T,
           legend = list(orientation = 'h', x = 0.2))
  
  for(i in 1:3){
    
    sig = ""
    if(ia_vars[i] > 0) sig = "+"
    
    text = paste0("<b>",sig, ia_vars[i],"%</b>")
    y = years[i] - 0.2
    x =  as.numeric(ia_vars[i])/2
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18))
    
    sig = ""
    if(lm_vars[i] > 0) sig = "+"
    
    text = paste0("<b>",sig, lm_vars[i],"%</b>")
    y = years[i] + 0.2
    x =  as.numeric(lm_vars[i])/2
    p = p %>% add_annotations(text = text, y = y, x = x, showarrow = F, font = list(size = 18))
    
  }
  
  return(p)
}