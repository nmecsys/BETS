#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr 
#' @importFrom forecast ma
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @import plotly 
#' @importFrom seasonal seas
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.gdp_comps = function(){
  
  gdp_comp = paste0(system.file(package="BETS"), "/mon_pib_comps.csv")
  data <- ts(read.csv2(gdp_comp, stringsAsFactors = F)[,-1],start = c(2000,1), frequency = 12)
  data <- aggregate(data)
  
  year2 = end(data)[1]
  year1 = end(data)[1]-1
  data <- window(data, start = year1)
  data[,5] = data[,5] - data[,6]
  data = data[,c(-6,-1)]
  data = t(data)
  rownames(data) =  c("Hous.<br>Exp.", "Gov.<br>Exp.","GFFK","NX")
  
  #s = apply(data[,-1], 1, function(x){sum(x)})
  # cbind(data[,1],s)
  
  colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
 
  a <- list(
    x = 0.18,
    y = 0.5,
    text = paste0("<b>", year1,"</b>"),
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 18)
  )
  
  b <- list(
    x = 0.82,
    y = 0.5,
    text = paste0("<b>", year2,"</b>"),
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 18)
  )
  
  
  m <- list(
    t = 50,
    pad = 1
  )
   
  p <- plot_ly(width = 700, height = 450) %>% 
  
      add_pie(labels = rownames(data), values = data[,1],
                textposition = 'inside',
                textinfo = "label+percent",
                insidetextfont = list(color = '#FFFFFF', size = 16),
                marker = list(colors = colors,
                              line = list(color = '#FFFFFF', width = 1)),
                showlegend = F,
                hole = 0.4,
                domain = list(x = c(0, 0.45), y = c(0, 1))) %>%
    
      add_pie(labels = rownames(data), values = data[,2],
              textposition = 'inside',
              textinfo = "label+percent",
              insidetextfont = list(color = '#FFFFFF', size = 16),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = F,
              hole = 0.4,
              domain = list(x = c(0.55, 1), y = c(0, 1))) %>%
        
        layout(title = '<b>GDP COMPONENTS</b><br><span style = "font-size:17">Nominal Yearly GDP - GDP Monitor (FGV/IBRE)</span>',
               annotations = list(a,b),
               titlefont = list(size = 19),
               margin = m,
                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  return(p)
}