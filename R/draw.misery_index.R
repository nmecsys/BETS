#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @importFrom grDevices rgb
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.misery_index = function(){
  
  ipca = suppressWarnings(BETS.get(13522)) 
  #ipca = ts(ipca[,"value"], start = c(1980,12), frequency = 12)
  ipca = window(ipca, start = c(2012,3))
  unemp = BETS.get(24369)
  misery = ipca + unemp 
  
  # Consumer Confidence
  
  file = paste0(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
  
  cons = window(ts(sond[115:nrow(sond),17],start = c(2010,7),frequency = 12),start = c(2012,3))
  
  a <- list(
    x = as.Date(misery)[length(misery)],
    y = misery[length(misery)],
    text = paste0("<b>",misery[length(misery)],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -40,
    ax = 0,
    font = list(size = 22)
  )
  
  b <- list(
    x = as.Date(cons)[length(cons)],
    y = cons[length(cons)],
    text = paste0("<b>",cons[length(cons)],"</b>"),
    xref = "x",
    yref = "y2",
    showarrow = TRUE,
    arrowhead = 6,
    ay = 40,
    ax = 0,
    font = list(size = 22)
  )
  
  t <- list(
    x = 0.5,
    y = 1.18,
    text = "<b>MISERY INDEX x CONSUMER CONFIDENCE</b>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  ay <- list(
    overlaying = "y",
    side = "right",
    zeroline = FALSE,
    showgrid = FALSE,
    tickfont = list(size = 22)
  )
  
  m <- list(
    t = 100,
    pad = 1,
    r = 60
  )
  
  rg = rgb(162,7,7, maxColorValue = 255)
  
  p = plot_ly(width = 700, height = 450) %>%
    add_lines(x = as.Date(misery), y = misery, name = "Misery Index") %>%
    add_lines(yaxis = "y2", x = as.Date(cons), y = cons, name = "Consumer Confidence") %>%
    layout(title = "", 
           yaxis = list(tickfont = list(size = 22)),
           yaxis2 = ay,
           xaxis = list(title = "", tickfont = list(size = 22)),
           margin = m,
           annotations = list(t,a,b),
           legend = list(orientation = 'h', x = 0.17, y = -0.33),
           shapes = list(
             list(type = "rect",
                  fillcolor = rg, line = list(color = rg), opacity = 0.2,
                  x0 = "2014-07-01", x1 = as.Date(misery)[length(misery)], xref = "x",
                  y0 = 10, y1 = 22, yref = "y")
             )
           )
  
  return(p)
}