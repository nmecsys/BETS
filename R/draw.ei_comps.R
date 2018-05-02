#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date 
#' @import plotly 

draw.ei_comps = function(){
  
  file.cei = paste0(system.file(package="BETS"), "/cei_comps.csv")
  cei <- read.csv2(file.cei, stringsAsFactors = F)
  
  file.lei = paste0(system.file(package="BETS"), "/lei_comps.csv")
  lei <- read.csv2(file.lei, stringsAsFactors = F)
  
  lei.labs = c("Swap<br>Rate","Manufacturing<br>Expec.", "Services<br>Expec.", "Consumers<br>Expec.", "Stock<br>Prices", "Terms of<br>Trade", "Consumer<br>Durable Goods<br>Production Exp.", "Exports<br>Volume")
  cei.labs = c("Industrial<br>Prod.","Ind. Electric<br>Energy Cons.","Shipments of<br>Corrugated<br>Paper", "Volume of<br>Sales (Ret.)", "Employement", 'Avg. Real<br>Income (Workers)')

  cei$Value = as.numeric(cei$Value)
  cei = cbind(cei, cei.labs)[,2:3]
  cei = cei[order(cei$Value, decreasing = T),]
  
  lei$Value = as.numeric(lei$Value)
  lei = cbind(lei, lei.labs)[,2:3]
  lei = lei[order(lei$Value, decreasing = T),]
  
  lei.x <- t(lei[,1])
  lei.y <- "LEI"
  data.lei = data.frame(lei.y, lei.x)
  
  cei.x <- t(cei[,1])
  cei.y <- "CEI"
  data.cei = data.frame(cei.y, cei.x)
  
  m <- list(
    t = 80,
    pad = 1
  )
  
  t <- list(
    x = 0.5,
    y = 1.175,
    text = "<b>LEI AND CEI COMPONENTS VARIATION</b>",
    xref = "paper",
    yref = "paper",
    showarrow = F,
    font = list(size = 19)
  )
  
  cols = c('rgba(38, 24, 74, 1)', 'rgba(38, 24, 74, 0.9)', 'rgba(38, 24, 74, 0.8)', 'rgba(71, 58, 131, 0.8)', 'rgba(71, 58, 134, 0.7)', 'rgba(164, 163, 204, 0.85)', 'rgba(190, 192, 213, 1)','rgba(122, 120, 168, 0.8)')
  
  p1 <- plot_ly(data.lei, type = 'bar', orientation = 'h',  width = 700, height = 450) %>%
          layout(xaxis = list(title = "",
                              showgrid = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              zeroline = TRUE,
                              zerolinecolor = '#969696',
                              zerolinewidth = 3),
                 yaxis = list(showgrid = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              zeroline = FALSE,
                              domain = c(0, 0.75)),
                 barmode = 'relative',
                 annotations = t,
                 showlegend = FALSE) %>%
                 add_annotations(xref = 'paper', yref = 'paper', 
                                  xanchor = 'right', x = 0.0, y = 0.45, 
                                  text = paste0("<b>",data.lei[1,1],"</b>"), showarrow = F,font = list(size = 18))
      
  p2 <- plot_ly(data.cei, type = 'bar', orientation = 'h',  width = 700, height = 450) %>%
          layout(xaxis = list(title = "",
                              showgrid = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              zeroline = TRUE,
                              zerolinecolor = '#969696',
                              zerolinewidth = 3),
                 yaxis = list(showgrid = FALSE,
                              showline = FALSE,
                              showticklabels = F,
                              zeroline = FALSE,
                              domain = c(0,0.75)),
                 barmode = 'relative',
                 #paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
                 showlegend = FALSE) %>%
          add_annotations(xref = 'paper', yref = 'paper', 
                          xanchor = 'right', x = 0, y = 0.3, 
                          text = paste0("<b>",data.cei[1,1],"</b>"), showarrow = F,font = list(size = 18))
  
  val.pos = 0
  val.neg = 0
  
  for(i in 1:nrow(lei)){
    
    val = as.numeric(lei[i,1])
    
    if(abs(val) >= 0.1){
      
      sig = ""
      
      if(val > 0){
        sig = "+"
        pos = val.pos + val/2
        val.pos = val.pos + val
      } else{
        pos = val.neg + val/2
        val.neg = val.neg + val
      }
      
      
      p1 = p1 %>% add_trace(x = data.lei[1,i+1], name = lei.labs[1], marker = list(color = cols[i])) %>% 
                  add_annotations(xref = 'x', yref = 'paper', 
                                  x = pos, y = 0.45, 
                                  text = paste0("<b>", sig, val,"%</b>"), 
                                  showarrow = F,font = list(size = 12, color = "#FFFFFF")) %>%
        add_annotations(xref = 'x', yref = 'paper', 
                        x = pos, y = 0.93, 
                        text = lei[i,2], 
                        showarrow = F,font = list(size = 12))
    }
  }
  
  lei.vals = lei[abs(lei$Value) >= 0.1,1]
  lei.pos = lei.vals[lei.vals > 0]
  lei.neg = lei.vals[lei.vals < 0]
  
  cei.vals = cei[abs(cei$Value) > 0.03,1]
  cei.pos = cei.vals[cei.vals > 0]
  cei.neg = cei.vals[cei.vals < 0]
  
  fac.pos =  sum(lei.pos)/sum(cei.pos)
  fac.neg = sum(abs(lei.neg))/sum(abs(cei.neg))
  
  val.pos = 0
  val.neg = 0
  
  for(i in 1:nrow(cei)){
    
    val = as.numeric(cei[i,1])
    
    if(abs(val) > 0.03){
      
      sig = ""
      
      if(val > 0){
        sig = "+"
        pos = fac.pos*(val.pos + val/2)
        val.pos = val.pos + val
      } else{
        pos = fac.neg*(val.neg + val/2)
        val.neg = val.neg + val
      }
      
      p2 = p2 %>% add_trace(x = data.cei[1,i+1], name = cei.labs[1], marker = list(color = cols[i])) %>% 
        add_annotations(xref = 'x', yref = 'paper', 
                        x = pos, y = 0.33, 
                        text = paste0("<b>", sig, val,"%</b>"), 
                        showarrow = F,font = list(size = 12, color = "#FFFFFF")) %>%
        add_annotations(xref = 'x', yref = 'paper', 
                        x = pos, y = 0.85, 
                        text = cei[i,2], 
                        showarrow = F,font = list(size = 12))
    }
  }
  
  s = subplot(p1,p2, nrows = 2)                                                                  
  
  return(s)
}