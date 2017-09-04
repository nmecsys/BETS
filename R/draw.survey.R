#' @title Create a chart of the Base Interest Rate (SELIC) time series
#' 
#' @description  Creates a plot of series 4189
#' 
#' @param survey xxx
#' 
#' @return An image file is saved in the 'graphs' folder, under the BETS installation directory.
#' @importFrom zoo as.Date as.yearqtr
#' @importFrom grDevices rgb
#' @importFrom utils tail
#' @import plotly 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}

draw.survey = function(survey){
  
  file = paste0(system.file(package="BETS"), "/sondagens_completo_fgv.csv")
  sond = read.csv2(file, stringsAsFactors = F)
  sond[,-1] = suppressWarnings(
                data.frame(lapply(sond[,-1], function(x){as.numeric(gsub(",",".",x))}))
              )
  
  #head(na.omit(sond[57:nrow(sond),c(1,10:12)]),1)
  
  if(survey == "consm"){
    
    inx = ts(sond[57:nrow(sond),17:19],start = c(2005,9),frequency = 12)
    inx = window(inx, start = c(2015,1))
    t = "<b>CONSUMERS SURVEY</b>"
    
  } else if(survey == "constr"){
    
    inx = ts(sond[115:nrow(sond),14:16],start = c(2010,7),frequency = 12)
    inx = window(inx, start = c(2015,1))
    
    temp = inx[,3]
    inx[,3] = inx[,1]
    inx[,1] = temp
    
    t = '<b>CONSTRUCTION INDUSTRY SURVEY</b>'
    
  } else if(survey == 'retail'){
    
    inx = ts(sond[111:nrow(sond),10:12],start = c(2010,3),frequency = 12)
    inx = window(inx, start = c(2015,1))
    t = "<b>RETAILERS SURVEY</b>"
    
  } else if(survey == 'servc'){
    
    inx = ts(sond[90:nrow(sond),6:8],start = c(2008,6),frequency = 12)
    inx = window(inx, start = c(2015,1))
    t = "<b>SERVICES SURVEY</b>"
    
  } else if(survey == 'transf_ind'){
    
    inx = ts(sond[57:nrow(sond),2:4],start = c(2005,9),frequency = 12)
    inx = window(inx, start = c(2015,1))
    t = "<b>MANUFACTURING INDUSTRY SURVEY</b>"
    
  }
  
  t = paste0(t,"<br><span style = 'font-size:16px'>Index</span>")
  
  max = max(inx[,1],inx[,2],inx[,3])
  min = min(inx[,1],inx[,2],inx[,3])
  
  if(inx[nrow(inx),1] < 0){
    y0 = 0 
  } else {
    y0 = inx[nrow(inx),1]
  }
  
  a <- list(
    x = tail(as.Date(inx),1),
    y = y0,
    text = paste0("<b>",inx[nrow(inx),1],"</b>"),
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 6,
    ay = -50,
    ax = 0,
    font = list(size = 22)
  )
  
  m <- list(
    t = 50,
    pad = 1
  )
  
  dates = as.Date(inx)
  months = as.yearmon(dates)
  
  p = plot_ly(width = 700, height = 450) %>%
        add_lines(x = dates, y = inx[,3], name = "Expectations") %>%
        add_lines(x = dates, y = inx[,2], name = "Present Situation") %>%
        add_trace(type = "bar", x = dates, y = inx[,1], name = "Confidence Index", marker = list(color = "#908989")) %>%
        layout(title = t, 
               yaxis = list(tickfont = list(size = 22), range = c(min-10,max+10)),
               xaxis = list(tickfont = list(size = 15), tickangle = 60, tickvals = dates, ticktext=as.character(months)),
               margin = m,
               titlefont = list(size = 19),
               annotations = a,
               legend = list(orientation = 'h', x = 0.17, y = -0.33))
  
  
  return(p)
}