#' @title  Add notes
#' 
#' @description  Add notes
#' 
#' @param series.list A \code{ts object}
#' @param xlim A \code{vector}
#' @param ylim A \code{vector}
#' @param names A \code{character}
#' @param dec An \code{integer}
#' 
#' 
#' @importFrom graphics par points  text
#' @importFrom stats frequency
#' @importFrom graphics strheight strwidth
#' 
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}


chart.add_notes = function(series.list, xlim, ylim, names = NULL, dec = 2){
  
  par(xpd=NA)
  
  if(class(series.list) == "ts"){
    series.list = list(series.list)
  }
  
  x.spam = xlim[2] - xlim[1]
  y.spam = ylim[2] - ylim[1]
  
  if(length(series.list) == 1){
    mg = 5.8*strheight("A")
    divs = 3
  }
  else{
    if(is.null(names)){
      names = names(series.list)
    }
    mg = 9*strheight("A")
    divs = length(series.list)*2 + 2 
  }
  
  x.coords = vector(mode = "numeric")

  dist = x.spam/divs
  
  for(i in 1:(divs-1)){
    x.coords[i] = xlim[1] + i*dist - strwidth("AAAAA")
  }

  y.coord = ylim[1] - mg
  j = 1
  
  for(i in 1:length(series.list)){
    
    series = series.list[[i]]
    len = length(series)
    freq = frequency(series)
    
    last.period.val = paste0(round((series[len]/series[len-1] - 1)*100,2),"%")
    last.year.val = paste0(round((series[len]/series[len-freq] - 1)*100,2),"%")
    
    if(freq != 365){
      dt.lp = as.Date(series)[len-1]
      dt.ly = as.Date(series)[len-freq]
      
      last.period.comp = paste0(format(dt.lp,"%b"),"/", format(dt.lp,"%Y"),": ", round(series[len-1],dec))
      last.year.comp = paste0(format(dt.ly,"%b"),"/", format(dt.ly,"%Y"), ": ", round(series[len-freq],dec))
    }
    else {
      last.period.comp = paste0("A day before: ", round(series[len-1],dec))
      last.year.comp = paste0("A month before: ", round(series[len-30],dec))
    }

    d.ly = 0

    if(nchar(last.year.val) == 4){
      d.ly = 0.05*x.spam
    }
    
    if(nchar(last.year.val) == 5){
      d.ly = 0.015*x.spam
    }
    
    x.coords[j] = x.coords[j] - 0.01*x.spam
    x.coords[j + 1] = x.coords[j + 1] + 0.04*x.spam
    
    if(last.period.val > 0){
      points(x = x.coords[j] + d.ly, y = y.coord - 0.022*y.spam, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.period.val < 0){
     points(x = x.coords[j] + d.ly, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j] + d.ly, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.period.val, x = x.coords[j] + 0.075*x.spam, y = y.coord - 0.01*y.spam, cex = 1.1, font = 2)
    text(last.period.comp, x = x.coords[j] +  0.075*x.spam, y = y.coord - 0.1*y.spam, cex = 0.9)
    
    if(last.year.val > 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord - 0.022*y.spam, pch = 24, col = "blue", bg = "blue", cex = 1.3)
    }
    else if(last.year.val < 0){
      points(x = x.coords[j+1] + d.ly, y = y.coord, pch = 25, col = "red", bg = "red", cex = 1.3) 
    }
    else {
      points(x = x.coords[j+1]+ d.ly, y = y.coord, pch = "-", col = "green", bg = "green", cex = 1.3) 
    }
    
    text(last.year.val, x = x.coords[j+1] + 0.075*x.spam, y = y.coord - 0.01*y.spam, cex = 1.1, font = 2)
    text(last.year.comp, x = x.coords[j+1] + 0.075*x.spam, y = y.coord - 0.1*y.spam, cex = 0.9)
    
    if(!is.null(names)){
      title.x = 0.5*x.coords[j] + 0.5*x.coords[j+1] + 0.04*x.spam
      title.y = y.coord + 2*strheight("A")
      text(names[i], x = title.x, y = title.y, cex = 0.9, font = 2)
    }
    
    j = j + 3
  }
}