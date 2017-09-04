require(BETS); require(lubridate)
t1 <- BETS.get(1212)

collapse <- function(x, method = c("sum","mean","3m","6m","gm","index","percentage"), periodicity, begin){
    
    
    if(missing(periodicity)){ stop("Series periodicity must be supplied.")}
    
    if(missing(begin)){ begin <- 1}
        
    
    if( periodicity = 12){
        if( begin < 0 | begin > 12){ stop("Begin must be positive and equal or less than 12.")}
        
        
        
    } else if( periodicity = 252){
        if( begin < 0 | begin > 252){ stop("Begin must be positive and equal or less than 252.")}
        
        
    } else if( periodicity = 360){
        if( begin < 0 | begin > 360){ stop("Begin must be positive and equal or less than 360.")}
        
        
    } else if( periodicity = 365){
        if( begin < 0 | begin > 365){ stop("Begin must be positive and equal or less than 365.")}
        
        
    } else if( periodicity = 4){
        if( begin < 0 | begin > 4){ stop("Begin must be positive and equal or less than 4.")}
        
        
    }
    
    lubridate::as_date(t1)
    
    y <- c()
    
    for(i in n:3){
        y[i] <-  x[i]*x[i-1]*x[i-2]
    }
    
    t1 
    
    
    
    
    
    
}



geom3 <- function(x, anual = F, frequency){
  # x: ts
  browser()
  n <- length(x)
  data <- data.frame(x, y = rep(NA, n), w = rep(NA, n), z = rep(NA, n), row.names = 1:n)
  colnames(data)[1] <- "x"
  data$y <- data$x/100 
  
  for(i in n:3){
    data[i,"w"] <-  data$y[i]*data$y[i-1]*data$y[i-2]
  }
  data[,"w"] <- data[,"w"]^(1/3)
  if(anual){data[,"w"] <- data[,"w"]^12}
  data$z <- (data$w - 1)*100
  st <- ts(data$z, start = start(x), end = end(x), frequency = frequency)
  st
}

geom3(t1)
