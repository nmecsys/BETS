as.date <- function(dates){
  
  not_formatted = grepl("/",dates)
  
  if(TRUE %in% not_formatted){
  
    for(i in 1:length(dates)){
      
      if(not_formatted[i]){
        
        dt = strsplit(dates[i],"/")[[1]]
        
        if(nchar(dt[1]) != 4){
          year = dt[3]
          dt[3] = dt[1]
          dt[1] = year
        }
        
        dates[i] = paste(dt,collapse = "-")
      }
    }
  }
  
  return(as.Date(dates))
}