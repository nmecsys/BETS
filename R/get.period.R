<<<<<<< HEAD
get.period <- function(start,frequency){
  
  if(class(start) != 'Date'){
    return(msg("Error: Argument 'start' must be a Date object"))
  }
  
  starting_year <- as.numeric(substr(start,1,4))
  
  if(frequency == 1){
    return(starting_year)
  }
  
  starting_month <- as.numeric(substr(start,6,7))
  
  if(frequency == 12){
    return(c(starting_year,starting_month))
  }
  
  if(frequency == 4){
    starting_quarter = ceiling(starting_month/3)
    return(c(starting_year,starting_quarter))
  }
  
  if(frequency == 52 || frequency == 365){
    return(1)    
  }
  
}
=======
get.period <- function(start,frequency){
  
  if(class(start) != 'Date'){
    return(msg("Error: Argument 'start' must be a Date object"))
  }
  
  starting_year <- as.numeric(substr(start,1,4))
  
  if(frequency == 1){
    return(starting_year)
  }
  
  starting_month <- as.numeric(substr(start,6,7))
  
  if(frequency == 12){
    return(c(starting_year,starting_month))
  }
  
  if(frequency == 4){
    starting_quarter = ceiling(starting_month/3)
    return(c(starting_year,starting_quarter))
  }
  
  if(frequency == 52 || frequency == 365){
    return(1)    
  }
  
}
>>>>>>> 71754a35b050d392363ebfd3f352018aae3558c9
