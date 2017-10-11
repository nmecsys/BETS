# Uma unica serie
BETS.get(4447)

# Mesma serie, filtrada por data de inicio
BETS.get(4447, from = "2000-01-01")

# Mesma serie, filtrada por data de fim
BETS.get(4447, to = "2000-01-01")

# Mesma serie, filtrada por data de inicio e de fim
BETS.get(4447, from = "1998-01-01", to = "2000-01-01")

# Duas series, filtradas por 1 data de inico e fim, caso 1
BETS.get(code = c(10777,4447),from = "2001-01-01", to = "2016-10-31")

# Duas series, filtradas por 1 data de inicio e fim, caso 2
BETS.get(code = c(10777,4447),from = c("2001-10-31",""),to = c("2016-10-31",""))

# Duas series, filtradas por datas diferentes 
BETS.get(code = c(10777,4447),from = c("2001-10-31","1998-09-01"), to = c("2014-10-31","2015-01-01"))

# Duas series, data de inicio igual, datas fim diferentes
BETS.get(code = c(10777,4447),from = "2001-10-31", to = c("2014-10-31","2015-01-01"))

# Duas series, datas de inicio diferentes, data fim igual
BETS.get(code = c(10777,4447),from = c("2002-10-31","1997-01-01"), to = "2015-01-01")


TEST_BETS.get = function(db = "bacen", lang = "en"){
  
  require(rowr)
  require(BETS)
  
  if(db == "bacen"){
    if(lang == "en"){
      database = "bacen_v7"
    }
    else {
      database = "base_final_ptv1"
    }
  }
  else{
    stop("Database is not available.")
  }
  
  test.dir = paste0(getwd(),"\\tests")
  
  if(!file.exists(test.dir)){
    dir.create(test.dir)
  }
  
  log.file = file.path(test.dir, paste0(database,".log"))
  file.create(log.file, overwrite = T)
  
  githubURL<- paste0("https://github.com/GreedBlink/databases/raw/master/",database,".Rdata")
  load(url(githubURL))
  metadata <- get(database)
  
  # Open a file connection and write a meaningful header
  conn <-file(log.file, "w")
  header <- paste("-- TESTING BETS.get with ", database, " metadata @", Sys.time())
  write(header, conn)
  
  # Get all codes
  codes = suppressWarnings(as.numeric(metadata[,1]))
  codes <- codes[!is.na(codes)]
  
  # Check if the series are available 
  write(paste0("## Errors and issues in BETS.get with ", database, "metadata "), conn, append = TRUE)
  
  errors = vector(mode = "integer")
  issues = vector(mode = "character")
  code = NULL
  
  for(i in 1:length(codes)){
    
    code = tryCatch({
      
            info = paste0("TESTANDO ",i,"a serie, codigo ",codes[i])
            
            ret = BETS.get(codes[i])
            
            if(class(ret) == "ts" || class(ret) == "data.frame"){
              info = paste(info,"-- OK")
            }
            else {

              info = paste(info, "-- ISSUE")
              message = paste0("?? ISSUE when code = ", codes[i], ". MESSAGE = ", ret)
              write(message, conn, append = TRUE)
              issues = c(codes[i],issues)
            }
            
            print(info)
            
          },
            error = function(e){
              
              message = paste0("!! ERROR when code = ", codes[i], ". MESSAGE = ", e)
              write(message, conn, append = TRUE)
              
              print(paste0("TESTANDO ",i,"a serie, codigo ",codes[i], " -- ERROR"))
              return(as.integer(codes[i]))
          },
            warning = function(w){
              
              message = paste0("?? ISSUE when code = ", codes[i], ". MESSAGE = ", w)
              write(message, conn, append = TRUE)
              
              print(paste0("TESTANDO ",i,"a serie, codigo ",codes[i], " -- ISSUE"))
              issues = c(codes[i],issues)
              
          })
    
    if(is.integer(code)){
      errors = c(code, errors)
    }
    
    perc = round((i/length(codes))*100, digits = 2)
    
    print(paste0("PROGRESSO: ", perc, "%"))
    print("-------")
  }
  
  # Check which codes are not being used
  max = max(codes)
  vacant = "## Codes that are not being used: "
  nums <- vector(mode = "character")
  j = 1
  
  for(i in 1:max){
    if(!(i %in% codes)){
      nums[j] = i
      j = j + 1
    }
  }
  
  # Log results
  vacant = paste0(vacant, paste(nums, collapse = ", "), " and all other positive integers greater than ", max)
  write(vacant, conn, append = TRUE)
  
  status = paste0(">> There were a total of ", length(errors), " errors.")
  write(status, conn, append = TRUE)
  
  status = paste0(">> There were a total of ", length(issues), " issues.")
  write(status, conn, append = TRUE)
  
  status = paste0(">> There are ", length(nums), " codes not being used.")
  write(status, conn, append = TRUE)
  
  # Log end of test
  footer <- paste("-- END OF TEST: BETS.get with ", database, "metadata @", Sys.time())
  write(footer, conn, append = TRUE)
  
  # Close file connection
  close(conn)
  
  # Log the codes of all the series with problems
  prob = cbind.fill(errors,issues,fill = "")
  names(prob) = c("errors","issues") 
  write.table(prob, paste0("tests//problematic_series-", database, ".csv"), sep = ",", row.names = F)
}