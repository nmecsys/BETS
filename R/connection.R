#' @title Connection with the server
#' @description  Make the connection with the server
#' @import DBI RMySQL digest
#' @importFrom utils  install.packages remove.packages

connection = function(){
    
        key <- readRDS(paste0(system.file(package="BETS"),"/data/key.rds"))
        dat <- readBin(paste0(system.file(package="BETS"),"/credentials.txt"),"raw",n=1000)
        aes <- AES(key,mode="ECB")
        raw <- aes$decrypt(dat, raw=TRUE)
        txt <- rawToChar(raw[raw>0])
        credentials <- read.csv(text=txt, stringsAsFactors = F)
        rm(key)
    
    
    
    tryCatch({
        conn = dbConnect(MySQL(),db=credentials$bd,user=credentials$login,password=as.character(credentials$password),host=credentials$host,port=credentials$port)
        return(conn)
    },
    error = function(e){
        message("Sorry, but it was not possible to connect to the server.")
        x <- readline("Do you want to install a development version of RMySQL and DBI package packages?\n(This may solve the connection problem)[Y/n]")
        if(x %in% c("y","Y","yes","Yes","YES")){
            tryCatch({
                if(requireNamespace("devtools")){
                
                    devtools::install_github("rstats-db/DBI")
                    devtools::install_github("rstats-db/RMySQL")
                    conn = dbConnect(MySQL(),db="bets",user="BETS_user",password="123456",host="200.18.49.107",port=3306)
                    return(conn)
                }else{
                    install.packages("devtools")
                    devtools::install_github("rstats-db/DBI")
                    devtools::install_github("rstats-db/RMySQL")
                    conn = dbConnect(MySQL(),db="bets",user="BETS_user",password="123456",host="200.18.49.107",port=3306)
                    return(conn)
                }
            },
            error = function(e){
                x<- readline("Do you want to install a previous version of DBI and RMySQL packages?")
                if(x %in% c("y","Y","yes","Yes","YES")){
                    remove.packages(c("RMySQL","DBI"))
                    install.packages("devtools")
                    devtools::install_version("DBI", version = "0.5", repos = "http://cran.us.r-project.org")
                    devtools::install_version("RMySQL", version = "0.10.9", repos = "http://cran.us.r-project.org") 
                    conn = dbConnect(MySQL(),db="bets",user="BETS_user",password="123456",host="200.18.49.107",port=3306)
                    return(conn)
                }else{
                    conn = dbConnect(MySQL(),db="bets",user="BETS_user",password="123456",host="200.18.49.107",port=3306)
                    return(conn)
                    #stop("Connection fail!")
                }
            })
        }else{
                stop("Connection failed!")
            } 
       })
}
