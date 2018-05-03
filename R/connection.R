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

    conn = tryCatch({
        dbConnect(MySQL(),db=credentials$bd,user=credentials$login,password=as.character(credentials$password),host=credentials$host,port=credentials$port)
    }, error = function(e){
        return(NULL)
    })
    
    return(conn)
}
