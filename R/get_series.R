get_series = function(code, from = "", to = "", data.frame = FALSE, frequency = NULL){
    
    date_to   = strsplit(to,split="-")
    to = paste0(date_to[[1]][3],"/",date_to[[1]][2],"/",date_to[[1]][1]) 
    
    date_from = strsplit(from,split="-")
    from = paste0(date_from[[1]][3],"/",date_from[[1]][2],"/",date_from[[1]][1])
    
    
    if(!grepl("ST_",code)){
        
        code = as.numeric(code)
        aux = get.series.bacen(code, from = from, to = to)[[1]]
        
        x <- get.series.bacen(code, from = from, to = to)[[1]]
        y <- get.series.bacen(code, from = from, to = to)[[1]]
        
        if(nrow(aux)<nrow(x)){
            aux <- x
        }else if(nrow(aux)<nrow(y)){
            aux <- y
        }
        
        if(nrow(aux) == 0){
            return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series is empty in the BACEN databases"))))
        }
        
        sch = suppressMessages(BETS.search(code = code, view = F))
        freq = NA
        
        if(class(sch) == "data.frame"){
            freq = trimws(sch[1,4])
        }
        
        no.meta = F
        
        if(is.na(freq)){
            msg(paste("There is no corresponding entry in the metadata table.\n\n", .WARN_SOFT), warn = TRUE)
            no.meta = T
            freq = ""
        }
        
        if(freq == "A"){
            freq = 1
        }
        else if(freq == "Q" || freq == "T"){
            freq = 4
        }
        else if(freq == "M"){
            freq = 12
        }
        else if(freq == "W" || freq == "S"){
            freq = 52
        }
        else if(freq == "D"){
            freq = 365
        }
        else {
            
            if(!no.meta){
                msg(paste("Malformed metadata. The value", freq, "is not valid for 'periodicity'\n\n", .WARN_SOFT), warn = TRUE)
            }
            
            if(is.null(frequency)){
                data.frame = T
            }
            else {
                freq = frequency 
            }
        }
        
    } else {
        
        freq = 365 
        conn = connection()
        
        aux = DBI::dbGetQuery(conn,paste0("select date, value from bets.IPC where code = '",code,"' order by date asc"))
        
        invisible(dbDisconnect(conn))
        
        if(nrow(aux) == 0){
            return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Series is empty in the FGV database"))))
        }
        
        #- Falta filtrar por datas!
    }
    
    aux1 = as.numeric(aux[,2])
    
    try = FALSE
    
    if(grepl("-",aux[1,1])){
        try = tryCatch({
            aux2 = as.Date(aux[,1], format = "%Y-%m-%d")},
            error = function(err) {
                return(TRUE)
            }
        )}else{
            try = tryCatch({
                aux2 = as.Date(aux[,1], format = "%d/%m/%Y")},
                error = function(err) {
                    return(TRUE)
                })
        }
    
    
    suppressWarnings(if(try==TRUE){
        
        return(invisible(msg(paste(.MSG_NOT_AVAILABLE,"Date formatting is inadequate."))))
    })
    
    
    if(freq != 365 && !data.frame){
        start = get.period(aux2[1],freq)
        ts <- ts(aux1, start = start, freq = freq)
    }
    else {
        ts = data.frame(date = aux2, value = aux1)
    }
    
    return(ts)
}
