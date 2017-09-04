#' Search for Sidra Series
#'
#' Searches the Sidra databases for a series by its description or a given table descriptions.
#'
#' @param x Either a character or a numeric. If character, function searches the Sidra metadata. If a numeric argument is provided the descriptions of the given table are seached .
#' @param len A \code{}.
#' @param nova_req A \code{}.
#' @param from A \code{}.
#' @param to A \code{}.
#' @param inputs A \code{}.
#' @param territory A \code{}.
#' @param variable A \code{}.
#' @param header A \code{}.
#' @param sections A \code{}.
#' 
#' 
#' @import xml2 rvest stringr
#' @importFrom lubridate month


sidra.aux <- function(x, len, nova_req, from, to, inputs, territory, variable, header, sections) { 

    tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", x))
    tabela <- rvest::html_text(tabela)

    
    d = strsplit(tabela, split = "/P/")
    d = strsplit(d[[1]][2], split = ":")
    d = trimws(d[[1]][1])
    
    
    
    if (stringr::str_count(d, "Ano") == 1){
        
        
        minus = to - from
        minus = floor(minus/ 3)
        
        for(i in len){
            
            tabela = data.frame()
            header2 = NULL
            
            for(j in seq(from,to, by=minus)){
            
                tabela1=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                        "t/", inputs[i], "/", territory, "/", "p/", 
                                        j, "-", (j+minus-1),  
                                        "/v/", variable[i], "/f/", "u", "/h/", header,
                                        sections[[i]]),
                                        ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
                
                
                t1 = paste("tabela", x, sep="_")
                tabela1 = rjson::fromJSON(tabela1)
                tabela1 = as.data.frame(do.call("rbind", tabela1))
                if(is.null(header2)){header2 = tabela1[1,]}
                tabela = as.data.frame(do.call("rbind", list(tabela, tabela1[2:nrow(tabela1),])))
            } 
        }
        
        colnames(tabela) <- unlist(header2)
        
        
        
    } else if (stringr::str_count(d, "M\u00EAs") == 1){
        
       
        
        # month(to2) <- month(to2) - month(from2)
    
        for(i in len){
            
            tabela = data.frame()
            header2 = NULL
            
            
            from2 = paste0(substr(from,1,4), "-", substr(from,5,6), "-01")
            to2 = paste0(substr(to,1,4), "-", substr(to,5,6), "-01")
            from2 = as.Date(from2)
            to2 = as.Date(to2)
            dif_mes = as.numeric(floor((to2 - from2)/365*12/nova_req))
            lubridate::month(from2) = dif_mes + lubridate::month(from2) 
            
            init = paste0(substr(from,1,4),substr(from,5,6))
            fin = paste0(substr(from2,1,4),substr(from2,6,7)) 
            
            
            for(j in 1:nova_req){        
        
                tabela1=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/", 
                                    init, "-", fin,  
                                    "/v/", variable[i], "/f/", "u", "/h/", header,
                                    sections[[i]]),
                             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
                
                
                
                init = paste0(sum(as.numeric(substr(from2,1,4)),1),substr(from2,6,7))
                lubridate::month(from2) = dif_mes + lubridate::month(from2)
                fin = paste0(substr(from2,1,4),substr(from2,6,7)) 
                
                t1 = paste("tabela", x, sep="_")
                tabela1 = rjson::fromJSON(tabela1)
                tabela1 = as.data.frame(do.call("rbind", tabela1))
                if(is.null(header2)){header2 = tabela1[1,]} 
                tabela = as.data.frame(do.call("rbind", list(tabela, tabela1[2:nrow(tabela1),])))
                
                
            }
            
        }
        
        colnames(tabela) <- unlist(header2)
        
        
        
    } else if(stringr::str_count(d, "Trimestre") == 1){
        
        
        for(i in len){


            tabela = data.frame()
            header2 = NULL
            
            
            from2 = paste0(substr(from,1,4), "-", substr(from,5,6), "-01")
            to2 = paste0(substr(to,1,4), "-", substr(to,5,6), "-01")
            from2 = as.yearqtr(from2)
            to2 = as.yearqtr(to2)
            dif_mes = as.numeric(floor((to2 - from2)/nova_req))
            from2 = dif_mes + from2 
            
            init = paste0(substr(from,1,4),substr(from,5,6))
            fin = paste0(substr(from2,1,4),"0",substr(from2,7,7)) 


            

            for(j in 1:nova_req){

                tabela1=RCurl::getURL(paste0("http://api.sidra.ibge.gov.br/values/",
                                    "t/", inputs[i], "/", territory, "/", "p/",
                                    from, "-", to,
                                    "/v/", variable[i], "/f/", "u", "/h/", header,
                                    sections[[i]]),
                             ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
                
                
                
                
                init = paste0(sum(as.numeric(substr(from2,1,4)),1),"0",substr(from2,7,7))
                from2 = dif_mes + from2
                fin = paste0(substr(from2,1,4),"0",substr(from2,7,7)) 
                
                t1 = paste("tabela", x, sep="_")
                tabela1 = rjson::fromJSON(tabela1)
                tabela1 = as.data.frame(do.call("rbind", tabela1))
                if(is.null(header2)){header2 = tabela1[1,]} 
                tabela = as.data.frame(do.call("rbind", list(tabela, tabela1[2:nrow(tabela1),])))
                
                


            }

        }
        
        
        colnames(tabela) <- unlist(header2)
        
        
    }

    rm(tabela1)
    
    
    
    
    # id2 = which(colnames(tabela)== "D4N")
    id = which(colnames(tabela)=="V" | colnames(tabela)=="Valor")
    id3 = which(colnames(tabela) == "M\u00EAs" | colnames(tabela) == "Ano" |
                    colnames(tabela) == "Trimestre")
    


    if ( colnames(tabela[id3]) == "M\u00EAs" & length(tabela[[id3]]) > 1){
        
        tabela$mes <- sapply(tabela["M\u00EAs"], 
                             FUN = function(x){substr(x,1,(nchar(x)-5))}) 
        tabela$ano <- sapply(tabela["M\u00EAs"], 
                             FUN = function(x){substr(x,(nchar(x)-3), nchar(x))}) 
        
        
        tabela$mes[tabela$mes == "janeiro"] <- "01"
        tabela$mes[tabela$mes == "fevereiro"] <- "02"
        tabela$mes[tabela$mes == "mar\u00E7o"] <- "03"
        tabela$mes[tabela$mes == "abril"] <- "04"
        tabela$mes[tabela$mes == "maio"] <- "05"
        tabela$mes[tabela$mes == "junho"] <- "06"
        tabela$mes[tabela$mes == "julho"] <- "07"
        tabela$mes[tabela$mes == "agosto"] <- "08"
        tabela$mes[tabela$mes == "setembro"] <- "09"
        tabela$mes[tabela$mes == "outubro"] <- "10"
        tabela$mes[tabela$mes == "novembro"] <- "11"
        tabela$mes[tabela$mes == "dezembro"] <- "12"
        
        tabela$mes_ano <- base::paste0(tabela$ano, "-",tabela$mes, "-01")
        tabela$mes_ano <- base::as.Date(tabela$mes_ano)
        tabela["M\u00EAs"] <- tabela$mes_ano
        tabela <- tabela[,1:(length(tabela)-3)]
        colnames(tabela)[id3] <- "Data"
        
    }
    
    if(colnames(tabela[id3]) == "Ano" & length(tabela[[id3]]) > 1){ 
        tabela$Ano <- base::paste0(tabela$Ano, "-01-01")
        tabela$Ano <- base::as.Date(tabela$Ano)
        colnames(tabela)[id3] <- "Data"
        
    }
    
    if(colnames(tabela[id3]) == "Trimestre" & length(tabela[[id3]]) > 1){
        
        tabela$trimestre <- sapply(tabela["Trimestre"], 
                                   FUN = function(x){substr(x,1,1)}) 
        tabela$ano <- sapply(tabela["Trimestre"], 
                             FUN = function(x){substr(x,(nchar(x)-3), nchar(x))}) 
        
        
        tabela$tri_ano <- base::paste0(tabela$ano, "-0",tabela$trimestre)
        tabela$tri_ano <- zoo::as.yearqtr(tabela$tri_ano)
        tabela["Trimestre"] <- tabela$tri_ano
        tabela <- tabela[,1:(length(tabela)-3)]
        colnames(tabela)[id3] <- "Data"    
    }
    
    #Transformando a coluna V em valor
    
    valor = NULL
    id = which(colnames(tabela)=="V" | colnames(tabela)=="Valor")
    tabela[,id] = suppressWarnings(ifelse(unlist(tabela[,id])!="..", 
                                          as.numeric(unlist(tabela[,id])),NA))
    
    
    return(tabela)
}
