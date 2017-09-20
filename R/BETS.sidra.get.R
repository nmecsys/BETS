#' A function to extract Sidra series using their API
#' 
#' The different parameters define the table and its dimensions (periods, variables, territorial units and classification) to be consulted. The parameters that define the sections may vary from table to table. Henceforth, the Sidra function ranges between 5 mandatory arguments to 7. You can only choose one variable per series per request, but multiple sections within the variable.
#' @param x Sidra series number.
#' @param from A string or character vector specifying where the series shall start
#' @param to A string or character vector specifying where the series shall end
#' @param territory Specifies the desired territorial levels.
#' @param variable An integer describing what variable characteristics are to be returned. 
#' Defaults to all available.
#' @param cl A vector containing the classification codes in a vector.
#' @param sections A vector or a list of vectors if there are two or more classification
#' codes containing the desired tables from the classification.
#' @keywords sidra
#' @export
#' @import rjson zoo
#' @importFrom httr GET content
#' @examples
#' \dontrun{sidra=BETS.sidra.get(x = c(1612), from = 1990, to = 2015, territory = "brazil", variable =109)
#' sidra=BETS.sidra.get(x = c(3653), from = c("200201"), 
#' to = c("201703"), territory = "brazil", 
#' variable = 3135, sections = c(129316,129330), cl = 544)
#' sidra=BETS.sidra.get(x = c(3653), from = c("200201"), 
#' to = c("201512"), territory = "brazil",  variable = 3135, 
#' sections = "all", cl = 544)
#' sidra=BETS.sidra.get(x = c(1618), from = c("201703"), to = c("201703"), 
#' territory = "brazil",
#' variable = 109, sections=list(c(39427), c(39437,39441)), cl = c(49, 48))
#' trim - x = 1620; from = 199001; to = 201701;  territory = "brazil"; 
#' sections=list(c(90687)); cl =c(11255); variable = 583
#' sidra = BETS.sidra.get(x = 1620, from = 199001, to = 201701,  
#' territory = "brazil",
#' sections=list(c(90687)), cl =c(11255), variable = 583)}


# x = c(5932); from = 199601; to = 201701; territory = "brazil"; variable =6564; cl = c(11255); sections = c("all")
# x = c(1612); from = 1990; to = 2015; territory = "n6/all"; variable =109; cl = NULL; sections = NULL


BETS.sidra.get <- function(x, from, to, territory = c(n1 = "brazil", n2 = "region", n3 = "state", 
                                                      n6 = "city", n8 = "mesoregion", n9 = "microregion", 
                                                      n129 = "citizenship", n132 = "semiarid", 
                                                      n133 = "semiaridUF"), 
                           variable, cl = NULL, sections = NULL){ 
    
    
     # browser()
    if (missing(x)){
        stop("Need to specify one serie.")
    } else if (length(x) > 1){stop("Argument x must have length one.")}

    x = as.character(x)
    
    
    if (missing(from)){
        stop("Need to specify 'from' parameter.")}
    if (missing(to)){
        stop("Need to specify 'to' parameter.")} 


    if (missing(variable)){
        stop("Need to specify one variable.")
    } else if (length(x) > 1){stop("Argument x must have length one.")}    
        
        
        
    # Território
    territory <- base::match.arg(territory)
    territory <- base::switch(territory,
                              brazil = "n1/all", 
                              region = "n2/all", 
                              state = "n3/all",
                              city = "n6/all",
                              mesoregion = "n8/all",
                              microregion = "n9/all",
                              citizenship = "n129/all",
                              semiarid = "n132/all",
                              semiaridUF = "n133/all")
    
    header = "y"
    
    
    if (length(cl) > 1){
        
        t1=list()
        
        for (i in 1:length(cl)){
            
            t1[i] =  paste0("/c", paste0(cl[i], collapse = ","))
            
        }
        
        t2 = NULL

        for (i in 1: length(sections)){
            
            
            t2[i] = paste0(t1[i], "/", paste0(sections[[i]], collapse = ","))
            
            
        }  
        
        sections = paste0(t2, collapse = "")
        
        
    }
    
    
    
    
    if (! is.null(sections) & length(cl) == 1){
        
        sections = unlist(sections)
        sections = c(cl,sections)
        sections = list(sections)
        
        for (i in seq_along(sections)){
            
            sections[i] = paste0("/c", sections[[i]][1], "/", 
                                 paste0(sections[[i]][2:length(sections[[i]])], 
                                        collapse = ","))
        }
    }
    sections = c(sections, rep('', (length(x)+1)-length(sections)))
    
    
    
    inputs = as.character(x)
    len = seq_along(inputs)
    serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)
    
    for (i in len){
        tabela=httr::GET(paste0("http://api.sidra.ibge.gov.br/values/",
                                "t/", inputs[i], "/", territory, "/", "p/", 
                                from, "-", to,  
                                "/v/", variable[i], "/f/", "u", "/h/", header,
                                sections[[i]]))
        
        tabela = base::rawToChar(httr::content(tabela,'raw'))
        

        if (strsplit(tabela, " ")[[1]][1] == "Par\uE2metro") {
            
            stop("The parameters 'from', 'to' or both are misspecified")
            
        } else if (strsplit(tabela, " ")[[1]][1] == "Tabela" & 
                   strsplit(tabela, " ")[[1]][3] == "Tabela"){
            
            param = strsplit(tabela, " ")[[1]][2]
            param = substr(param, 1, nchar(param)-1)
            warning(sprintf("The table %s does not contain public data", param))
            
        } else if (strsplit(tabela, " ")[[1]][1] == "Quantidade"){
            
            
            nova_req <- ceiling(as.numeric(strsplit(tabela, " ")[[1]][5]) / 
                                as.numeric(strsplit(tabela, " ")[[1]][9])) + 1
            
            
            tabela <- sidra.aux(x, len, nova_req, from, to, inputs, territory, variable, header, sections)
            
            
            
            
            
            
           
            
            
        } else{
            t1 = paste("tabela", x, sep="_")
            tabela = rjson::fromJSON(tabela)
            tabela = as.data.frame(do.call("rbind", tabela))

            tabela2 = tabela
            colnames(tabela) = unlist(tabela[1,])
            tabela = tabela[-1,]
            id = which(colnames(tabela)=="V" | colnames(tabela)=="Valor")
            
            
            id2 = which(colnames(tabela2)== "D4N")
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
            rm(tabela2)
            
        }
        
        assign(serie[i],tabela)
        rm(tabela)
    }

    lista = list()
    ls_df = ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]
    for ( obj in ls_df ) { lista[obj]=list(get(obj)) }
    
    return(lista)
    
} 
