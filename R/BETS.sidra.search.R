#' Search for Sidra Series
#'
#' Searches the Sidra databases for a series by its description or a given table descriptions.
#'
#' @param description A \code{character} argument. Function searches the Sidra metadata and prints results in a window.
#' @param code A numeric argument must be provided. The descriptions of the given table are returned.
#' @param view A \code{boolean}. The default is \code{TRUE}. If set to \code{FALSE}, the results are NOT going to be shown.
#' @param browse A \code{boolean}. If browse is set to \code{TRUE}, the description table opens in your browser for better visualization.
#' @examples
#' \dontrun{
#' BETS.sidra.search("pib")
#' BETS.sidra.search(1248)
#' }
#' @keywords sidra IBGE
#' @importFrom utils View 
#' @importFrom htmltools html_print
#' @importFrom stringr str_split
#' @import xml2 rvest stringr RMySQL DBI
#' @export 

BETS.sidra.search <- function(description = NULL, code, view = TRUE, browse = FALSE) {
    
    
    conn = connection()
    tb = "metadata_pt"
    # description = description
    
     # browser()
    
    if(is.null(description) & missing(code)){
        invisible(dbDisconnect(conn))
        return(msg("No search parameters. Please set the values of one or more parameters."))    
    }
    
    if(!is.null(description) & !missing(code)){
        invisible(dbDisconnect(conn))
        return(msg("You must input a description OR a code, not both."))    
    }
    
    
    if (!is.null(description)){ 
        if (is.numeric(description)){
            code = description; rm(description)
        }
    }
    # browser()
    
    if (!is.null(description) && missing(code)) {
       
        if(description == "*" &&  missing(code)){
            
            query <- paste0("select * from ", tb,  " where source = 'Sidra'")
            
        }    
            
        params = vector(mode = "character")
        
        ## Break description parameters
        and_params = vector(mode = "character")
        or_params = vector(mode = "character")
        
        # Workaround
        description = paste0(description, " ")
        
        # Do not match whole expressions
        exprs = regmatches(description,gregexpr("~ ?'(.*?)'",description))[[1]]
        
        if(length(exprs) != 0){
            for(i in 1:length(exprs)){
                description = gsub(exprs[i], "", description)
                exprs[i] = gsub("~", "", exprs[i])
                exprs[i] = gsub("'", "", exprs[i])
                exprs[i] = trimws(exprs[i])
                and_params = c(and_params, paste0("description not like " ,"\'%", exprs[i] ,"%\'"))
            }
        }
        
        # Match whole expressions
        exprs = regmatches(description,gregexpr("'(.*?)'",description))[[1]]
        
        if(length(exprs) != 0){
            for(i in 1:length(exprs)){
                description = gsub(exprs[i], "", description)
                exprs[i] = gsub("'", "", exprs[i])
                exprs[i] = trimws(exprs[i])
                or_params = c(or_params, paste0("description like " ,"\'%", exprs[i] ,"%\'"))
            }
        }
        
        # Do not match words
        words = regmatches(description,gregexpr("~ ?(.*?) ",description))[[1]]
        
        if(length(words) != 0){
            for(i in 1:length(words)){
                description = gsub(words[i], "", description)
                words[i] = gsub("~", "", words[i])
                words[i] = trimws(words[i])
                and_params = c(and_params, paste0("description not like " ,"\'%", words[i] ,"%\'"))
            }
        }
        
        # Match words
        words = str_split(description, " ")[[1]]
        words = words[words != ""]
        
        if(length(words) != 0){
            for(i in 1:length(words)){
                or_params = c(or_params, paste0("description like " ,"\'%", words[i] ,"%\'"))
            }
        }
        
        if(length(and_params) > length(or_params)){
            desc = and_params[1]
            and_params = and_params[-1]
        } else {
            desc = or_params[1]
            or_params = or_params[-1]
        }
        
        if(length(or_params) != 0){
            for(i in 1:length(or_params)){
                desc = paste(desc, "and", or_params[i])
            }
        }
        
        if(length(and_params) != 0){
            for(i in 1:length(and_params)){
                desc = paste(desc, "and", and_params[i])
            }
        }
        
        params = c(params, desc)
        
        
        query = paste0("select * from ", tb, " where source = 'Sidra' and")
        query = paste(query, params[1])
        if(length(params) != 1) {
            for(i in 2:length(params)){
                query = paste(query, "and", params[i])
            }
        }
        
        
        results = dbGetQuery(conn, query)
        results$description = iconv(results$description, from = "UTF-8")
        results$unit = iconv(results$unit, from = "UTF-8")
        results$code = str_replace(results$code, "Sidra_", "")
        
        count = dbGetQuery(conn,paste0("select count(source) from ", tb, " where source = 'Sidra'"))
        invisible(dbDisconnect(conn))
        
        
        if(nrow(results) > 0){
            msg(paste("Found", nrow(results),"out of",  count,"series.",sep=" "))
            msg("If you have found the series you want, you can input its number in this function to get the metadata.")
            
            
            if(view==T){
                return(View(results,"Metadata"))
            }
            else{
                return(results)
            }
        }
        else{
            msg("No series found. Try using another combination of search terms.")
        }
        
        
    } 
    
    
    if(!missing(code)){
        
        tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
        tabela <- rvest::html_text(tabela)
        
        
        
        
        
        d = strsplit(tabela, split = "\r\n")
        d = trimws(d[[1]])
        d2 = c()
        
        for ( i in seq_along(d)){
            
            if(d[i] != ""){
                
                d2 = c(d2,d[i])
                
            }
            
        }
        
        d3 = paste(d2[10:length(d2)], collapse = "\n")
        
        
        
        
        
        if(browse != FALSE){
            
            shell.exec(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
        } else{
            
            # utils::View(d3)
            return(writeLines(d3))
            
        }
        
        
    }
    
    
} 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # if(is.character(description) & missing(code)){
    # 
    #     description <- stringr::str_replace_all(description, " ", "%20")
    # 
    #     tabela <- xml2::read_html(paste0("https://sidra.ibge.gov.br/Busca?q=", description))
    # 
    #     tabela <- rvest::html_nodes(tabela,".busca-link-tabela")
    #     tabela <- rvest::html_text(tabela)
    # 
    # 
    #     tabela <- stringr::str_replace(tabela, "Tabela ", "")
    #     tabela <- stringr::str_split(tabela, "-", n = 2)
    #     tabela <- matrix(trimws(unlist(tabela)), ncol = 2, byrow = TRUE)
    # 
    #     colnames(tabela) <- c("code", "description")
    #     msg(paste("Found", nrow(tabela), "results."))
    #     utils::View(tabela)
    # 
    # 
    #     # return(writeLines(tabela))
    # } else if (is.numeric(code)){
    # 
    # 
    #     tabela <- xml2::read_html(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
    #     tabela <- rvest::html_text(tabela)
    # 
    # 
    # 
    # 
    # 
    #     d = strsplit(tabela, split = "\r\n")
    #     d = trimws(d[[1]])
    #     d2 = c()
    # 
    #     for ( i in seq_along(d)){
    # 
    #         if(d[i] != ""){
    # 
    #             d2 = c(d2,d[i])
    # 
    #         }
    # 
    #     }
    # 
    #     d3 = paste(d2[10:length(d2)], collapse = "\n")
    # 
    # 
    # 
    # 
    # 
    #     if(browse != FALSE){
    # 
    #     shell.exec(paste0("http://api.sidra.ibge.gov.br/desctabapi.aspx?c=", code))
    #     } else{
    # 
    #         # utils::View(d3)
    #         return(writeLines(d3))
    # 
    #     }
    # 
    # 
    # } else{ stop("Either 'description' or 'code' must be provided as input.") }

# }







# library(htmltools); View(html_print(pre(paste0(capture.output(print(mtcars)), collapse="\n"))))
