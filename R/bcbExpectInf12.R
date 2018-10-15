#' @title bcbExpectInf12
#' 
#' @description Market expectations for inflation in the next 12 months
#' 
#' 
#' @param variables Possible options: "Media", "Mediana", "DesvioPadrao",
#' "CoeficienteVariacao", "Minimo", "Maximo".
#' @param start Initial date at which the data was projected, in ISO format.
#' @param end Final date at which the data was projected, in ISO format.
#' @param indicator  A string. Available indicator.
#' @param limit A integer. A limint of data in request, top is 10000.
#'
#'
#' @import rjson
#' @importFrom  dplyr bind_rows
#' @return A data.frame.
#' @export 
#'
#' 
#' @note The available indicators are: IGP-DI, IGP-M, INPC, IPA-DI, IPA-M, IPCA, IPCA-15, IPC-FIPE.  
#' 
#' @examples 
#'  # bcbExpectInf12()
#' 
#' 
#' 



bcbExpectInf12 <- function(indicator = 'IPC-FIPE',limit = 100, variables = c("Media","Mediana","DesvioPadrao","CoeficienteVariacao","Minimo","Maximo","numeroRespondentes","baseCalculo"), start, end ){
    
    
    indicator = str_replace_all(indicator," ","%20")
    variables = paste0(variables,collapse = ",")
    if(limit > 10000 | limit < 0)stop("You need provid a limit in between 0 and 10000!")
    # variaveis
    variaveis_a <- paste("filter=Indicador%20eq%20'",indicator,"'",sep="")
    variaveis_b <- paste("format=json&$top=",limit,sep="")
    variaveis_c <- paste0("Indicador,", "Data,","Suavizada,",
                          variables, collapse = ",")
    
    if(missing(start) & missing(end)){
        timespan <- ""
    }else if(!missing(start) & missing(end)){
        timespan <- paste0("%20and%20Data%20gt%20'", start,"'")
    }else if(!missing(start) & !missing(end)){
        timespan <- paste0("%20and%20Data%20gt%20'", start, "'%20and%20", "Data%20lt%20'", end,"'")
    }else{
        timespan <- paste0("%20and%20Data%20lt%20'", end,"'")  
    }
    
    
    baseurl <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/"
    query_url <- paste(baseurl, "ExpectativasMercadoInflacao12Meses", "?$",variaveis_b,"&$",variaveis_a,timespan,
                       "&$select=",variaveis_c, sep = "", collapse = "")
    
    
    data = bind_rows(fromJSON(file =  query_url)$value)
    
    return(data)
}





















































