#' @title bcbExpectT
#' 
#' @description Quarterly Market Expectations.
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
#' @return A data.frame.
#' @importFrom dplyr bind_rows
#' @export 
#'
#' 
#' @note The available indicators are: PIB Agropecuario, PIB Industrial, PIB Serviços e PIB Total.  
#' 
#' @examples 
#'  # bcbExpectT()
#' 
#' 
#' 



bcbExpectT <- function(indicator = 'PIB Total',limit = 100, variables = c("Media","Mediana","DesvioPadrao","CoeficienteVariacao","Minimo","Maximo","numeroRespondentes"), start, end ){
    
    indicator = str_replace_all(indicator," ","%20")
    
    variables = paste0(variables,collapse = ",")
    if(limit > 10000 | limit < 0)stop("You need provid a limit in between 0 and 10000!")
    # variaveis
    variaveis_a <- paste("filter=Indicador%20eq%20'",indicator,"'",sep="")
    variaveis_b <- paste("format=json&$top=",limit,sep="")
    variaveis_c <- paste0("Indicador,", "IndicadorDetalhe,", "Data,",
                         "DataReferencia,", variables, collapse = ",")
    
    if(missing(start) & missing(end)){
        timespan <- ""
    }else if(missing(start) & !missing(end)){
        timespan <- paste0("%20and%20Data%20gt%20'", start,"'")
    }else if(!missing(start) & !missing(end)){
        timespan <- paste0("%20and%20Data%20gt%20'", start, "'%20and%20", "Data%20lt%20'", end,"'")
    }else{
        timespan <- paste0("%20and%20Data%20lt%20'", end,"'")  
    }
    
    
    baseurl <- "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/"
    query_url <- paste(baseurl, "ExpectativasMercadoTrimestrais", "?$",variaveis_b,"&$",variaveis_a,timespan,
                       "&$select=",paste0(variaveis_c), sep = "", collapse = "")
    
    data = bind_rows(fromJSON(file =  query_url)$value)
    
    return(data)
}





















































