#' A function to extract BACEN series using their API
#' @param x Bacen series numbers. Either an integer or a numeric vector.
#' @param from A string specifying where the series shall start.
#' @param to A string specifying where the series shall end.
#' @param save A string specifying if data should be saved in csv or xlsx format. 
#' Defaults to not saving.
#' @keywords bacen
#' @author Fernando Teixeira \email{fernando.teixeira@fgv.br} and Jonatha Azevedo 
#' \email{jonatha.costa@fgv.br}
#' @importFrom httr GET content

get.series.bacen<- function(x, from = "", to = "",save = ""){

  
  if (missing(x)){
    stop("Need to specify at least one series.")
  }
  
  if (! is.numeric(x)){
    stop("Argument x must be numeric.")
  }
  
  if (from == ""){
    data_init = "01/01/1980"
  } else {data_init = from}
  
  if (to == ""){
    data_end = format(Sys.Date(), "%d/%m/%Y")
  } else {data_end = to}
  
  inputs = as.character(x)
  len = seq_along(inputs)
  serie = mapply(paste0, "serie_", inputs, USE.NAMES = FALSE)

 for(i in len){
    texto=tryCatch({
     k = paste0('http://api.bcb.gov.br/dados/serie/bcdata.sgs.',
                inputs[i], 
                '/dados?formato=csv&dataInicial=', data_init, '&dataFinal=',
                data_end)
     dados = httr::GET(k, timeout(10))
     aux = httr::content(dados,'raw')
     aux2= base::rawToChar(aux)
     
     DF <- data.frame(do.call(cbind, strsplit(aux2, "\r\n", fixed=TRUE)))
     names(DF) <- "mist"
     DF$mist   <- as.character(DF$mist)
     DF$mist   <- gsub(x = DF$mist,pattern = '"',replacement = "")
     DF$data   <- gsub(x = DF$mist,pattern = ";.*",replacement = "")
     DF$valor  <- gsub(x = DF$mist,pattern = ".*;",replacement = "")
     DF$valor  <- gsub(x = DF$valor,pattern = ",",replacement = ".")
     DF <- DF[-1,-1]
 })}
 assign(serie[i], DF)
 rm(DF, texto)
  

  lista = list()
  ls_df = ls()[grepl('data.frame', sapply(ls(), function(x) class(get(x))))]
  for ( obj in ls_df ) { lista[obj]=list(get(obj))}
  
  return(invisible(lista))
  
}
