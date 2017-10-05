#' @title Search for a Brazilian Economic Time Series
#' 
#' @description Searches the BETS databases for a time series by its description, source, periodicity, code, data, unit of measurement and database name. 
#' 
#' @param description A \code{character}. A search string to look for matching series descriptions. Check the syntax rules under the 'Details' section for better performance. 
#' @param src A \code{character}. The source of the series. See the 'Details' section for a list of the available sources.
#' @param periodicity A \code{character}. The periodicity of the series. See the 'Details' section for a list of possible values.  
#' @param unit A \code{character}. The unit of measurement of the data. See the 'Details' section for a list of possible values.  
#' @param code An \code{integer}. The index of the series within the database. 
#' @param view A \code{boolean}. The default is \code{TRUE}. If set to \code{FALSE}, the output's \code{head} will be printed in your console as a \code{data.frame}.     
#' @param start A \code{date}. Starting date of the series.
#' @param lang A \code{character}. The search language. The default is "en" for english, but "pt" for portuguese is also possible.
#' 
#' @return A \code{list} that can be interpreted as a \code{data.frame}. The fields are described below.
#' 
#' \tabular{ll}{
#'  code \tab The code/index of the series within the database \cr
#'  description \tab The description of the series \cr
#'  periodicity \tab The periodicity of the series \cr
#'  start \tab Starting date of the series \cr
#'  source \tab The source of the series \cr
#'  unit \tab The unit of measurement of the data
#'}
#'
#' @details 
#' 
#' \itemize{
#' 
#' \item{ Syntax rules for the parameter \code{description}, the search string to look for matching series descriptions: 
#'    \enumerate{
#'        \item{To search for alternative words, separate them by white spaces. 
#'              Example: \code{description = "ipca core"} means that the series description must contain 'ipca' AND'core'
#'        }
#'        \item{To search for whole expressions, surround them with \code{' '}.
#'              Example: \code{description = "'core ipca' index"} means that the series description must contain 'core ipca' AND 'index'
#'        }
#'        \item{To exclude words from the search, insert a \code{~} before each of them.
#'              Example: \code{description = "ipca ~ core"} means that the series description must contain 'ipca' AND must NOT contain 'core'
#'        }
#'        \item{To exclude whole expressions from the search, surround them with code{' '} and insert a \code{~} before each of them.
#'              Example: \code{description = "~ 'ipca core' index"} means that the series description must contain 'index' AND must NOT contain 'core ipca'
#'        }
#'        \item{It is possible to search for multiple words or expressions and to negate multiple words or expressions, as long as the preceeding rules are observed. 
#'        }
#'        \item{The white space after the negation sign (\code{~}) is not required. But the white spaces AFTER expressions or words ARE required.
#'        }
#'      }
#'    }
#' 
#' \item{ Possible values for the parameter \code{src}:
#'    \tabular{ll}{
#'      IBGE \tab Brazilian Institute of Geography and Statistics \cr
#'      BCB \tab Central Bank of Brazil \cr
#'      FGV \tab Getulio Vargas Foundation \cr
#'      FGv-IBRE \tab Getulio Vargas Foundation - Brazilian Institute of Economics \cr
#'      BCB e FGV \tab Central Bank of Brazil and Getulio Vargas Foundation \cr
#'      BCB-Deban \tab Cetral Bank of Brazil - Department of Banking and Payments \cr
#'      BCB-Depin \tab Central Bank of Brazil - Department of International Reserves \cr
#'      BCB-Derin \tab Central Bank of Brazil - Department of International Affairs \cr
#'      BCB-Desig \tab Central Bank of Brazil - Department of Financial Monitoring \cr
#'      BCB-Secre \tab Central Bank of Brazil - Executive Secretariat \cr
#'      BCB-Demab \tab Central Bank of Brazil - Department of Open Market Operations \cr
#'      BCB-Denor \tab Central Bank of Brazil - Department of Financial System Regulation \cr
#'      BCB-Depec \tab Central Bank of Brazil - Department of Economics \cr
#'      Sisbacen \tab Central Bank of Brazil Information System \cr
#'      Abecip \tab Brazilian Association of Real Estate Loans and Savings Companies
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{periodicity}:
#'    \tabular{ll}{
#'      A \tab anual data    \cr
#'      M \tab monthly data  \cr
#'      Q \tab quaterly data \cr
#'      W \tab weekly data   \cr
#'      D \tab daily data 
#'    }
#' }
#' 
#' \item{ Possible values for the parameter \code{unit}:
#'    \tabular{ll}{
#'      R$ \tab brazilian reais \cr
#'      $ \tab US dolars        \cr
#'     \% \tab percentage 
#'    }
#' }
#'}
#' 
#'
#' 
#' @note 
#' 
#' This function uses specific version of the \code{\link[RMySQL]{RMySQL}} package. If it does not run correctly, try installing version 0.10.9 of the 
#' \code{\link[RMySQL]{RMySQL}} package using:
#' 
#' > remove.packages("RMySQL") # If necessary
#' > install.packages("devtools) # If necessary
#' > devtools::install_version("DBI", version = "0.5", repos = "http://cran.us.r-project.org")
#' > devtools::install_version("RMySQL", version = "0.10.9", repos = "http://cran.us.r-project.org")
#'
#' 
#' 
#' @examples 
#' #not run
#' #BETS.search(description="sales",view = FALSE)
#' # Output: BETS-package: Found 55 out of 12981 time series
#' 
#' #BETS.search(src="Denor", view = FALSE)
#' # Output: BETS-package: Found 1 out of 12981 time series
#' 
#' #BETS.search(periodicity="A", view = FALSE)
#' # Output: BETS-package: Found 2308 of 12981 time series
#' 
#' @references 
#' 
#' Central Bank of Brazil
#' 
#' @keywords search
#' 
#' @import RMySQL
#' @import DBI
#' @importFrom stringr str_split
#' @importFrom utils View
#' @export 

BETS.search = function(description="*",src,periodicity,unit,code,start,view=TRUE,lang="en"){
  
  conn = connection()
  
  if(lang == "en"){
    tb = "metadata_en"
  } else {
    tb = "metadata_pt"
  }
  
   if(description == "*" && missing(src) && missing(periodicity) && missing(unit) && missing(code)){
      query <- paste0("select * from ", tb)
     #  results = dbGetQuery(conn, query)
     #  results$description = iconv(results$description, from = "UTF-8")
     #  invisible(dbDisconnect(conn))
     # if(view==T){
     #  return(View(results,"Metadata"))
     #  }
     #  else{
     #    return(results)
     #  }
    } else {
  
      if(missing(description) && missing(src) && missing(periodicity) && missing(unit) && missing(code)){
        invisible(dbDisconnect(conn))
        return(msg("No search parameters. Please set the values of one or more parameters."))    
      }
      
      params = vector(mode = "character")
      
      if(!missing(description)){
        
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
        }
        else {
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
      }
      
      if(!missing(src)){
        params = c(params, paste0("source like " ,"\'%", src ,"%\'"))
      }
      
      if(!missing(periodicity)){
        params = c(params, paste0("periodicity like " ,"\'%", periodicity ,"%\'"))
      }  
      
      if(!missing(unit)){
        params = c(params, paste0("unit like " ,"\'%", unit ,"%\'"))
      }  
      
      if(!missing(code)){
        params = c(params, paste0("code like " ,"\'", code ,"\'"))
      }
      
      if(!missing(start)){
        params = c(params, paste0("start like " ,"\'", start ,"\'"))
      }
      
      query = paste0("select * from ", tb, " where")
      query = paste(query, params[1])
      
      if(length(params) != 1) {
        for(i in 2:length(params)){
          query = paste(query, "and", params[i])
        }
      }
    }
  
  results = dbGetQuery(conn, query)
  results$description = iconv(results$description, from = "UTF-8")
  results$unit = iconv(results$unit, from = "UTF-8")

  count = dbGetQuery(conn,paste0("select count(*) from ", tb))
  invisible(dbDisconnect(conn))
  
  if(nrow(results) > 0){
    msg(paste("Found", nrow(results),"out of", count ,"time series.",sep=" "))
    
 
    if(view==T){
      return(View(results,"Metadata"))
    }
    else{
      return(head(results))
    }
  }
  else{
    msg("No series found. Try using another combination of search terms.")
  }
}
