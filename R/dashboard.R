#' @title  Create a BETS custom dashboard
#' 
#' @description  Generate thematic dashboards using a selection of BETS time series and charts. For now, themes and charts are pre-defined.
#' 
#' @param type A \code{character}. The theme of the dashboard. The only three options, for the time being, is 'business_cycle', 'macro_situation' and 'custom'. Custom dashboards can be rendered with any given set of charts.
#' @param saveas A \code{character}. A path and a name for the dashboard file (a .pdf file). If this parameter is not provided, the dashboard will be saved inside the 'dashboards' folder, under the BETS installation directory.
#' @param charts A \code{character} and/or \code{ts} object list. The charts to be added to a custom dashboard. Up to 16 charts are allowed, including pre-defined charts, identified by their codes (see \code{\link{chart}}). This will only work if parameter 'type' is set to 'custom'. 
#' @param parameters A \code{list}. A list of parameters. See the 'Details' section for a description of these parameters for each type of dashboard.
#' 
#' @details
#' 
#' \bold{Macro Situation and Custom Dashboard Parameters} 
#' 
#' \tabular{ll}{
#' \code{text}\tab The text to be printed in the dashboard. Separate paragraphs with two backslashes 'n' and pages with '##'. There are no other syntax rules.\cr
#' \code{author}\tab The author's name.\cr
#' \code{email}\tab The author's email.\cr
#' \code{url}\tab The author's webpage.\cr
#' \code{logo}\tab The author's business logo.\cr
#' }
#' 
#' 
#' \bold{Additional Custom Dashboard Parameters}
#' 
#' \tabular{ll}{
#' \code{style} \tab A \code{character}. The style of the charts. As in \code{chart}, can be either \code{'plotly'} or \code{'normal'}.\cr
#' \code{charts.opts} \tab A \code{list} of parameters lists, one for each chart. Parameters are specified in \code{\link{chart}} \cr
#' }
#' 
#' @return A .pdf file (the dashboard)
#' 
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}
#' 
#' @examples 
#' 
#' # dashboard()
#' # dashboard(saveas = "survey.pdf")
#' # dashboard(type = "macro_situation")
#' 
#' @export
#' @import rmarkdown


dashboard = function(type = "business_cycle", charts = "all", saveas = NA, parameters = NULL){
  
  rmd = paste0(type, "_dashboard.Rmd")
  file = system.file(package="BETS", rmd)
  
  if(!is.null(parameters$text)){
    if(is.null(parameters$author)){
      msg("You've provided an analysis to be printed together with the dashboard, but the argument 'author' is missing. Dashboard will not be printed.")
    }
  }
  
  if(type == "custom"){
      
      if(is.null(parameters)){
          parameters = list()
      } 
      
      parameters$charts = charts
  }
  
  if(is.null(parameters)){
    rmarkdown::render(file)
  }
  else {
    rmarkdown::render(file, params = parameters)
  }
  
  if(is.na(saveas)){
    dir = paste0(system.file(package="BETS"),"//dashboards")
    dir.create(dir) 
    saveas = paste0(dir, "//", type, "_dashboard.pdf")
  }
  
  file = gsub(".Rmd", ".pdf", file)
 
  file.copy(file, saveas, overwrite = T)
  file.remove(file)
  system2("open", saveas)
}