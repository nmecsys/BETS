#' @title Display a list of sources available at BETS package
#'
#' @description Display a list of sources available at BETS package in console. The numbers of
#' sources will increase wiht new versions of the package.
#'
#'
#' @export 


BETSsources <- function() {
    return(message("The sources available at BETS, to date, are: \n >  Banco Central, IBGE, Sidra, FGV"))
}


#' @title Display a list of contents available at BETS expectations functions
#'
#' @description Display a data frame with all informations about the expectation indicators available in BCB expectation sector
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr 
#' @export 


BETSexpect.info <- function() {
    data.info = file.path(system.file(package="BETS"), "/expectativas_info.csv")
    data.info = as_tibble(data.info)
    return(data.info)
}
