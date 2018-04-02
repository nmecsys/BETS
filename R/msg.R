#' @title Format and show a console message.
#' 
#' @description Customizes a message and shows it in the console.
#' 
#' @param ... Arguments to be passed to \code{\link[base]{message}}
#' @param skip_before A \code{boolean}. Indicates if a line should be skipped before the message. 
#' @param skip_after A \code{boolean}. Indicates if a line should be skipped after the message. 
#' @param warn A \code{boolean}. Indicates whether a warning should be thrown.
#' 
#' @return None
#' 
#' @importFrom stringr str_c
#' @author Talitha Speranza \email{talitha.speranza@fgv.br}, Jonatha Azevedo \email{jonatha.azevedo@fgv.br}


msg <- function(..., skip_before=TRUE, skip_after=FALSE, warn = FALSE) {
  
  m <- str_c("BETS-package: ", ...)
  
  if(skip_before) k <- paste0("\n", m)
  if(skip_after) k <- paste0(m, "\n")
  Encoding(k) <- "UTF-8"
  
  if(warn){
    warning(k, call. = FALSE)
  }
  else {
    message(k)
  }
  
  invisible(m)
}