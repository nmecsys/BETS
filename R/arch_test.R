#' @title Perform an ARCH test
#' 
#' @description  Performs an ARCH test and show the results. Formerly, this function was part of FinTS, now an obsoleted package.
#' 
#' @param x A \code{ts} object. The time series
#' @param lags An \code{integer}. Maximum number of lags 
#' @param demean A \code{boolean}. Should the series be demeaned? 
#' @param alpha A \code{numeric} value. Significance level
#' 
#' @return A \code{list} with the results of the ARCH test
#' 
#' @importFrom stats embed lm pchisq resid
#' @export 
#' 
#' @author Spencer Graves \email{spencer.graves@prodsyse.com}, Talitha Speranza \email{talitha.speranza@fgv.br}

arch_test <- function (x, lags = 12, demean = FALSE, alpha = 0.5) {
    
    # Capture name of x for documentation in the output  
    xName <- deparse(substitute(x))
    x <- as.vector(x)
    if(demean) x <- scale(x, center = TRUE, scale = FALSE)

    lags <- lags + 1
    mat <- embed(x^2, lags)
    arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
    STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
    PARAMETER <- lags - 1
    PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
    METHOD <- "ARCH LM-test;  Null hypothesis:  no ARCH effects"
    if(PVAL >= alpha){
        htk = TRUE
    } else { htk = FALSE }
    
    result <- data.frame("statistic" = STATISTIC, "p.value" = PVAL, "htk" = htk)
    return(result)
}
