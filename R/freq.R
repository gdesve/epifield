# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#' #'
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#' @export
#' @param ... As numbers, factors or text.
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'

freq <- function(...) {
  arg <- deparse(substitute(...))

  var <- getvar(arg)
  if (! is.null(var)) {
    count <- table(var, useNA="no")
    tot <- length(var)
    prop <- round(prop.table(count)*100, digits = 2)
    cum <- cumsum(prop)
    result <- cbind(count,
                    prop,
                    cum)
    mis  <- sum(is.na(var))
    vname <- getvar()
    colnames(result) <- c("Freq", "%" , "cum%")
    result <- rbind(result,Totals = colSums(result))
    deci <- c(0,2,2)
    result[nrow(result),ncol(result)] <- 100
    title <- paste("Frequency distribution of ")
    outputtable(result,deci,tot=FALSE,title,subtitle=vname)
    cat("Missings :",mis," (",round(mis/tot*100, digits = 2),"%)\n")
    # print(result)
    invisible(result)
  }
}


# epifield documentation for RData using roxygen2
#' @title
#' Cross tabulation ( 2by2 table).
#' @description
#' \code{epitable} Display a cross tabulation of two binary variables optionnaly with
#'  row or col percentages.
#'
#'
#' @name epitable
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{freq}} for frequency distributions
#' @export
#' @param exp  "Exposure" as numbers, factors or text.
#' @param out  "Outcome" as numbers, factors or text
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' \dontrun{
#' epitable(c(3,1,2,2,5))
#' }
#'
#'
epitable <- function(exp,out)  {

   expdata <- getvar(deparse(exp))
   outdata <- getvar(out)
   r <- table(expdata,outdata)
   m <- 1

}
