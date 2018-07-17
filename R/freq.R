# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#'
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#' @export
#' @param ... A number, factor or text
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' \dontrun{
#' freq(c(3,1,2,2,5))
#' }
#'
#'

freq <- function(...) {
  var <- getvar(...)
  count <- table(var, exclude = NULL)
  prop <- round(prop.table(count)*100, digits = 2)
  cum <- cumsum(prop)
  result <- cbind(count,
                  prop,
                  cum)
  colnames(result) <- c("Freq", "%" , "cum%")
  result = rbind(result,Totals = colSums(result))
  result[nrow(result),ncol(result)] <- 100
  print(result)
  invisible(result)
}

printtable <- function(table)  {


}




