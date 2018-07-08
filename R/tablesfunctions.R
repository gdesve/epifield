# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#'
#' @param varx A number, factor or text
#' @return An array containing  values of \code{varx}   \code{}
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'


freq <- function(varx) {
  distrib <- table(varx)
  prop <- round(distrib / sum(distrib), digits = 2)
  print(distrib)
  prop
}
