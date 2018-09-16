# epifield documentation using roxygen2
#' @title
#' logreg
#' @description
#' \code{logreg} Do a logistic regression using submitted model .
#' #'
#'
#' @name logreg
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @export
#'
#' @param outvar As numbers, factors or text. Represent the outcome
#' @param expvar As character : model to be tested as var1 + var2 + var3
#' @return An array containing  resulting summary
#' #' @examples
#' logreg()
#'
logreg <- function(outvar,expvar) {
  r <- as.list(match.call())
  outval <- getvar(r$outvar)
  outvar <- getvarname()

  m <- mode(substitute(expvar))
  if (! m == "character" ) {
     expvarlist <- deparse(substitute(expvar) )
  } else {expvarlist <- expvar}

  df <- getdf()
  form <- paste0("glm(", outvar," ~ ",expvarlist, ", data = df, family = binomial(logit))")

  reg <- eval(parse(text=form) )

  # reg <- glm(sport ~ , data = df, family = binomial(logit))
  summary(reg)

}
