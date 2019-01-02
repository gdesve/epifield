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
#' @importFrom stats coef confint
#' @export
#'
#' @param outvar As numbers, factors or text. Represent the outcome
#' @param expvar As character : model to be tested as var1 + var2 + var3
#' @param quietly If TRUE, supress direct output
#' @param full For full output
#' @return An array containing  resulting summary
#' #' @examples
#' logreg()
#'
logreg <- function(outvar,expvar, full= FALSE,quietly=FALSE) {
  r <- as.list(match.call())
  outval <- getvar(r$outvar)
  outvar <- getvarname()

  m <- mode(substitute(expvar))
  if (! m == "character" ) {
     expvarlist <- deparse(substitute(expvar) )
  } else {expvarlist <- expvar}

  df <- getlastdf()
  form <- paste0("glm(", outvar," ~ ",expvarlist, ", data = df, family = binomial(logit))")
  if (get_option("show_Rcode")) {
    catret(form)
  }
  reg <- eval(parse(text=form) )

  # reg <- glm(sport ~ , data = df, family = binomial(logit))
  if (!quietly) {
    if (full) {
      print(summary(reg))
    } else {
      print(reg$call)
    }

    r <- exp(cbind(coef(reg), confint(reg)))
    catret("")
    catret("Odds ratio with CI")
    catret("")
    dimnames(r) <- list(dimnames(r)[[1]],c("Odds ratio","LCI","UCI"))
    r2=round(r,digits=get_option("stat_digits"))
    print(r2)
  }
  reg$OR <- r
  names(dimnames(reg$OR)) <- c("","Odds ratio with CI")
  invisible(reg)
}
