#' Title Calculate incidence rates with CI
#'
#' @param df  A 2 column data.frame
#' @param conflvl The confidence level, usualy 0.95
#'
#' @return a table of result
#' @export
#' @importFrom stats qchisq
#'
#' @examples
#' dat <- cbind(c(5,10,20),c(100,120,150))
#' incrates(dat)
incrates <- function(df, conflvl = 0.95)
{
  lim  <- 1 - ((1 - conflvl)/2)

#  if (is.matrix(df) == FALSE)
#      stop("Error: dat must be a two-column matrix")

      case <- df[, 1]
      total <- df[, 2]
      p <- case/total
      low <- ifelse(case == 0, 0, (0.5 * qchisq(p = lim, df = 2 * case + 2, lower.tail = FALSE)/total))
      up <- 0.5 * qchisq(p = 1 - lim, df = 2 * case, lower.tail = FALSE)/total

      r <- data.frame(IR = p, LCI = low, UCI = up)

      return(r)
}
