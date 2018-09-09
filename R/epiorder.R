#' epiorder
#'
#' @param data data.frame
#' @param vars vector of colnames
#' @param values vector of ordered values
#' @param labels vector of optionals ordered labels
#'
#' @return data.frame
#' @export epiorder
#'
#' @author Jean Pierre Decorps
#'
#' @examples
#' # TODO
epiorder <- function(data, vars, values, labels=NULL) {
  .var <- c(vars)
  .df <- data
  for (v in .var) {
    if (!is.null(labels)) {
      .df[,v] <- factor(.df[,v], levels=values, ordered = TRUE, labels=labels)
    } else {
      .df[,v] <- factor(.df[,v], levels=values, ordered = TRUE)
    }
  }
  .df
}
