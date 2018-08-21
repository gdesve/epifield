#' Title
#'
#' @param data 
#' @param colname 
#' @param newname 
#'
#' @return
#' @export
#'
#' @examples
epf.rename <- function(data, colname, newname)
{
  names(data) <- sub(colname, newname, names(data))
  data
}
