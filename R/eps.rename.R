#' Title
#'
#' @param data data.frame
#' @param colname chr - name of the column
#' @param newname chr - new name of the column
#'
#' @return data.frame
#' @export
#'
#' @examples #
rename <- function(data, colname, newname)
{
  names(data) <- sub(colname, newname, names(data))
  data
}
