#' Move a column of a data.frame to another position
#'
#' @param data data.frame to be modified 
#' @param tomove character - the column to be moved
#' @param where character - in c("first", "last", "before", "after")
#' @param target - character - column when where is "after" or "before"
#'
#' @return data.frame
#' @export 
#'
#' @examples
#' library(EpiStats)
#' data(Tiramisu)
#' Tiramisu <- eps.move(Tiramisu, "uniquekey", "after", "dateonset")
eps.move <- function(data, tomove, where = "last", target = NULL) {
  tomove <- as.character(substitute(tomove))
  target <- as.character(substitute(target))
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      data[append(temp, values = tomove, after = (match(target, temp)-1))]
    },
    after = {
      data[append(temp, values = tomove, after = (match(target, temp)))]
    })
  x
}

# Test de la fonction
# library(EpiStats)
# data(Tiramisu)
# str(Tiramisu)
# Tiramisu <- eps.move(Tiramisu, "uniquekey", "after", "dateonset")
# str(Tiramisu)

