#' @title Calculate incidence rates with CI
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

      r <- cbind(p, low, up)
      colnames(r) <- c("IR","LCI", "UCI")
      # rownames <- categories
      rows <- vector("character",length = length(df[,1]))
      rows[] <- ""
      rownames(r) <- rows
      names(dimnames(r)) <- c("","Incidence rates")
      title = paste("Incidence of ","")
      outputtable(r,deci=6,title="Incidence of ",coldeci=c(TRUE,TRUE,TRUE))
      return(r)
}



#' @title Compute attack rates
#'
#' @description  Compute attack rates for two by two tables
#'
#' @param outcome Variable for outcome values
#' @param exposure Variable to be tested as risk factor
#' @param data    The dataset
#' @param rowcol  Row or col percentage
#'
#' @return  The result table
#' @export
#'
attack.rate <- function(outcome, exposure, data, rowcol = "cols") {

  #create an empty list to store results
  output <- list()

  counts <- table(data[, exposure], data[, outcome] )
  #get column proportions
  prop <- round(prop.table(counts, 1) * 100, digits = 2)
  #get row totals
  denominator <- rowSums(counts)[2]

  #pull counts together
  result <- cbind(Ill = counts[2, ], N = denominator, AR = prop[2, ])

  if (nrow(counts) > 2) {
      #get column proportions
      prop <- round(prop.table(counts, 1) * 100, digits = 2)

      #get row totals
      denominator <- rowSums(counts)

      #pull counts together
      result <- cbind(Ill = counts[ , 2], N = denominator, Proportions = prop[ , 2])
    }

    #store your output table in the list
    output[[exposure]] <- result

  return(output)
}

