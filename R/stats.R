#' @title Calculate incidence rates with CI
#'
#' @param count variable to be used as countfor calculating the incidence
#' @param denom denominator to use for incidence rates
#' @param data  Optionnal data.frame containing the data
#' @param per integer to use as unit per
#' @param conflvl The confidence level, usualy 0.95
#'
#' @return a table of result
#' @export
#' @importFrom stats qchisq
#'
#' @examples
#' dat <- data.frame("lab"=c("A","B","C"),"nb"=c(5,10,20),"den"=c(100,120,150))
#' setdata(dat)
#' incrates(nb,den, dat)
#'
incrates <- function(count,denom, data, per = 100000, conflvl = 0.95) {
      r <- as.list(match.call())
      if (missing(data)) {
        data <- getlastdf()
        olddata <- ""
      } else {
        olddata <- setdata()
        setdata(deparse(substitute(data)))
      }

      lim  <- 1 - ((1 - conflvl)/2)
      case <- getvar(r$count)
      casename <- getvar()
      total <- getvar(r$denom)

      if (! olddata=="") setdata(olddata)

      total <- total/per
      p <- round(case/total,4)
      nline <- length(p)
      if (nline>0) {
      low <- ifelse(case == 0, 0, (0.5 * qchisq(p = lim, df = 2 * case + 2, lower.tail = FALSE)/total))
      up <- 0.5 * qchisq(p = 1 - lim, df = 2 * case, lower.tail = FALSE)/total
      low=round(low,4)
      up=round(up,4)
      if (is.data.frame(data)) {
         oldcolname <- colnames(data)
         oldnbvar <- length(oldcolname)
         coldec <- vector(mode="logical",length=oldnbvar)
         coldec <- c(coldec,TRUE,TRUE,TRUE)
         r <- cbind(data, p, low, up)
         newcolnames <- c(oldcolname,"IR","LCI", "UCI")
         colnames(r) <- newcolnames
      } else {
        r <- cbind(case, p, low, up)
        colnames(r) <- cbind(casename,"IR","LCI", "UCI")
        coldec <- c(FALSE,TRUE,TRUE,TRUE)
      }
      # rownames <- categories
      rownames(r) <- c(1:nline)
      names(dimnames(r)) <- c("","Incidence rates")
      title = paste("Incidence of ",casename,"per",format(per,scientific=FALSE))
      outputtable(r,deci=4,totrow=FALSE,title=title,coldeci=coldec,first=3)
      invisible(r)
      } else {cat("Error in formula") }
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

