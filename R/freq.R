# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#' #'
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#' @export
#' @param ... As numbers, factors or text.
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'

freq <- function(...) {
  arg.passed <- substitute(...)

  cur.var <- getvar(arg.passed)
  if (! is.null(cur.var)) {
    count <- table(cur.var, useNA="no")
    tot <- length(cur.var)
    prop <- round(prop.table(count)*100, digits = 2)
    cum <- cumsum(prop)
    result <- cbind(count,
                    prop,
                    cum)
    mis  <- sum(is.na(cur.var))
    var.name <- get_option("last_varname")
    colnames(result) <- c("Freq", "%" , "cum%")
    result <- rbind(result,Total = colSums(result))
    deci <- c(0,2,2)
    result[nrow(result),ncol(result)] <- 100
    title <- paste("Frequency distribution of",getvar())
    names(dimnames(result))  <- c(var.name,title)
    outputtable(result,deci,tot=FALSE,title)
    # missing should be added to result
    cat("Missings :",mis," (",round(mis/tot*100, digits = 2),"%)\n")
    r <- list()
    r$table <- result
    r$total <- tot
    r$missing <- mis
    invisible(r)
  }
}


# epifield documentation for RData using roxygen2
#' @title
#' Cross tabulation ( 2by2 table).
#' @description
#' \code{epitable} Display a cross tabulation of two binary variables optionnaly with
#'  row or col percentages.
#'
#'
#' @name epitable
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{freq}} for frequency distributions
#' @importFrom stats chisq.test fisher.test
#' @export
#' @param exp  "Exposure" as numbers, factors or text.
#' @param out  "Outcome" as numbers, factors or text
#' @param row  "Row percentages"
#' @param col  "Col percentages"
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' \dontrun{
#' epitable(c(3,1,2,2,5))
#' }
#'
#'
epitable <- function(exp,out,row=FALSE,col=FALSE)  {
   r <- as.list(match.call())
   expdata <- getvar(r$exp)
   expdata.name <- getvarname()

   tot <- length(expdata)

   outdata <- getvar(r$out)
   outdata.name <- getvarname()
   # length to be verified

   # to get options
   params <- names(r)

   if (! ( is.null(expdata) |  is.null(outdata) )  ) {


     # calculations
     r <- table(expdata,outdata,useNA="no")
     names(dimnames(r))  <- c(expdata.name,outdata.name)
     # check size of result table
     bin <- (dim(r)==c(2,2)&&TRUE)

     mis  <- sum(is.na(expdata)|is.na(outdata))
     if (bin) {
       t <- chisq.test(r)
       f <- fisher.test(r)$p.value
     }
     print(r)

     if (row) {
       prop <- prop.table(r,1)
       prop <- cbind(prop,100)
       print(prop)
     }

     result <- list()
     result$table <- r
     result$chisq <- t$statistic[[1]]
     result$chisq.p <- t$p.value
     result$fischer <- t$p.value
     result$missing <- mis

     cat(t$statistic,"(", t$p.value,") Fisherexact :",f)

     cat("Missings :",mis," (",round(mis/tot*100, digits = 2),"%)\n")

     return(result)
   }
}


# epifield documentation using roxygen2
#' @title
#' Reorder data for epi table ( 2by2 table).
#' @description
#' \code{epiorder} Rearrange order of factor variable to produce classical epi table
#'  1/0  Yes/No  +/-
#'
#'
#' @name epiorder
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{epitable}} for cross tabulation
#' @export
#' @param var  Variable to reorder (will be converted as factor).
#' @param mode "Yesno"   "Yesno"  "10" "+-"
#' @param custom  Custom labels
#'
#' @return A vector reordered
#' @examples
#' \dontrun{
#' epiorder(c(0,1,0,1,1))
#' }
#'
#'
epiorder <- function(var,mode="Yesno",custom=NULL) {
  r <- as.list(match.call())
  coldata <- getvar(r$var)
  colname <- getvarname()
  colfullname <- getvar()
  # colname <- as.character(substitute(var))
  if (! is.null(coldata) ) {
    dfname <- get_option("last_df")
    df=get(dfname)
    coldata <- factor(coldata)
    # test for type of levels  (otherwise calling it two time will erase all values)
    clevels <- levels(coldata)
    nlevels <- nlevels(coldata)
    if (nlevels > 2) {
       cat("epiorder is only for binary variables (with 2 categories)")
    } else if (sort(clevels)[1] == "0" ) {
      clevels <- c("1","0")
    } else if ( substr(toupper(sort(clevels)[1]),1,1) == "N" ) {
      clevels <- sort(clevels,decreasing = TRUE)
    }

    coldata <- factor(coldata, levels = c(1,0) , labels = c("Yes","No"), ordered = TRUE)
  }

  df[,colname] <- coldata

  # assign(dfname,df,inherits = TRUE )

  push.data(dfname,df)

  bold(colfullname)
  normal(" Reordered with labels: ")
  catret(levels(coldata))

  # exp <- paste0(substitute(var),"<- coldata")
  # r <- try(evalq(parse(text = exp), envir = df, enclos = .GlobalEnv),TRUE)
  # r
  # df
}

push.data <- function(dfname,df) {

  exp = call("<-",dfname,df)
  eval(exp,envir=.GlobalEnv)

}
