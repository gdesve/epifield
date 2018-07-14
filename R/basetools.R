# Epifield basetools source
#
# Basic functions to facilitate further coding
#
# Objective is to produce a very simplified set of command
# for field epidemiologist who are familiar with Epi7 or Stata
#
#   http://
#
# Author : Gilles Desve & al...
#

# Dataset must be documented
#'#title Tiramisu outbreak
#'
#' A dataset containing the data from an outbreak
#'
#' @format A data frame with 291 rows and 22 variables:
#' \describe{
#'   \item{X}{Sequential number}
#'   \item{age}{Age of cases}
#'   \item{ill}{outcome variable}
#'   \item{dateonset}{outcome variable}
#'   \item{sex}{outcome variable}
#'   \item{tira}{outcome variable}
#'   \item{tportion}{outcome variable}
#'   \item{wmousse}{outcome variable}
#'   \item{dmousse}{outcome variable}
#'   \item{mousse}{outcome variable}
#'   \item{mportion}{outcome variable}
#'   \item{beer}{outcome variable}
#'   \item{uniquekey}{outcome variable}
#'   \item{redjelly}{outcome variable}
#'   \item{fruitsalad}{outcome variable}
#'   \item{tomato}{outcome variable}
#'   \item{mince}{outcome variable}
#'   \item{salmon}{outcome variable}
#'   \item{horseradish}{outcome variable}
#'   \item{chickenwin}{outcome variable}
#'   \item{roastbeef}{outcome variable}
#'   \item{pork}{outcome variable}
#'
#' }
#' @source  Epiet case study
"tira"



#' right
#'
#' Extract x rigth characters from a text
#'
#' @param text Text to extract from
#' @param num_char Number of char to extract from rigth
#'
#' @return  \code{num_char} extracted characters
#' @examples
#' \dontrun{
#' right("dummy_test",4)
#' }
#' @importFrom foreign read.dta
#' @importFrom utils ls.str
#'
right = function (text, num_char){
  substr(text,nchar(text)-(num_char-1),nchar(text))
}


left = function (text, num_char){
  substr(text,1,num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

charcount <- function(pattern,stosearch) {
  length(attr(gregexpr(pattern,stosearch)[[1]],
              "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}


file.ext <- function(text) {
  x <- strsplit(text,"\\.")
  i <- length(x[[1]])
  ext <- x[[1]][i]
  ext
}

file.name <- function(text) {
  name <- basename(text)
  x <- strsplit(name,"\\.")
  i <- length(x[[1]])
    if (i>1) {
    ext <- x[[1]][i-1]
    } else {
    ext <- name
  }
  ext
}

read <- function(filename) {
  s <- filename
  ext <- file.ext(filename)
  name <- file.name()
  # look at the content
  test <- utils::read.csv(file = name , nrows = 1)
  # count and identify separator
  if (ext == "csv") {
     utils::read.csv(filename)
  }
  if (ext == "dta") {
    # foreign packages is required
    r <- requireNamespace("foreign", quietly = TRUE)
    if (!r) {
      message("Package foreign required")
    }
    foreign::read.dta(filename)
  }
}

set.test <- function (x, value) {
   t <- value
}


#
#x <- NA
#try( x <- ... )
#if( is.na(x) ) {
#  ...
#} else {
#  ...
#}


clear <- function() {
  rm(list=setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv,mode="function")), envir=.GlobalEnv)
  result <- gc()  # garbage collector
}

setepifield <- function(env) {
  epifieldEnv <- new.env(parent = .GlobalEnv)
  epifieldEnv$option <- TRUE
}


test <- function(varname) {
  var <- deparse(substitute(varname))
  if (exists(var)) {
    return(varname) }
  else  {
    r<-try(value <- varname,TRUE)
    if (!inherits(r, "try-error")){
      return(r)
    } else {
      df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
      if (length(df)>0) {
         dfvar <- paste(df,"$",var ,sep="")
         return(eval(parse(text=dfvar)))
      } else  {
      cat(var , "is not defined")
      }
    }
  }
}


