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

epif_env <- new.env(parent = emptyenv())

epif_env$start <- 1
epif_env$end <- 2

#' get_option
#'
#' retrieve a package option
#'
#' @param op name of the option to retrieve
#' @export
#' @return  option value
#' @examples
#' get_option("option")
#'
#'
get_option <- function(op) {
  s_op <- deparse(substitute(op))
  if ( exists(s_op)) {
      if (is.character(op)) {
        s_op <- op
      }
  }
  if (match(s_op, ls(envir = epif_env),nomatch=0) ) {
     eval(parse(text=paste0("epif_env$",s_op)))
  } else {
    warning("Option unavailable")
  }
}

#' set_option
#'
#' assign a package option
#'
#' @param op name of the option to assign
#' @param value The value to be assigned to option
#' @export
#' @return  option value
#' @examples
#' set_option("option",1)
#'
#'

set_option <- function(op, value) {
  s_op <- deparse(substitute(op))
  if ( exists(s_op)) {
    if (is.character(op)) {
      s_op <- op
    }
  }
  old <- NA
  eval(parse(text=paste0("old <- epif_env$",s_op)))
  eval(parse(text=paste0("epif_env$",s_op ,"<- value")))

  invisible(old)
}


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

# count number of specific char into a text
charcount <- function(pattern,stosearch) {
  length(attr(gregexpr(pattern,stosearch)[[1]],
              "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}


file.ext <- function(text) {
  x <- strsplit(text,"\\.")
  i <- length(x[[1]])
  ext <- ""
  if (i>1) {
    ext <- x[[1]][i]
  }
  ext
}

file.name <- function(text) {
  name <- basename(text)
  x <- strsplit(name,"\\.")
  x[[1]][1]
}

#'  use
#'
#'  read a data.frame
#'
#' @export
#' @param filename  Name of file to be read
#' @examples
#' fil <- tempfile(fileext = ".data")
#' cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
#' sep = "\n")
#' readLines(fil, n = -1)
#' unlink(fil) # tidy up
#'


use <- function(filename) {
  # try to find a name...
  s <- filename
  ext <- file.ext(filename)
  name <- file.name(filename)

  if (ext == "csv") {
    # look at the content
    # count and identify separator
    test <- readLines(filename , n = 3)
    comma1 <- charcount(",",test[1])
    comma2 <- charcount(",",test[2])
    if (comma1 > 0 & comma1 == comma2) {
       df <- utils::read.csv(filename)
    }
  }
  if (ext == "dta") {
      # foreign packages is required
      r <- requireNamespace("foreign", quietly = TRUE)
      if (!r) {
        message("Package foreign required")
      }
      df <- foreign::read.dta(filename)
   }
  utils::head(df)
  invisible(df)
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

#internal function to retrieve dataset variables
getvar <- function(varname) {
  var <- deparse(substitute(varname))
  # if var exists it is returned as is
  if (exists(var)) {
    return(varname) }
  else  {
    # var doesn't exist.. may be it's a formula ?
    r<-try(value <- varname,TRUE)
    if (!inherits(r, "try-error")){
      # it's a formula ... it's evaluation is returned
      return(r)
    } else {
      # may be varname is part of a dataset ?
      .df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
      ndf <- length(.df)
      j <- 1
      nfound <- 0
      dffound <- ""
      while(j <= ndf) {
        ifound <- grep(var,names(get(.df[j])))
        if (length(ifound)>0) {
          dfname <- .df[j]
          nfound <- nfound + 1
          # list of dataset containing varname
          dffound <- paste0(dffound,ifelse(dffound=="","",", "),dfname)
        }
        j <- j+1
      }
      # only one ? great
      if (nfound == 1) {
         dfvar <- paste(dfname,"$",var ,sep="")
         return(eval(parse(text=dfvar)))
      } else {
        if (nfound > 1) {
          warning(paste(var ,"is an ambigous name and exists in following dataset :", dffound),call.=FALSE)
          return(NULL)
        } else {
          warning(paste(var , "is not defined"),call.=FALSE)
          return(NULL)
        }
      }
    }
  }
}


d.line <- function() {
  cat("-----------------------------------------------------------\n")
}


getdf <- function(varname) {
  .df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
  ndf <- length(.df)
  j <- 1
  nfound <- 0
  while(j <= ndf) {
    ifound <- grep(varname,names(get(.df[j])))
    if (length(ifound)>0) {
      dfname <- .df[j]
      nfound <- nfound + 1
    }
    # cat(.df[j]," ",ifound," ",nfound," ",dfname)
    j <- j+1
  }
  if (nfound == 1) {
    dfvar <- paste(dfname,"$",varname ,sep="")
    return(eval(parse(text=dfvar)))
  } else {
    if (nfound > 1) {
      cat(varname ," is ambigous")
    } else {
      cat(varname , "is not defined")
    }
  }
}

rename <- function(oldname,newname) {

  names(data) <- sub(oldname, newname, names(data))

}

