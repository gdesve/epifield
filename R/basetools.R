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

r <- requireNamespace("utils", quietly = TRUE)
if (!r) {
  message("Package utils required")
}

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

filename <-function(text) {
  # remove extension

  # remove path

}

fileext <- function(text) {
  x <- strsplit(text,"\\.")
  i <- length(x[[1]])
  ext <- x[[1]][i]
  ext
}

read <- function(filename) {
  s <- filename
  ext <- fileext(filename)
  name <-
  # look at the content
  test <- read.csv(file = name , nrows = 1)
  # count and identify separator
  if (ext == "csv") {

     read.csv(filename)
  }
  if (ext == "dta") {
    # foreign packages is required

  }

}

set.test <- function (x, value) {
   t <- value
}

set.global <- function (x, value) {
  x <- deparse(substitute(x))
  assign(x, value, pos=.GlobalEnv)
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

freq <- function(varx) {
   distrib <- table(varx)
   prop <- round(distrib / sum(distrib), digits = 2)
   print(distrib)
   prop
}

