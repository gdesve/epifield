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

<<<<<<< HEAD
file.ext <- function(text) {
=======
fileext <- function(text) {
>>>>>>> 8a7a9292a498d611490a762ba841d2b053caee37
  x <- strsplit(text,"\\.")
  i <- length(x[[1]])
  ext <- x[[1]][i]
  ext
}

<<<<<<< HEAD
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
=======
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
>>>>>>> 8a7a9292a498d611490a762ba841d2b053caee37

  }

read <- function(filename) {
  s <- filename
  ext <- file.ext(filename)
  name <- file.name()
  # look at the content
  test <- read.csv(file = name , nrows = 1)
  # count and identify separator
  if (ext == "csv") {
     read.csv(filename)
  }
  if (ext == "dta") {
    # foreign packages is required
    require("foreign")
  }
}

<<<<<<< HEAD
=======
set.test <- function (x, value) {
   t <- value
}

>>>>>>> 8a7a9292a498d611490a762ba841d2b053caee37
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
<<<<<<< HEAD
=======

clear <- function() {
  rm(list=setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv,mode="function")), envir=.GlobalEnv)
  result <- gc()  # garbage collector
}
>>>>>>> 8a7a9292a498d611490a762ba841d2b053caee37

clear <- function() {
  rm(list=setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv,mode="function")), envir=.GlobalEnv)
  result <- gc()  # garbage collector
}


