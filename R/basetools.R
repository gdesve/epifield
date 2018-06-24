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


read <- function(filename) {
  s <- filename


}



freq <- function(varx) {
   distrib <- table(varx)
   prop <- round(distrib / sum(distrib), digits = 2)
   print(distrib)
   prop
}

