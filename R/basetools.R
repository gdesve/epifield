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

# Dataset must be documented
#'#title Test sample from Tiramisu outbreak
#'
#' A dataset containing somedata for testing purpose
#'
#' @format A data frame with 10 rows and 5 variables:
#' \describe{
#'   \item{X}{Sequential number}
#'   \item{age}{Age of cases}
#'   \item{sex}{outcome variable}
#'   \item{tira}{outcome variable}
#'   \item{beer}{outcome variable}
#'
#' }
#' @source  Epiet case study
"test"


epif_env <- new.env(parent = emptyenv())

epif_env$start <- 1
epif_env$end <- 2

SEP   <- "|"
CROSS <- "+"
LINE  <- "-"
FIRST <- 12
COL   <- 8

epif_env$last_var <- ""

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
  # we get op as symbol
  s_op <- deparse(substitute(op))
  # if op is a variable wich contain char, we use content of op
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

#' setdata
#'
#' assign a data.frame as default dataframe
#'
#' @param df name of the dataframe to assign
#' @export
#' @return  current df name
#' @examples
#' df <-as.data.frame(c(1,2))
#' setdata(df)
#' rm(df)
#'

setdata <- function(df=NULL) {
    if (missing(df)) {
       return(get_option("dataset"))
    } else if (is.data.frame(df)) {
      # tester si le dataset est bien nommé et n'a pas été construit en direct
      e <- as.character(substitute(df))
      if (sum(match(ls.str(.GlobalEnv,mode="list"),e),na.rm=TRUE) > 0 ) {
             set_option("dataset",substitute(df))
      } else {
         stop("erreur dataset name is incorrect")
      }
    } else if (is.character(df)) {
      # si on passe le nom alors c'est bon, on le recupère direct
      set_option("dataset",df)
    }
    # pour finir verifier que df fait bien partie de l'environnement
}




#' Title count
#'
#' count number of record / row corresponding to expr
#'
#' @param expr A logical expression
#'
#' @return Number of rows macthing expr
#' @export
#'
#' @examples
#' count(c(1,2,3,1)==1)
#'
count <- function(expr) {
  # print(as.list(match.call()))
  r<-try(eval(expr),TRUE)
  if (inherits(r, "try-error")){
    # it's not a correct formula ... try to do better
    call <- as.call(list(sum,substitute(expr),na.rm = TRUE))
    env <- get_option("dataset")
    if (is.character(env) & ! env=="") {
      env <- eval(parse(text=env)) # epif_env$dataset
      r <- eval(call,env,parent.frame())
    }
  } else {  # formula is correct ... dont't change anything
    r <- sum(expr,na.rm=TRUE)
  }
  r
  # if (is.logical(expr) ) print(TRUE)
}

# Another possibility would be to complete expr with getdf / setdata
# r<-try(eval(sum(heu == 2)))
# r[1] : Error in eval(sum(heu == 2)) : objet 'heu' introuvable
# substr(expr , heu ) <-  tira$heu




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

# count number of specific char into a text using reg expr
charcount <- function(pattern,stosearch) {
  lengths(regmatches(stosearch, gregexpr(pattern, stosearch)))
  # length(attr(gregexpr(pattern,stosearch)[[1]],
  #            "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}

replicate <- function(char,ntime) {
  paste(rep(char,ntime),collapse="")
}

lpad <- function(value,width = 11, digit = 0) {
  r <- format(format(value,nsmall=digit),width = width ,justify="right")
  if (is.character(value) & (nchar(r) > width )) {
    r <- paste0(substr(r,1,width-2),"..")
  }
  return(r)
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
#'  read a data.frame.
#'  The function try to identify the file structure in order to call the appropriate specific
#'  command
#'
#' @export
#' @param filename  Name of file to be read. Type is defined by extension
#' @examples
#' fil <- tempfile(fileext = ".data")
#' cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
#' sep = "\n")
#' readLines(fil, n = -1)
#' unlink(fil) # tidy up
#'


use <- function(filename="") {
  # no file ? choose one
  if (filename=="") {
    filename <- file.choose()
  }
  # try to extract name...
  s <- filename
  ext <- file.ext(filename)
  name <- file.name(filename)
  if ( file.exists(filename)) {
    # file exists.. let's go
    if (ext == "csv") {
      # look at the content
      # count and identify separator
      test <- readLines(filename , n = 2)
      comma1 <- charcount(",",test[1])
      semicol1 <- charcount(";",test[1])
      comma2 <- charcount(",",test[2])
      semicol2 <- charcount(";",test[2])
      if (comma1 > 0 ) {
         df <- utils::read.csv(filename)
      }
      if (semicol1  > 0 ) {
        df <- utils::read.csv2(filename)
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
    fileatt <- dim(df)
    cat("File ",filename," loaded. \n")
    cat(fileatt[1], "Observations of ",fileatt[2]," variables. Use str(name) for details")
    invisible(df)
  } else {
    # file doens't exists ??
    cat("File \"",filename,"\" doesn't exist.\n", sep="")
    cat("Verify your working directory. Current is", getwd())

  }
}


#
#x <- NA
#try( x <- ... )
#if( is.na(x) ) {
#  ...
#} else {
#  ...
#}


#'  @title clear
#'
#'  clear memory for beginner.
#'
#'
#' @export
#' @param all  if all function are also removed from memory
#'
#'
clear <- function(all=FALSE) {
  arg <- as.list(match.call())
  if ((!is.null(arg[["all"]]))  ) {
    rm(list=ls(.GlobalEnv),envir=.GlobalEnv)
  } else {
    rm(list=setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv,mode="function")), envir=.GlobalEnv)
  }
  result <- gc()  # garbage collector
}

#internal function to retrieve dataset variables
getvar <- function(varname=NULL) {
  var <- deparse(substitute(varname))
  if ( var=="NULL" ) {
    return(epif_env$last_var)
  }
  epif_env$last_var <- var
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
         epif_env$last_var <- dfvar
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


tab_line <- function(ncol,tot=FALSE) {
  l1 <- replicate(LINE,FIRST+1)
  l2 <- replicate(LINE,(ncol-1)*(COL+1))
  l3 <- ifelse(tot,CROSS,LINE)
  l4 <- replicate(LINE,COL+1)
  cat(l1,CROSS,l2,l3,l4,"\n",sep ="")
}

tab_row <- function(rname,line,deci,tot=FALSE) {
  l <- length(line)
  cat(lpad(rname,FIRST))
  cat("",SEP)
  for (i in 1:(l-1)) {
    cat(lpad(line[i],COL,digit = deci[i])," ")
  }
  if (tot) cat(COL)
  cat(lpad(line[l],COL,digit = deci[l]))
  cat("\n")
}


outputtable <- function(table,deci=NULL,tot=FALSE,title="Frequency distribution",subtitle="" )  {

  cat(title,"\n")
  ncol <- dim(table)[2]
  nline <- dim(table)[1]
  coln <- colnames(table)
  rown <- rownames(table)

  if (is.null(deci)) {
    deci[1:nline] <- 0
  }
  # Les entêtes de colonne
  tab_row(subtitle,coln,deci,tot)
  tab_line(ncol,tot)
  for (i in (1:(nline-1))) {
    tab_row(rown[i],table[i,],deci,tot)
  }
  tab_line(ncol,tot)
  tab_row(rown[nline],table[nline,],deci,tot)
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

