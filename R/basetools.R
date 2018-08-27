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
#'#title Gastroenteritidis outbreak in pensylvania
#'
#' A dataset containing the data from an outbreak
#'
#' @format A data frame with 103 rows and 32 variables:
#' \describe{
#'   \item{id}{Sequential number}
#'   \item{dob}{Date of birth}
#'   \item{age}{Age (year)}
#'   \item{sex}{Sex Males / Females}
#'   \item{floor}{Floor of residence}
#'   \item{room}{Room number}
#'   \item{meal}{Meal in dinning room}
#'   \item{duration}{Duration of symptom}
#'   \item{dayonset}{Day of onset}
#'   \item{timeonset}{Time of onset}
#'   \item{diarrhoea}{Diarrhoea}
#'   \item{temperatur}{temperatur}
#'   \item{fever}{fever}
#'   \item{vomiting}{vomiting}
#'   \item{abdopain}{abdopain}
#'   \item{nausea}{nausea}
#'   \item{headache}{headache}
#'   \item{blended}{blended}
#'   \item{eggs}{eggs}
#'   \item{chicken}{chicken}
#'   \item{potatoes}{potatoes}
#'   \item{broccoli}{broccoli}
#'   \item{ham}{ham}
#'   \item{sandwich}{sandwich}
#'   \item{fruits}{fruits}
#'   \item{blendedfru}{blendedfru}
#'   \item{protein}{protein}
#'   \item{lab}{lab}
#'   \item{agegroup}{agegroup}
#'   \item{case}{case}
#'   \item{earlycase}{earlycase}
#'   \item{latecase}{latecase}
#'
#' }
#' @source  Alain Moren investigation & case study
"gastro"

# Dataset must be documented
#'#title Test sample from outbreak
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
  s_op <- as.character(substitute(op))
  if (exists(s_op)) {
    if (is.character(op)) {
      s_op <- op
    }
  }
  if (match(s_op, ls(envir = epif_env), nomatch = 0)) {
    eval(parse(text = paste0("epif_env$", s_op)))
  } else {
    # warning("Option unavailable")
    r <- ""
  }
}

#' list_option
#'
#' retrieve all package option
#'
#' @export
#' @return  list of option values
#' @examples
#' list_option()
#'
#'

list_option <- function() ls(envir = epif_env)

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
  if (exists(s_op)) {
    if (is.character(op)) {
      s_op <- op
    }
  }
  old <- NA

  eval(parse(text = paste0("old <- epif_env$", s_op)))
  eval(parse(text = paste0("epif_env$", s_op , "<- value")))

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

setdata <- function(df = NULL) {
  if (missing(df)) {
    return(get_option("dataset"))
  } else if (is.data.frame(df)) {
    # tester si le dataset est bien nommé et n'a pas été construit en direct
    e <- as.character(substitute(df))
    if (sum(match(ls.str(.GlobalEnv, mode = "list"), e), na.rm = TRUE) > 0) {
      set_option("dataset", as.character(substitute(df)) )
    } else {
      stop("erreur dataset name is incorrect")
    }
  } else if (is.character(df)) {
    # si on passe le nom alors on le recupère direct
    if (exists(df)) {
      if (is.data.frame(get(df))) {
         set_option("dataset", df)
      } else cat(df , " is not a data.frame")
    }
  }
  # pour finir verifier que df fait bien partie de l'environnement
}

getdata <- function() {

  df <- get_option("dataset")  # epif_env$dataset

  if ( is.character(df) ) {
    if (! df == "") {
      # dataset contain name ... then get the data.frame
      df <- eval(parse(text = df))
    }
  }
  # we verify that we finally have a dataframe
  if ( ! is.data.frame(df)) {
    df <- NULL
  }
  df
}


#' @title  count number of record / row
#'
#' @description Toyal return the number of row sastifying a condition.
#'
#' @param expr A logical expression
#'
#' @return Number of rows macthing expr
#' @export
#'
#' @examples
#' total(c(1, 2, 3, 1) == 1)
#'
total <- function(expr) {
  # print(as.list(match.call()))
  m <- NA
  if (missing(expr)) {
    expr <- getdata()  # epif_env$dataset
  }
  r <- try(eval(expr), TRUE)
  if (inherits(r, "try-error")) {
    # it's not a correct formula ... we store the error
    m <- r[[1]]
    r <- NA
    # and try to do better by evaluating in the context of current dataset
    call <- as.call(list(sum, substitute(expr), na.rm = TRUE))
    env <- getdata()  # epif_env$dataset
    if (is.data.frame(env)) {
      # we evaluate the "sum" in that environnement
      m <- NA
      r <- try(eval(call, env, parent.frame()) , TRUE)
      if (inherits(r, "try-error")) {
        # still an error we report them
        m <- r[[1]]
        r <- NA
      }
    }
  } else {
    # formula is correct, is it a data frame ?
    if (is.data.frame(expr)) {
      # we return number of row
      r <- dim.data.frame(expr)[[1]]
    } else {
      # ... dont't change anything
      r <- sum(expr, na.rm = TRUE)
    }
  }
  if (!is.na(m)) {
    # should look for different error ... (not found ) / ( $ operator is invalid : "" are missing)
    red("Error :")
    pos <- regexpr("object '(\\w+)' not found", m )[1]
    normal(substring(m, pos ))
    cat("Considere to set the default data.frame using ")
    bold("setdata(dataname)")
    normal("\n")
  }
  r
  # if (is.logical(expr) ) print(TRUE)
}

# Another possibility would be to complete expr with getdf automatically by looking for object
# avec un regexpr type
#  r <- regexpr("object '(\\w+)' not found", r[1],perl=TRUE)
#  p <- r[1]
#  l <- r[2]
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
#'
right = function (text, num_char) {
  substr(text, nchar(text) - (num_char - 1), nchar(text))
}


left = function (text, num_char) {
  substr(text, 1, num_char)
}

mid = function(text, start_num, num_char) {
  substr(text, start_num, start_num + num_char - 1)
}

# count number of specific char into a text using reg expr
charcount <- function(pattern, stosearch) {
  lengths(regmatches(stosearch, gregexpr(pattern, stosearch)))
  # length(attr(gregexpr(pattern,stosearch)[[1]],
  #            "match.length")[attr(gregexpr(pattern,stosearch)[[1]], "match.length")>0])
}

pos <- function(pattern, stosearch) {
    r <- regexpr(pattern, stosearch)
    r <- ifelse(r < 0,0,r)
}

replicate <- function(char, ntime) {
  paste(rep(char, ntime), collapse = "")
}

catret  <- function(...) {
   cat(...,"\n")
}

lpad <- function(value,
                 width = 11,
                 digit = 0) {
  r <-
    format(format(value, nsmall = digit),
           width = width ,
           justify = "right")
  if (is.character(value) & (nchar(r) > width)) {
    r <- paste0(substr(r, 1, width - 2), "..")
  }
  return(r)
}

file.ext <- function(text) {
  x <- strsplit(text, "\\.")
  i <- length(x[[1]])
  ext <- ""
  if (i > 1) {
    ext <- x[[1]][i]
  }
  ext
}

file.name <- function(text) {
  name <- basename(text)
  x <- strsplit(name, "\\.")
  x[[1]][1]
}

ask <- function(message,answers) {
  r <- ""
  while(r=="" ){
    n <- readline(message)
    if(!is.na(match(n,as.vector(answers)))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
    break
  }
}


ok <- function() {
  ask("Do you confirm?", c("Yes", "Y", "y") )
}

bold <- function(tx) {
  cat("\033[1m",tx,sep="")
}

italic <- function(tx) {
  cat("\033[3m",tx,sep="")
}

red <- function(tx) {
  cat("\033[31m",tx,sep="")
}

normal <- function(tx) {
  cat("\033[0m",tx,sep="")
}





#'  read
#'
#'  read a data.frame.
#'  The function try to identify the file structure in order to call the appropriate specific
#'  command
#'
#' @export
#' @importFrom foreign read.dta
#' @param filename  Name of file to be read. Type is defined by extension
#' @param label Label to be added as attribute to dataframe
#' @examples
#' fil <- tempfile(fileext = ".data")
#' cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
#' sep = "\n")
#' df <- read(filename=fil)
#' unlink(fil) # tidy up
#'


read <- function(filename = "", label = NULL) {
  # no file ? choose one
  if (filename == "") {
    r <- try(filename <- file.choose())
    if (inherits(r, "try-error")) {
      # user have cancelled , stop now
      return(r)
    }
  }
  # try to extract name...
  s <- filename
  ext <- file.ext(filename)
  name <- file.name(filename)
  if (file.exists(filename)) {
    # file exists.. let's go
    if (ext == "csv") {
      # look at the content
      # count and identify separator
      test <- readLines(filename , n = 2)
      comma1 <- charcount(",", test[1])
      semicol1 <- charcount(";", test[1])
      comma2 <- charcount(",", test[2])
      semicol2 <- charcount(";", test[2])
      if (comma1 > 0) {
        df <- utils::read.csv(filename)
      } else if (semicol1  > 0) {
        df <- utils::read.csv2(filename)
      }
    } else  if (ext == "dta") {
      # foreign packages is required
      r <- requireNamespace("foreign", quietly = TRUE)
      if (!r) {
        message("Package foreign required")
      }
      df <- foreign::read.dta(filename)
    } else if (ext == "rec") {
      # foreign packages is required
      r <- requireNamespace("foreign", quietly = TRUE)
      if (!r) {
        message("Package foreign required")
      }
      df <- foreign::read.epiinfo(filename)
    } else {
      cat("Extension '", ext, "'not found")
    }
    if (!missing(label)) {
      attr(df, "label") <- label
    }
    if (is.data.frame(df)) {
      fileatt <- dim(df)
      cat("File ", filename, " loaded. \n")
      cat(fileatt[1],
          "Observations of ",
          fileatt[2],
          " variables. Use str(name) for details")
      invisible(df)
    }
  } else {
    # file doens't exists ??
    cat("File \"", filename, "\" doesn't exist.\n", sep = "")
    cat("Verify your working directory. Current is", getwd())

  }
}

# Not sure it' usefull because specific syntax ....
# label(test) <- "test data"
`label<-` <- function(x,value) {
  attr(x,"label") <- value
  x
}

#
#x <- NA
#try( x <- ... )
#if( is.na(x) ) {
#  ...
#} else {
#  ...
#}

add.sep <- function(li,c) {
  sep <- function(x)  paste(x, c)
  li2 <- lapply(li,sep)
  l <- length(li)
  li2[l] <- li[l]
  li2
}


#' @title Remove variables, dataset or functions from memory
#'
#' @description
#' Clear can be used to remove objects from memory (variables, data.frame, functions).
#' Clear is easier than \code{\link{rm}} and is more secure because, by default, it ask for confirmation.
#' Objects to remove can be specified as is or by their name ("character").
#' It's possible to erase all vars, all functions using keywords : "vars" or "functions"
#' "all" keyword will allows total cleaning.
#' @details
#' When keyword or pattern are used and there is more than one object to clear, a confirmation will be issued.
#' Except if noask parameters is set to true
#'
#' @export
#' @importFrom utils ls.str
#' @param what Keyword (vars, functions, all) or pattern
#' @param noask to clear whithout confirmation. Useful when running from a script
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality, available at \url{https://github.com/}.
#' @seealso \code{\link{rm}}
#' @examples
#' tmp <- 5
#' temp <- 5
#' clear(t)
#'
clear <- function(what, noask = FALSE) {
  # arg <- as.list(match.call())
  continue <- TRUE
  if ( missing(what) ) what <- "vars"
  swhat <- as.character(substitute(what))
  if ( length(swhat) > 1 ) {
    swhat <- paste0(swhat[2],swhat[1],swhat[3])
  }
  if ( sum(grep("\\$",swhat) ) > 0 ) {
    cat("To clear a data.frame variable like ")
    italic(swhat)
    normal("  Use drop function")
    continue <- FALSE
  }
  # if expr is a variable wich contain char, we can use content of expr ?
  # if (continue & exists(swhat,.GlobalEnv, inherits = FALSE)) {
  #   if (is.character(what) & length(what)==1) {
  #     twhat <- what
  #     swhat <- ifelse(exists(twhat,.GlobalEnv, inherits = FALSE),what,swhat)
  #   }
  # }
  #swhat <- parse(swhat)
  if ( continue ) {
    switch (
      swhat,
      "vars" = { li = setdiff(ls(.GlobalEnv), ls.str(.GlobalEnv, mode = "function")) } ,
      "functions" = { li = ls.str(.GlobalEnv, mode = "function") },
      "all" =  { li = ls((.GlobalEnv)) },
      {
        # there is an objects with that name... we remove it
        if ( exists(swhat) ) {
          li <- c(swhat)
        } else {
           li <- ls(.GlobalEnv, pattern = swhat)
        }
      }
    )
    l <- length(li)
    if (l > 0) {
      lid <- add.sep(li,"- ")
      cat(l, " objet(s) to remove :")
      italic(as.character(lid))
      normal("\n")
      if ( ( l == 1 & li[1]==swhat ) ||  noask || ok() ) {
          rm(list = li, envir = .GlobalEnv)
      }
    } else {
        cat("No such objets :'")
        italic(swhat)
        normal("'. Use keywords:")
        bold("vars, functions, all")
        normal(" or a pattern (see help)")
    }
    result <- gc()  # garbage collector
  }
}


is.var <- function(what="") {
  lsfound <- FALSE
  r <- try(mode(what),TRUE)
  if ( ! inherits(r, "try-error")) {
     mwhat <- r
     switch(mwhat ,
      "name" = {
         what <- as.character(substitute(what))
       } ,
      "call" = {
        what <- ""
      } ,
      "function" = {
         what <- ""
         }
     )
     if (length(what) > 1) {
        what <- as.character(substitute(what))
     }
     if ( ! (what == "") ) {
       lsys <- sys.nframe()-1
       what <-glob2rx(what)
       for (i in lsys:0)  {
          lc <- ls(sys.frame(i),pattern=what)
          if ( length(lc) > 0 ) lsfound <- TRUE
       }
     }
   }
   lsfound
}


#' @importFrom utils glob2rx
#internal function to retrieve dataset variables
getvar <- function(what = NULL) {

  # first, if what is missing we return previous one
  if (missing(what)) {
    return(get_option("last_var"))
  } else {

    # should we look at var content ??
    # subst <- FALSE
    # if var is char content is used
    # if (exists(var)) {
    #   if (is.character(varname) & length(varname)== 1 ) {
    #      var<-eval(varname)
    #      subst<-TRUE
    #   }
    # }

    # Look at type of argument and get a working version of it
    mwhat <- mode(what)
    switch(mwhat ,
      "character" = {
        varname <- what
      } ,
      "call" =  {
        varname <- deparse(what)
      } ,
      "name" = {
        varname <- as.character(what)
      } ,
      { # else
        varname <- what
      }
    )

    # got it, we save the name
    epif_env$last_var <- varname

    # just create an expression with content
    ex <- parse(text=varname)

    if (is.var(varname)) {
      # if var exists it is returned as is
      # We evaluate in case what was passed with quote
      # because we evaluate an expression we must avoid evaluating local var !
      # then we start with parent and go ahead until .GlobalEnv
      lsys <- sys.nframe()-1
      while (lsys >= 0) {
         r <- try(eval.parent(ex,lsys-1),TRUE)
         if (!inherits(r, "try-error")) {
           return(r)
           lsys <- -1
         } else lsys <- lsys-1
      }
    } else {
      # var doesn't exist.. may be it's a formula ? We try to eval but we catch error
      continue <- FALSE
      r <- try(eval(ex), TRUE)
      if (!inherits(r, "try-error")) {
        # it's a formula ... it's evaluation is returned if not a function
        if ( ! mode(r) == "function" ) {
          return(r)
        } else continue <- TRUE
      }
      if (continue) {
        # may be varname is part of a dataset ?
        .df <-
          names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
        ndf <- length(.df)
        j <- 1
        nfound <- 0
        dffound <- ""
        while (j <= ndf) {
          ifound <- grep(varname, names(get(.df[j])))
          if (length(ifound) > 0) {
            dfname <- .df[j]
            nfound <- nfound + 1
            # list of dataset containing varname
            dffound <-
              paste0(dffound, ifelse(dffound == "", "", ", "), dfname)
          }
          j <- j + 1
        }
        # only one ? great
        if (nfound == 1) {
          varname <- paste(dfname, "$", varname , sep = "")
          # we update varname with data.frame value
          epif_env$last_var <- varname
          return(eval(parse(text =varname)))
        } else {
          if (nfound > 1) {
            warning(
              paste(
                varname ,
                "is an ambigous name and exists in following dataset :",
                dffound
              ),
              call. = FALSE
            )
            return(NULL)
          } else {
            warning(paste(varname , "is not defined as variable or data.frame column"), call. = FALSE)
            return(NULL)
          }
        } # 0 or more than 1
      } # it's not a formula
    } # var not exists
  } # not missing
}


tab_line <- function(ncol, tot = FALSE) {
  l1 <- replicate(LINE, FIRST + 1)
  l2 <- replicate(LINE, (ncol - 1) * (COL + 1))
  l3 <- ifelse(tot, CROSS, LINE)
  l4 <- replicate(LINE, COL + 1)
  cat(l1, CROSS, l2, l3, l4, "\n", sep = "")
}

tab_row <- function(rname, line, deci, tot = FALSE) {
  l <- length(line)
  cat(lpad(rname, FIRST))
  cat("", SEP)
  for (i in 1:(l - 1)) {
    cat(lpad(line[i], COL, digit = deci[i]), " ")
  }
  if (tot)
    cat(COL)
  cat(lpad(line[l], COL, digit = deci[l]))
  cat("\n")
}


outputtable <-
  function(table,
           deci = NULL,
           tot = FALSE,
           title = "Frequency distribution",
           subtitle = "")  {
    cat(title, "\n")
    ncol <- dim(table)[2]
    nline <- dim(table)[1]
    coln <- colnames(table)
    rown <- rownames(table)

    if (is.null(deci)) {
      deci[1:nline] <- 0
    }
    # Les entêtes de colonne
    tab_row(subtitle, coln, deci, tot)
    tab_line(ncol, tot)
    for (i in (1:(nline - 1))) {
      tab_row(rown[i], table[i, ], deci, tot)
    }
    tab_line(ncol, tot)
    tab_row(rown[nline], table[nline, ], deci, tot)
  }



getdf <- function(varname) {
  .df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
  ndf <- length(.df)
  j <- 1
  nfound <- 0
  while (j <= ndf) {
    ifound <- grep(varname, names(get(.df[j])))
    if (length(ifound) > 0) {
      dfname <- .df[j]
      nfound <- nfound + 1
    }
    # cat(.df[j]," ",ifound," ",nfound," ",dfname)
    j <- j + 1
  }
  if (nfound == 1) {
    dfvar <- paste(dfname, "$", varname , sep = "")
    return(eval(parse(text = dfvar)))
  } else {
    if (nfound > 1) {
      cat(varname , " is ambigous")
    } else {
      cat(varname , "is not defined")
    }
  }
}


rename <- function(oldname, newname) {
  names(data) <- sub(oldname, newname, names(data))

}
