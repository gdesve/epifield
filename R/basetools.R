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

# constant used in epifield tools
SEP   <- "|"
CROSS <- "+"
LINE  <- "-"
FIRST <- 18
COL   <- 8

# epifield envirronement used to manage epifield options
epif_env <- new.env(parent = emptyenv())

# options for epifield
epif_env$start <- 1
epif_env$end <- 2
# default number of digit in stats output
epif_env$stat_digits <- 4
# default option controling output of R Code when usefull
epif_env$show_Rcode <- FALSE

# global to retrieve current and last selection in short syntax system
# The current selection applied to the current dataframe
epif_env$select <- ""

# The last_error in epifield functions
epif_env$last_error <- NA

# internal used to reset the short syntax system
resetvar <- function() {
  epif_env$last_var <- ""          # last argument object name (in long syntax)
  epif_env$last_isvar <- ""        # last argument is a column name ?
  epif_env$last_varname <- ""      # if yes, varname is the column name
  epif_env$last_df <- ""           # if yes, df is the last dataframe
  epif_env$last_error <- NA
}

# retrieve the last short varname (dataframe column name)
getvarname <- function()  { return(get_option("last_varname")) }

#' get_option
#'
#' retrieve an epifield package option or parameter
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

get_lastdfname <- function() {
  get_option("last_df")
}

# to retrieve or set last_error from epifield
last_error <- function(mess="")  {
  lastmessage <- get_option("last_error")
  if (! missing(mess)) {
    set_option("last_error",mess)
  }
  lastmessage
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
#' @return  The previous option value before new assignment
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

#' @title set or retrieve the default data.frame
#'
#' Set a data.frame as default data.frame for epifield function. This avoid typing
#' and simplify syntax for R newcomers. setdata is mandatory for some functions :
#' generate, countif
#' If missing df name, then setdata() return the current data.frame name
#'
#' @param df Name of the data.frame to set as default
#' @export
#' @return  The current data.frame name
#' @examples
#' df <-as.data.frame(c(1,2))
#' setdata(df)
#' rm(df)
#'

setdata <- function(df = NULL) {
   # if argument is NULL setdata return the current default data frame
   if (missing(df)) {
    return(get_option("dataset"))
  } else {
    # argument is a dataframe ?
    m_df <- try(is.data.frame(df),TRUE)
    if ( ! inherits(m_df, "try-error")) {
      # df exists as an object
      # if TRUE then it is a data frame
      if (m_df) {
        # setdata as a meaning only if the passed dataframe exist in environment
        c_df <- as.character(substitute(df))
        # the name is searched in global env
        if (sum(match(ls.str(.GlobalEnv, mode = "list"), c_df), na.rm = TRUE) > 0) {
          cat("Default data frame is now set to",c_df)
          set_option("dataset", c_df )
        } else {
          stop("Data frame should exist in global environnment")
        }
      # df is not a data frame, if arg is character, we search for a dataset named df
      } else if (is.character(df)) {
        # if df is empty then we cancel the default dataframe
        if (df=="") {
           set_option("dataset", df)
           cat("setdata cleared")
        } else if (exists(df)) {
          # an object named df exist, is it a data frame ?
          if (is.data.frame(get(df))) {
            set_option("dataset", df)
            cat("Default data frame is now set to",df)
          } else stop(df , " is not a data.frame")
        } else stop(df , " doesn't exist in environment")  # no object with that name
      }
    } else {
      # a data frame was passed directly as argument
      stop("Data frame should exist in global environnment")
    }
  }
}

# retrieve the default data.frame defined by setdata
# getdata return the df if there is only one in memory
getdata <- function() {
  df <- get_option("dataset")  # epif_env$dataset
  if ( is.character(df) ) {
    if (! df == "") {
      # dataset contain name ... then get the data.frame
      df <- get(df)
      # df <- eval(parse(text = df))
    }
  }
  # we verify that we finally have a dataframe
  if ( ! is.data.frame(df)) {
    df <- NULL
  }
  # if no dataframe set by default and one is available in global env, then we use it
  if (is.null(df)) {
    list_df <- names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
    ndf <- length(list_df)
    if (ndf == 1) {
      df <- get(list_df[1])
    }
  }
  df
}

# given a column name, finddf retrieve all df containing that column
# mainly used by getvar in short syntax
finddf <- function(varname) {
  .df <-
    names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame)))
  ndf <- length(.df)
  j <- 1
  nfound <- 0
  dffound <- ""
  dflist <- list()
  while (j <= ndf) {
    pat <- paste0("^",varname,"$")
    ifound <- grep(pat, names(get(.df[j])))
    if (length(ifound) > 0) {
      nfound <- nfound + 1
      dflist[nfound] <- .df[j]
      # list of dataset containing varname
      dffound <-
        paste0(dffound, ifelse(dffound == "", "", ", "), .df[j])
    }
    j <- j + 1
  }
  r <- list()
  r$count <- nfound
  r$namelist <- dflist
  r$namestring <- dffound
  return(r)
}


# retrieve the last data.frame found for last call to getvar system
getlastdf <- function() {

  df <- get_lastdfname()  # epif_env$last_df

  if ( is.character(df) ) {
    if (! df == "") {
      # dataset contain name ... then get the data.frame
      df <- get(df)
      # df <- eval(parse(text = df))
    }
  }
  # we verify that we finally have a dataframe
  if ( ! is.data.frame(df)) {
    df <- NULL
  }
  df
}



#' @title count number of record / row
#'
#' @description Count return the number of row sastifying a condition.
#'
#' @param expr A logical expression ora data.frame. If a data.frame or a vector is given,
#' countif() return the number of rows. If a logical expression is given, countif() return
#' the number of records satisfaying the condition
#' If a logical expression is given, the expression can use short column names (variable names)
#' as long as the default data.frame has been selected using setdata(thedataframe)
#'
#' @return Number of rows macthing the expression
#' @export
#'
#' @examples
#' countif(c(1, 2, 3, 1) == 1)
#'
countif <- function(expr) {
  # print(as.list(match.call()))
  m <- NA
  if (missing(expr)) {
    rex <- getdata()  # epif_env$dataset
  } else {
    rex <- eval_expr(substitute(expr))
  }

  # formula is correct, is it a data frame ?
  if (is.data.frame(rex)) {
    # we return number of row of the data.frame
    r <- dim.data.frame(rex)[[1]]
  } else if (is.logical(rex) ) {
    if (length(rex) == 1) {
       if (is.na(rex)) {
          r <- NA
       } else {
         r <- sum(rex, na.rm = TRUE)
       }
    } else {
      # ... dont't change anything and return sum of TRUE
      r <- sum(rex, na.rm = TRUE)
    }
  } else if (length(rex) > 1) {
    # it's a vector, we return it's length
    r <- length(rex)
  } else {
    r <- rex
  }

  # r <- try(eval(expr), TRUE)
  # if (inherits(r, "try-error")) {
  #   # it's not a correct formula ... we store the error
  #   m <- r[[1]]
  #   r <- NA
  #   # and try to do better by evaluating in the context of current dataset
  #   call <- as.call(list(sum, substitute(expr), na.rm = TRUE))
  #   env <- getdata()  # epif_env$dataset
  #   if (is.data.frame(env)) {
  #     # we evaluate the "sum" in that environnement
  #     m <- NA
  #     r <- try(eval(call, env, parent.frame()) , TRUE)
  #     if (inherits(r, "try-error")) {
  #       # still an error we report them
  #       m <- r[[1]]
  #       r <- NA
  #     }
  #   }
  # } else {
  #   # formula is correct, is it a data frame ?
  #   if (is.data.frame(expr)) {
  #     # we return number of row of the data.frame
  #     r <- dim.data.frame(expr)[[1]]
  #   } else if (is.logical(expr) ) {
  #     # ... dont't change anything and return sum of TRUE
  #     r <- sum(expr, na.rm = TRUE)
  #   } else if (length(expr) > 1) {
  #     # it's a vector, we return it's length
  #     r <- length(expr)
  #   } else {
  #     r
  #   }
  # }
  m <- last_error()
  if (!is.na(m)) {
    # should look for different error ... (not found ) / ( $ operator is invalid : "" are missing)
    red("Error :")
    pos <- regexpr("object '(\\w+)' not found", m )[1]
    normal(substring(m, pos ))
    cat("Considere to set the default data.frame using ")
    bold("setdata(name of data.frame)")
    normal("\n")
  }

  # the number of records found
  # we can't test NA because warning if length > 1 ...
  r

}

# Another possibility would be to complete expr with getdf automatically by looking for object
# avec un regexpr type
#  r <- regexpr("object '(\\w+)' not found", r[1],perl=TRUE)
#  p <- r[1]
#  l <- r[2]
# r<-try(eval(sum(heu == 2)))
# r[1] : Error in eval(sum(heu == 2)) : objet 'heu' introuvable
# substr(expr , heu ) <-  tira$heu


#' @title generate a new variable
#'
#' @description Generate a new variable into the default data.frame.
#' Default data.frame must be set using sedata(df) prior to use generate.
#'
#' @param name Name of the new variable/column to create
#' @param value The value to assign to the new variable.
#' if it's a logical expression. The new variable will contain TRUE or FALSE
#' according to the evaluation of the logical expression for each row
#' The new variable is added to the original dataset.
#'
#' @return The new variable
#' @export
#'
#' @examples
#' generate("test", "age/10")
#'
generate <- function(name, value) {
  dfname <- get_option("dataset")
  df <- getdata()  # epif_env$dataset
  name <- as.character(substitute(name))
  r <- NULL
  if (is.data.frame(df)) {
    # we evaluate the "value" in that environnement
    ex <- substitute(value)
    m <- NA
    r <- try(eval(ex, df, parent.frame()) , TRUE)
    if (inherits(r, "try-error")) {
      # still an error we report them
      m <- r[[1]]
      r <- NA
    } else {
      df[,name] <- r
      push.data(dfname,df)
    }
  } else {
    cat("To use generate, if you have more than one data frame in memory,",
         "then you should first set the default data.frame with sedata()")
  }

  invisible(r)
}


#' Title Drop a data.frame column
#'
#' @param varname The name of the column to be dropped
#'
#' @return Message to confirm
#' @export
#'
#' @examples
#' dropvar(dumepivar)
dropvar <- function(varname) {
  r <- as.list(match.call())

  vartodrop <- getvar(r$varname)
  if (! is.null(vartodrop) ) {
    # we collecte the data.frame and infos
    df <- getlastdf()
    vartodropname <- getvarname()
    dfname <- get_lastdfname()

    # we drop from df copy
    df[,vartodropname] <- NULL

    # feedback for user
    cat("Column ")
    bold(vartodropname)
    normal(" dropped from ")
    bold(dfname)
    catret("")

    # update original data.frame
    push.data(dfname,df)

  }
}

#' Title select part of a data.frame
#'
#' @param expr A logical condition applied to data.frame in order to select rows
#'
#' @return the selected vector
#' @export
#'
select <- function(expr) {
  sl <- get_option("select")
  dfname <- get_option("dataset")
  if (missing(expr) & ! (sl =="") ) {
    load("epifield.tmp", envir = .GlobalEnv)
    file.remove("epifield.tmp")
    set_option("select","")
    n <- nrow(getdata())
    cat("Selection cleared for", dfname, " :",n,"rows" )
  } else if (missing(expr)) {
    cat("No current selection to restore for",dfname)
  }
  if ( ! missing(expr) ) {
    ex <- substitute(expr)
    df <- getdata()
    dfname <- get_option("dataset")
    sl <- get_option("select")
    cursel <- deparse(ex)
    if (sl=="") {
      sl <- cursel
      save(list=c(dfname), file="epifield.tmp", envir = .GlobalEnv)
    } else {
      sl <- paste(sl, "&" , cursel)
    }
    set_option("select",sl)
    m <- NA
    r <- try(eval(ex, df, parent.frame()) , TRUE)
    if (inherits(r, "try-error")) {
      # still an error we report them
      m <- r[[1]]
      r <- NA
    } else {
      sdf <- df[r,]
      n <- nrow(sdf)
      push.data(dfname,sdf)
      cat(dfname,"Rows selected :", sl , "(",n,"rows)\n","Use select('') to clear the selection and to retrieve the original data " )
      invisible(sdf)
    }
  }
}

# right
#
# Extract x rigth characters from a text
#
# @param text Text to extract from
# @param num_char Number of char to extract from rigth
#
# @return  \code{num_char} extracted characters
# @examples
# \dontrun{
# right("dummy_test",4)
# }
#
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

colname <- function(what) {
  what <- deparse(what)
  lstop <- nchar(what)
  ldol <- regexpr("\\$",what)
  ldol <- ldol[1]
  if (ldol > 0) {
    what <- substr(what,ldol+1,lstop)
  }
  return(what)
}

lpad <- function(value,
                 width = 11,
                 digit = 0) {
  if (is.numeric(value) ) {
    r <-
      format(round(value, digits = digit),
             width = width ,nsmall = digit ,
             justify = "right")
  } else {
    r <-
      format(value, width = width , justify = "right")
  }
  if (is.character(value) & (nchar(r) > width)) {
    r <- paste0(substr(r, 1, width - 2), "..")
  }
  return(r)
}

#' Transform character values into date
#'
#' @param x Character value to transform into date
#' @param format Optionnal format
#' @param update If TRUE the default, the data.frame is updtade with new values
#' @importFrom utils head
#' @return the changed data
#' @export
#'
#' @examples
#' data(gastro)
#' chartodate(gastro$dob,"yy/m/d")
chartodate <- function(x,format="d/m/y",update=TRUE)  {
    r <- substitute(x)
    varx <- getvar(r)
    df <- getlastdf()
    dfname <- get_lastdfname()
    varname <- getvarname()
    varfname <- getvar()
    if (!is.null(varx)) {
      lvar <- sapply(varx,nchar)
      lvarm <- mean(lvar,na.rm=TRUE)
      fdate <- format
      if (lvarm>8) {
        if (charcount("y",fdate)==1) {
          fdate <- sub("y","yy",fdate)
        }
      }
      fdate <- sub("d","%d",fdate)
      fdate <- sub("m","%m",fdate)
      fdate <- sub("Y","%Y",fdate)
      fdate <- sub("yy","%Y",fdate)
      fdate <- sub("y","%y",fdate)
      dvar <- as.Date(varx,fdate)
      if (update & !is.null(df))  {
        df[,varname] <- dvar
        push.data(dfname,df)
      }
      invisible(dvar)
    }
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

bold <- function(...) {
  cat("\033[1m",...,sep="")
}

italic <- function(...) {
  cat("\033[3m",...,sep="")
}

red <- function(...) {
  cat("\033[31m",...,sep="")
}

normal <- function(...) {
  cat("\033[0m",...,sep="")
}





#'  @title Read a data.frame from disk into memory
#'
#'  Read a file of various format and return a data.frame.
#'  The function try to identify the file structure in order to call the appropriate specific
#'  command
#'  Currently accepted extension are : csv, dta, rec, rda
#'
#'  For csv files the first line is analysed to identify the separator.
#'  Accepted separator are  ,  or  ;
#'
#'  @return  The data.frame is returned (and should be assign to a variable)
#'
#' @export
#' @importFrom foreign read.dta
#' @param filename  Name of file to be read. Type is defined by extension
#' @param factorise Indicate if character variable should be read as factor. If false, the default
#' The column is read as character without transformation. Further transformation can be done
#' with factor base function or with epiorder function.
#' @param lowercase If TRUE, variable names are set to lowercase otherwise they are not changed
#' @param label Label to be added as attribute to data.frame. This label will be used as description
#' @param ...  other standard read options
#' @examples
#' fil <- tempfile(fileext = ".data")
#' cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
#' sep = "\n")
#' df <- read(filename=fil)
#' unlink(fil) # tidy up
#'

read <- function(filename = "", factorise = FALSE, lowercase= FALSE, label = NULL,...) {
  # no file ? choose one
  if (filename == "") {
    catret("retrieving file tree...please wait.")
    r <- try(filename <- file.choose())
    if (inherits(r, "try-error")) {
      # user have cancelled , stop now
      return(r)
    }
  }
  # try to extract name...
  ext <- tolower(file.ext(filename))
  name <- file.name(filename)
  if (file.exists(filename)) {
    # file exists.. let's go
    if (ext == "csv") {
      # look at the content
      # count and identify separator
      test <- readLines(filename , n = 2)
      comma1 <- charcount(",", test[1])
      semicol1 <- charcount(";", test[1])
      if (comma1 > 0) {
        df <- utils::read.csv(filename,as.is = !factorise,...)
      } else if (semicol1  > 0) {
        df <- utils::read.csv2(filename,as.is = !factorise,...)
      } else {
        red("Separator not identified in :")
        normal("\n")
        catret(test[[1]])
        catret(test[[2]])
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
    } else if (ext == "rda" | ext == "rdata" ) {
      # load return name and load content into selected env
      df <- load(filename)
      df <- get(df)
    } else {
      cat("Extension '", ext, "'not found")
    }
    if (!missing(label)) {
      attr(df, "label") <- label
    }
    if (is.data.frame(df)) {
      fileatt <- dim(df)
      if (lowercase) {
         names(df)<-casefold(names(df))
      }
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
#' If there only one object matching the exactly the \code{what} parameter, this object is removed whithout confirmation
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
    normal("  Use dropvar function")
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
      cat(l, " object(s) to remove :")
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

# exists look only in GlobalEnv and parent, is.var will search from current and parent until global but not in base
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
       set_option("last_isvar","")
       for (i in lsys:0)  {
          lc <- ls(sys.frame(i),pattern=what)
          if ( length(lc) > 0 ) {
             r=try(eval(parse(text = lc[1]), sys.frame(i)),TRUE)
             if (! inherits(r, "try-error")) {
              lsfound <- TRUE
              set_option("last_isvar",r)
             }
          }
       }
     }
   }
   lsfound
}




# internal function to retrieve dataset variables

#' @title retrieve a data.frame column
#'
#' @param what Name of the column
#'
#' @return The column
#' @export
#' @importFrom utils glob2rx
#'
#' @examples
#' getvar()
getvar <- function(what = NULL) {

  # first, if what is missing we return previous one
  if (missing(what)) {
    return(get_option("last_var"))
  } else {
    argpassed <- substitute(what)
    # should we look at var content ??
    # subst <- FALSE
    # if var is char content is used
    # if (exists(var)) {
    #   if (is.character(varname) & length(varname)== 1 ) {
    #      var<-eval(varname)
    #      subst<-TRUE
    #   }
    # }
    # reset of global vars
    resetvar()
    iscol <- FALSE
    dfname <- ""
    # Look at type of argument and get a working version of it
    r <- try(mwhat <- mode(what),TRUE)
    if (inherits(r, "try-error")) {
       what <- substitute(what)
    }
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
        varname <- deparse(argpassed)
      }
    )
    # got it, we save the name

    epif_env$last_var <- varname
    epif_env$last_varname <- varname
    if ( (l <-pos("\\$",varname)) > 0) {
      epif_env$last_varname <- substring(varname,l+1)
      epif_env$last_df <- substr(varname,1,l-1)
      iscol <- TRUE
    }

    # just create an expression with content
    ex <- parse(text=varname)

    if (is.var(varname)) {
      # if var exists it is returned as is
      # We evaluate in case what was passed with quote
      # because we evaluate an expression we must avoid evaluating local var !
      # then we start with parent and go ahead until .GlobalEnv
      return(get_option("last_isvar"))
      # lsys <- sys.nframe()-1
      # while (lsys >= 0) {
      #    r <- try(eval.parent(ex,lsys-1),TRUE)
      #    if (!inherits(r, "try-error")) {
      #      return(r)
      #      lsys <- -1
      #    } else lsys <- lsys-1
      # }
    } else {
      # var doesn't exist.. may be it's a formula ? We try to eval but we catch error
      continue <- FALSE
      r <- try(eval(ex), TRUE)
      if (!inherits(r, "try-error")) {
        # it's a formula ... it's evaluation is returned if not a function
        if ( ! mode(r) == "function" ) {
          if ( ! iscol ) {
            epif_env$last_df <- "formula"
          }
          return(r)
        } else {
          #  in that situation we can look for column name... to be modified
          warning(
            paste(
              varname ,
              "is probably not a variable but a function"),
            call. = FALSE
          )
        }
      } else continue <- TRUE
      if (continue) {
        # may be varname is part of a dataset ?
        dffound <- finddf(varname)
        # only one ? great
        if (dffound$count > 1) {
          dfset <- setdata()
          if (!dfset=="") {
            lset <- dfset %in% dffound$namelist
            if (lset) {
              dfname  <- dfset
            }
          }
        }
        if (dffound$count == 1) {
          dfname <- dffound$namelist[[1]]
        }
        if (!dfname=="") {
          varfullname <- paste(dfname, "$", varname , sep = "")
          # we update varname with data.frame value
          epif_env$last_var <- varfullname
          epif_env$last_varname <- varname
          epif_env$last_df <- dfname
          r <- try(eval(parse(text =varfullname)),TRUE)
          return(r)
        } else if (dffound$count > 1){
            warning(
              paste0(
                varname ,
                " is an ambiguous name and exists in following datasets: ",
                dffound$namestring,"\n","You could try ",dffound$namelist[[1]],"$",varname,
                "\n or try to use setdata(",dffound$namelist[[1]],")"
              ),
              call. = FALSE
            )
            resetvar()
            return(NULL)
        } else {
          warning(paste(varname , "is not defined as variable or data.frame column"), call. = FALSE)
          return(NULL)
        }
      }
    } # var not exists
  } # not missing
}


tab_line <- function(ncol, tot = FALSE, first=FIRST) {
  l1 <- replicate(LINE, first + 1)
  l2 <- replicate(LINE, (ncol - 1) * (COL + 2))
  l3 <- ifelse(tot, CROSS, LINE)
  l4 <- replicate(LINE, COL )
  cat(l1, CROSS, l2, l3, l4, "\n", sep = "")
}

tab_row <- function(rname, line, deci=0, tot = FALSE, coldeci=NULL, indic=NULL, first=FIRST) {
  l <- length(line)
  if (is.null(coldeci)) {coldeci[1:l] <- FALSE}
  cat(lpad(rname, first))
  cat("", SEP)
  for (i in 1:(l - 1)) {
    ndigit <- ifelse(coldeci[i],deci,0)
    fout <- lpad(line[[i]], COL, digit = ndigit)
    cat(fout, " ")
  }
  if (tot) {
    if (!is.null(indic)) {
      cat(indic)
    } else {
      cat(SEP)
    }
  }
  ndigit <- ifelse(coldeci[l],deci,0)
  cat(lpad(line[[l]], COL, ndigit ))
  cat("\n")
}


outputtable <-
  function(table,
           deci = NULL,
           totcol = FALSE,
           totrow = TRUE,
           title = "Frequency distribution",
           rowperc = NULL,
           colperc=NULL,
           coldeci=NULL,
           first=FIRST)  {
    catret(title)
    catret("")
    ncol <- dim(table)[2]
    nline <- dim(table)[1]
    coln <- colnames(table)
    rown <- rownames(table)

    if (is.null(coldeci)) {
      coldeci[1:ncol] <- FALSE
    }

    # columns title
    if (! is.null(names(dimnames(table))[2]) ) {
      catret(replicate(" ",COL*(ncol/2)+FIRST ), names(dimnames(table))[2])
    }

    # rows title and columns names
    name <- names(dimnames(table))[1]
    if (is.null(name))  name <- ""
    tab_row(name, coln, deci, totcol, coldeci,first=first)

    # separator line
    tab_line(ncol, totcol,first=first)

    percdeci<-NULL
    percdeci[1:ncol-1] <- TRUE
    percdeci[ncol] <- FALSE

    # each row
    totline <- nline
    if (totrow) {totline <- nline - 1}
    for (i in (1:(totline))) {
      tab_row(rown[i], table[i, ], deci, totcol,coldeci,first=first)
      if ( ! is.null(rowperc) ) {
        tab_row("", rowperc[i, ], deci, totcol,percdeci,indic=">",first=first)
      }
      if ( ! is.null(colperc) ) {
        tab_row("", colperc[i, ], deci, totcol,percdeci,indic="V",first=first)
      }
    }

    # separator line
    tab_line(ncol, totcol,first=first)
    # Totals row
    if (totrow) {
      tab_row(rown[nline], table[nline, ], deci, totcol, coldeci,first=first)
      if ( ! is.null(colperc) ) {
        tab_row("", colperc[nline, ], deci, totcol,percdeci,indic="V",first=first)
      }
    }
  }


insertrow <- function(DFtoadd, newrow, r) {
  DFtoadd  <- rbind(DFtoadd[1:r-1], newrow, DFtoadd[-(1:r-1)])
}

#' Change the name of a data.frame column
#'
#' @param oldname Name of the column/variable to rename
#' @param newname New name to apply
#'
#' @return Message to confirm the change
#' @export
#'
#' @examples
#' df <- as.data.frame( c(One=1,Two=2) )
#' rename(Two,Last)
rename <- function(oldname, newname) {
  r <- as.list(match.call())
  old <- getvar(r$oldname)
  if (! is.null(old) ) {
    old.fname <- getvar()
    old.name <- getvarname()
    dfname <- get_lastdfname()
    df <- getlastdf()

    newname <- as.character(substitute(newname))
    lname <- names(df)
    lname[lname==old.name] <-  newname
    names(df)<-lname
    push.data(dfname,df)

    normal(" ")
    bold(old.fname)
    normal(" renamed as ")
    bold(newname)
    normal("")
    catret("")
  }

}



#' @title Replacement for |
#'
#' @description  %or% can be used everywhere | is used.
#' To be tested
#'
#' @details
#' it's mainly for those whitout | on keyboard
#'
#' @export
#' @param a logical or condition
#' @param b logical or condition
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality, available at \url{https://github.com/}.
#' @examples
#' TRUE %or% FALSE
#'
#'
`%or%` <- function(a, b) {return(a|b) }


#' @title Replacement for &
#'
#' @description  %and% can be used everywhere & is used.
#' To be tested
#'
#' @details
#' it's mainly for those whitout & on keyboard
#'
#' @export
#' @param a logical or condition
#' @param b logical or condition
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality, available at \url{https://github.com/}.
#' @examples
#' TRUE %and% TRUE
#'
`%and%` <- function(a, b) { return(a&b) }


#
# KHI2 <- function(A, B, C, D)
# {
#   t <- chisq.test(matrix(c(A,B,C,D),ncol=2), correct=FALSE);
#   return(c(t$statistic, t$p.value));
# }
#
# computeFisher <- function(A, B, C, D)
# {
#   t <- fisher.test(matrix(c(A,B,C,D),ncol=2));
#   t$p.value
# }
