# Epifield data management function source
#
# Data management functions designed to modify data.frame
#
# Objective is to produce a very simplified set of command
# for field epidemiologist who are familiar with Epi7 or Stata
#
#   http://
#
# Author : Gilles Desve & al...
#




#' Title recode a var if a condition if true
#'
#' @param var The vector or data.frame column to recode
#' @param value The value to assign
#' @param condition If the condition is verified
#'
#' @return The recode variable
#' @export
#'
#' @examples
#' recode.if(age, "young", age < 18)
recode.if  <- function(var, value, condition) {
  r <- as.list(match.call())
  vartorec <- getvar(r$var)
  vartorecname <- getvarname()
  cond <- r$condition
  condtext <- deparse(cond)
  df <- getdf()
  dfname <- get_option("last_df")
  recoded <- FALSE
  # test dfname or use error result to find it


  condresult <- eval_expr(condtext,df)
  continue <- TRUE

  if (is.null(condresult)) {
    catret("Logical expression not valid : " , condtext)
    continue <-FALSE
  }
  if ( length(condresult) == 1 ) {
    if ( is.na(condresult) ) {
      continue <- FALSE
      catret("NA returned for: (" , condtext,").", dfname, "unchanged" )
    } else if (is.character(condresult)) {
      continue <- FALSE
      catret("(" , condtext,") doesn't return a logical value.", dfname, "unchanged" )
    }
  }
  if ( continue ) {
    nrec <- sum(condresult)
    if (nrec >0 ) {
      df[condresult,vartorecname] <- value
      recoded <- TRUE
      catret(nrec, "record(s) recoded as",value, "in",dfname, "for ", condtext )
    } else {
      catret("No record for: (" , condtext,").", dfname, "unchanged" )
    }
  }
  if (recoded) {
    push.data(dfname,df)
  }
  invisible(df[,vartorecname])
}


#' Title recode a var if a condition if true
#'
#' @param var The vector or data.frame column to recode
#' @param oldvalue The value to assign
#' @param newvalue The value to replace
#'
#' @return The recode variable
#' @export
#'
#' @examples
#' recode.value(case, "Oui", 1)
recode.value  <- function(var, oldvalue, newvalue) {
  r <- as.list(match.call())
  vartorec <- getvar(r$var)
  vartorecname <- getvarname()
  # we use old value asis to construct the condition
  oldv <- deparse(r$oldvalue)
  newvalue <- r$newvalue
  newvaluetext <- deparse(newvalue)
  recoded <- FALSE
  df <- getdf()
  dfname <- get_option("last_df")

  # test dfname or use error result to find it
  condtext <- paste0(vartorecname,"==",oldv)
  condresult <- eval_expr(condtext,df)

  continue <- TRUE

  if (is.null(condresult)) {
    catret("Logical expression not valid : " , condtext)
    continue <-FALSE
  }
  if ( length(condresult) == 1 ) {
    if ( is.na(condresult) ) {
      continue <- FALSE
      catret("NA returned for: (" , condtext,").", dfname, "unchanged" )
    }
  }
  if ( continue ) {
    nrec <- sum(condresult)
    if (nrec >0 ) {
      if ( is.factor(vartorec) ) {
         # we change only the labels...
        nl <- nlevels(vartorec)
        lev = levels(vartorec)
        for (i in 1:nl) {
           if (lev[[i]]==oldvalue) {
              lev[i] <- newvalue
           }
        }
        recoded <- TRUE
        levels(vartorec) <- lev
        df[,vartorecname] <- vartorec
      } else {

        newcolexpr <- paste0("ifelse(",condtext,",",newvaluetext,",df$",vartorecname,")")
        df$newvar <- as.factor()
        df$newvar <- eval_expr(newcolexpr, df)

        df[,vartorecname] <- df$newvar

        # expr <- paste0(dfname,'[',condtext,',"',vartorecname,'"] <- ',newvaluetext )
        # r <- try(eval(expr, df, .GlobalEnv) , TRUE)
        # df[condresult,vartorecname] <- newvalue
        recoded <- TRUE
        catret(nrec, "record(s) recoded as",newvalue, "in",dfname, "for ", condtext )
      }
    } else {
      catret("No record for: (" , condtext,").", dfname, "unchanged" )
    }
  }
  if (recoded) {
     push.data(dfname,df)
  }
  invisible(df[,vartorecname])
}



#' Title  eval an expression looking at default dataset
#'
#' @param expr  An expression to be evaluated
#' @param env   An otionnal environnement to evaluate expr
#'
#' @return  Result of expression evaluation
#' @export
#'
#' @examples
#' eval_expr(5>4)
eval_expr <- function(expr,env)  {

  r <- try(mexpr <- mode(expr),TRUE)
  if (inherits(r, "try-error")) {
    expr <- substitute(expr)
  }
  mexpr <- mode(expr)
  switch(mexpr ,
         "character" = {
           ex <- expr
         } ,
         "call" =  {
           ex <- deparse(expr)
         } ,
         "name" = {
           ex <- as.character(expr)
         } ,
         { # else
           ex <- deparse(substitute(expr))
         }
  )
  r <- try(eval(parse(text=ex)), TRUE)
  m <- NA
  if (inherits(r, "try-error")) {
    # it's not a correct formula ... we store the error
    # and we may try to extract the var in error and find them
    m <- r[[1]]
    r <- NA
    # if env is not specified we try to get the default dataset (see setdata())
    if (missing(env)) { env <- getdata() }
    if (is.data.frame(env)) {
      # and try to do better by evaluating in the context of current dataset
      m <- NA
      r <- try(eval(parse(text=ex), env, parent.frame()) , TRUE)
      if (inherits(r, "try-error")) {
        # still an error we store them
        m <- r[[1]]
        r <- NA
      }
    }
  }
  if (! is.na(m)) {
    catret(m)
  }
  # we return the result
  r
}

