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
#' @param oldvalue The value to replace
#' @param newvalue The value to assign
#'
#' @return The recode variable
#' @export
#'
#' @examples
#' recode.if(age, "Unknow", NA)
recode.value  <- function(xvar, oldvalue, newvalue) {
  r <- as.list(match.call())
  vartorec <- getvar(r$xvar)
  vartorecname <- getvarname()
  oldv <- deparse(r$oldvalue)
  # test dfname or use error result to find it
  condtext <- paste0(vartorecname,"==",oldv)
#  vartorecname <- deparse(vartorecname)
  # this part is specific to recode value ...
  ifexpr <- paste0("recode.if(",vartorecname,",",condtext,",",newvalue,")")
  r <- eval(parse(text=ifexpr) )
    # recode.if(vartorecname,condtext, newvalue)
  r
  # df <- getdf()
  # dfname <- get_option("last_df")
  # recoded <- FALSE
  # # test dfname or use error result to find it
  #
  #
  # condresult <- eval_expr(condtext,df)
  # continue <- TRUE
  #
  # if (is.null(condresult)) {
  #   catret("Logical expression not valid : " , condtext)
  #   continue <-FALSE
  # }
  # if ( length(condresult) == 1 ) {
  #   if ( is.na(condresult) ) {
  #     continue <- FALSE
  #     catret("NA returned for: (" , condtext,").", dfname, "unchanged" )
  #   } else if (is.character(condresult)) {
  #     continue <- FALSE
  #     catret("(" , condtext,") doesn't return a logical value.", dfname, "unchanged" )
  #   }
  # }
  # if ( continue ) {
  #   nrec <- sum(condresult)
  #   if (nrec >0 ) {
  #     df[condresult,vartorecname] <- value
  #     recoded <- TRUE
  #     catret(nrec, "record(s) recoded as",value, "in",dfname, "for ", condtext )
  #   } else {
  #     catret("No record for: (" , condtext,").", dfname, "unchanged" )
  #   }
  # }
  # if (recoded) {
  #   push.data(dfname,df)
  # }
  # invisible(df[,vartorecname])
}

#
# if ( is.factor(vartorec) ) {
#   # we change only the labels...
#   nl <- nlevels(vartorec)
#   lev = levels(vartorec)
#   for (i in 1:nl) {
#     if (lev[[i]]==oldvalue) {
#       lev[i] <- newvalue
#     }
#   }
#   recoded <- TRUE
#   levels(vartorec) <- lev
#   df[,vartorecname] <- vartorec
# } else {
#




#' Title recode a var if a condition if true
#'
#' @param var The vector or data.frame column to recode
#' @param condition Logical expression to select value to be recoded
#' @param newvalue The value to replace
#'
#' @return The recoded variable
#' @export
#'
#' @examples
#' recode.if(age, age > 100, 100)
recode.if  <- function(xvar, condition, newvalue) {
  r <- as.list(match.call())
  vartorec <- getvar(r$xvar)
  vartorecname <- getvarname()
  vartorecfname <- getvar()

  r <- try(mcond <- mode(condition),TRUE)
  if (inherits(r, "try-error")) {
    condtext <- substitute(condition)
    condtext <- deparse(condtext)
  } else if (mode(condition)=="character") {
    condtext <- condition
  } else {
    condtext <- deparse(condition)
  }
  newvaluetext <- deparse(newvalue)
  recoded <- FALSE
  df <- getdf()
  dfname <- get_option("last_df")

  condresult <- eval_expr(condtext,df)

  continue <- TRUE
  newvar <- vartorec

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
      newcolexpr <- paste0("ifelse(",condtext,",",newvaluetext,",",vartorecfname,")")
      newvar <- eval_expr(newcolexpr, df)
      if (is.data.frame(df)) {
         df[,vartorecname] <- newvar
         recoded <- TRUE
      }
      # expr <- paste0(dfname,'[',condtext,',"',vartorecname,'"] <- ',newvaluetext )
      # r <- try(eval(expr, df, .GlobalEnv) , TRUE)
      # df[condresult,vartorecname] <- newvalue
      catret(nrec, "record(s) recoded as",newvaluetext, "in",vartorecfname, "for", condtext )

    } else {
      catret("No record for: (" , condtext,").", vartorecfname, "unchanged" )
    }
  }
  if (recoded) {
     push.data(dfname,df)
     invisible(newvar)
  } else {newvar}

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
  last_error(NA)
  if (inherits(r, "try-error")) {
    # it's not a correct formula ... we store the error
    # and we may try to extract the var in error and find them
    m <- r[[1]]
    last_error(m)
    r <- NA
    # if env is not specified we try to get the default dataset (see setdata())
    if (missing(env)) { env <- getdata() }
    if (is.data.frame(env)) {
      # and try to do better by evaluating in the context of current dataset
      m <- NA
      last_error(m)
      r <- try(eval(parse(text=ex), env, parent.frame()) , TRUE)
      if (inherits(r, "try-error")) {
        # still an error we store them
        m <- r[[1]]
        last_error(m)
        r <- NA
      }
    }
  }
  # if (! is.na(m)) {
  #  catret(m)
  # }
  # we return the result
  r
}

