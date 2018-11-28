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

# cryptoagr =aggregate(crypto[,c("count","Year")],by=list(Region=crypto$Region,Sex=crypto$Sex), FUN=sum)
# cryptoagr =aggregate(crypto$count,by=list(Year=crypto$Region), FUN=sum)
# cryptoagg <- aggregate(numberofcases ~ month, FUN = sum, data = crypto2015,na.rm=TRUE)

#' Title
#'
#' @param countvar Variable to be aggregated
#' @param byvar Indicator for wich sum will be calculated
#' @param byvar2 Second Indicator for wich sum will be calculated
#' @param byvar3 Third Indicator for wich sum will be calculated
#'
#' @return aggregated data
#' @export
#'
#' @examples
#' df <- data.frame("count"=c(4,5),"year"=c(2015,2016))
#' sumby(count,year)
sumby <- function(countvar,byvar,byvar2,byvar3 ) {
  r <- as.list(match.call())

  getvar(r$countvar)
  varcount <- getvar()
  # dfname <- get_option("last_df")
  getvar(r$byvar)
  varby <- getvar()
  if (!missing(byvar2)) {
    getvar(r$byvar2)
    varby2 <- getvar()
    colvar2 <- paste(",",deparse(getvarname() ))
    varby2 <-paste(",", varby2)
  } else {varby2<-"";colvar2<-""}
  if (!missing(byvar3)) {
    getvar(r$byvar3)
    varby3 <- getvar()
    colvar3 <- paste(",",deparse(getvarname() ) )
    varby3 <-paste(",", varby3)
  } else {varby3<-"";colvar3<-""}

  texpr <- paste("aggregate(",varcount,",by=list(",varby,varby2,varby3,"),FUN=sum)" )
    # this doesn't allow vector as parameters
    # texpr <- paste("aggregate(",r$countvar,"~",r$byvar,",FUN=sum, data =",dfname, ")" )
  res = eval(parse(text=texpr))
  byvar <- colname(r$byvar)
  countvar <- colname(r$countvar)
  col.names <- paste("c(",deparse(byvar),colvar2,colvar3,',',deparse(countvar),')')
  colnames(res) <- eval(parse(text=col.names))
  # r <- aggregate(countvar ~ byvar, FUN = sum, data = df,na.rm=TRUE)
  res
}


#' Title recode a var if a condition if true
#'
#' @param xvar The vector or data.frame column to recode
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

  if ( is.factor(vartorec) ) {
    # we change only the labels...
    df <- getdf()
    dfname <- get_option("last_df")
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
    push.data(dfname,df)
    vartorec
  } else {
    oldv <- deparse(r$oldvalue)
    # test dfname or use error result to find it
    condtext <- paste0(vartorecname,"==",oldv)
  #  vartorecname <- deparse(vartorecname)
    # this part is specific to recode value ...
    ifexpr <- paste0("recode.if(",vartorecname,",",condtext,",",newvalue,")")
    r <- eval(parse(text=ifexpr) )
      # recode.if(vartorecname,condtext, newvalue)
    invisible(r)
  }
}

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


#' Title
#'
#' @param xvar The numeric variable to recode
#' @param tovar Optionnaly, the new variable to create in the data frame
#' @param by Either a numeric representing the size of each categorie
#'        or a vector with the cutoff
#' @param update if TRUE, the default then original data.frame is updated
#'
#' @return The new variable
#' @export
#'
#' @examples
#' recode.by(gastro$age,agegr,by=10)
recode.by  <- function(xvar, tovar="", by=10 , update = TRUE) {
  r <- as.list(match.call())
  vartorec <- getvar(r$xvar)
  if (is.null(vartorec)) {
    stop()
  }
  vartorecname <- getvarname()
  vartorecfname <- getvar()
  df <- getdf()
  dfname <- get_option("last_df")
  labellist <- NULL
  if (is.numeric(vartorec)) {
    if (length(by)==1) {
      minv <- by * min(vartorec)%/%by
      maxv <- max(vartorec)
      maxv <- maxv + by - maxv%%by
      breakslist <- seq(minv,maxv,by)
      cutint <-function(x,by) {
        t <- paste0(as.character(x),"-",as.character(x+by-1) )
      }
      labellist <- sapply(breakslist[-length(breakslist)],cutint,by)
    } else breakslist <- by

    # cut_interval
    groupvar <- cut(vartorec,breaks=breakslist, labels=labellist)
    tovar <- substitute(tovar)
    if (update) {
      if (is.data.frame(df)) {
        if (! tovar=="") {
          cn <- colnames(df)
          cn <- c(cn,tovar)
          df <- cbind(df,groupvar)
          colnames(df) <- cn
        } else {
          df[,vartorecname] <- groupvar
        }
        push.data(dfname,df)
      }
    }

  } else {
    catret("recode.by only accept numeric vector")
  }

  invisible(groupvar)
}

#' Title recode a var if a condition if true
#'
#' @param xvar The vector or data.frame column to recode
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
    nrec <- sum(condresult,na.rm = TRUE)
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
  } else invisible(newvar)

}



# Title  eval an expression looking at default dataset
#
# @param expr  An expression to be evaluated
# @param env   An otionnal environnement to evaluate expr
#
# @return  Result of expression evaluation
# @export
#
# @examples
# eval_expr(5>4)
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

#' @title  List data.frame column
#'
#' @param df A data.frame
#'
#' @return Structure of data.frame column
#' @export
#'
#' @examples
#' describe(test)
describe <- function(df) {
  fileatt <- dim(df)
  dfn <- substitute(df)
  catret(fileatt[1],
      "Observations of ",
      fileatt[2],
      " variables. Use str(",dfn,") for details")

  r1 <- colnames(df)
  ldf <- length(r1)
  r2 <- sapply(df,class)
  r3 <- attr(df,"var.labels")
  if (is.null(r3)) {
    r3 <- vector("character",length = ldf)
  }
  r4 <- sapply(df,levels)
  r5 <- vector("character",length = ldf)
  for (i in 1:ldf) {
    # toString to be tested ...
    r5[i] <- paste(r4[[i]],sep = " ",collapse= ",")
  }
  r <- cbind(type=r2,desc=r3,labels=r5)
  r

}

#' @title  List head of data.frame allowing to select column to view
#'
#' @param ... variable name to include in list
#' @param nline Number of rows to show (default to n) If negative then number of row from the end
#'
#' @return the resulting list
#' @export
#'
#' @examples
#' \dontrun{
#' view(age,case)
#' view(age,case, nline=5)
#' view(age,case, nline= -10)
#' }

view <- function(...,nline=10) {
  r <- as.list(match.call())
  n <- names(r)
  i <-pmatch("nline",n, nomatch = 0)
  if (i > 0) {
    r[i] <- NULL
  }
  l <- length(r)
  c <- vector()
  j <- 0
  for (i in 2:l) {
    j <- j+1
    c[j]<- as.character(r[i])
  }
  df <- getdata()
  if (nline > 0) {
     res <- df[1:nline,c]
  } else {
    l <- nrow(df)
    nline <- l + nline
    res <- df[nline:l,c]
  }
  res
}


