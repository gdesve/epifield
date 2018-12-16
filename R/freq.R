# epifield documentation for RData using roxygen2
#' @title
#' Frequency distribution.
#' @description
#' \code{freq} Display a frequency distribution.
#' #'
#'
#' @name freq
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{table}} for 2by2 tables
#' @export
#' @param ... As numbers, factors or text.
#' @param missing If false then missing values are not included in the table
#'   A summary output of number of missing values is added at the end
#' @param quietly No output, only return value
#' @return An array containing  values of \code{...}   \code{}
#'
#' @examples
#' freq(c(3,1,2,2,5))
#'
#'
freq <- function(...,missing=FALSE,quietly = FALSE) {
  arg.passed <- substitute(...)

  cur.var <- getvar(arg.passed)
  if (! is.null(cur.var)) {
    count <- table(cur.var, useNA=ifelse(missing,"ifany","no") )
    tot <- length(cur.var)
    prop <- round(prop.table(count)*100, digits = 2)
    cum <- cumsum(prop)
    result <- cbind(count,
                    prop,
                    cum)
    mis  <- sum(is.na(cur.var))
    var.name <- get_option("last_varname")
    colnames(result) <- c("Freq", "%" , "cum%")
    result <- rbind(result,Total = colSums(result))
    cdeci <- c(FALSE,TRUE,TRUE)
    deci <- 1
    result[nrow(result),ncol(result)] <- 100
    title <- paste("Frequency distribution of",getvar())
    names(dimnames(result))  <- c(var.name,title)
    if (! quietly) {
       outputtable(result,deci,totcol=FALSE,title=title,coldeci=cdeci )
    }
    # missing should be added to result
    if (! missing) {
      cat("Missing (NA):",mis," (",round(mis/tot*100, digits = 2),"%)\n")
    }
    # construct of returned list
    r <- list()
    r$table <- result
    r$total <- tot
    r$missing <- mis
    invisible(r)
  }
}


# epifield documentation for RData using roxygen2
#' @title
#' Cross tabulation ( 2by2 table).
#' @description
#' \code{epitable} Display a cross tabulation of two variables optionnaly with
#'  row or col percentages. Chi Square with associated p.value are calculated.
#'  If table contain binary variable, then epiorder function is apply on the two variable
#'  to get a resulting table compatible with usual epidemiology interpretation.
#'  0/1 variables are transformed into Yes/No and Yes is displayed before No
#'  Exposed Cases appear on upper left part of the table.
#'
#'
#' @name epitable
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{freq}} for frequency distributions
#' @importFrom stats chisq.test fisher.test
#' @export
#' @param exp  "Exposure" as numbers, factors or text. short syntax is available
#' see help(epifield)
#' @param out  "Outcome" as numbers, factors or text
#' @param missing Boolean if FALSE, missing are not included in the table.
#'   A summary output of number of missing values is added at the end
#' @param row  "Row percentages"
#' @param col  "Col percentages"
#' @param fisher TRUE by default, display the fisher exact probability.
#' If table is larger than 2*2 then Fisher is not calculated
#' @return An array containing  values of \code{...}   \code{}
#' @examples
#' #' epitable(c(1,1,2,2,1),c(3,3,4,4,4))
#'
epitable <- function(out,exp,missing=FALSE,row=FALSE,col=FALSE,fisher=TRUE)  {
   r <- as.list(match.call())
   expdata <- getvar(r$exp)
   expdata.name <- getvarname()
   expdata.fname <- getvar()
   if (! is.null(expdata)) {
    expdata <- epiorder(expdata,update=FALSE,reverse=TRUE )
   }

   tot <- length(expdata)

   outdata <- getvar(r$out)
   outdata.name <- getvarname()
   outdata.fname <- getvar()
   if ( ! is.null(outdata)) {
    outdata <- epiorder(outdata,update=FALSE, reverse=TRUE)
   }
   # length to be verified

   # to get options
   params <- names(r)

   if (! ( is.null(expdata) |  is.null(outdata) )  ) {


     # calculations
     r <- table(expdata,outdata,useNA=ifelse(missing,"ifany","no"))
     # to suppress the chisq warning if table is more than 2*2
     options("warn"=-1)
     t <- chisq.test(r)
     options("warn"=0)
     # check size of result table
     bin <- (dim(r)[1]==2 & dim(r)[2]==2)
     if (bin & fisher) {
       f <- fisher.test(r)$p.value
     } else {fisher <- FALSE}
     proprow <- NULL
     propcol <- NULL
     if (row) {
       proprow <- round(prop.table(r,1)*100, digits = 2)
       proprow <- cbind(proprow,100)
     }
     if (col) {
       propcol <- round(prop.table(r,2)*100, digits = 2)
       propcol <- cbind(propcol,"")
       propcol <- rbind(propcol,100)
     }

     m <- margin.table(r,1)
     r <- cbind(r,Total = m)
     m <- margin.table(r,2)
     r <- rbind(r,Total = m)

     # must be done after all structure changes
     names(dimnames(r))  <- c(expdata.name,outdata.name)

     mis  <- sum(is.na(expdata)|is.na(outdata))

     title <- paste("Tabulation of",outdata.fname,"by",expdata.fname)

     outputtable(r, deci=1, totcol=TRUE, title=title, rowperc = proprow , colperc = propcol )

     # construct the return list
     result <- list()
     result$table <- r
     result$rowperc <- proprow
     result$colperc <- propcol

     result$chisq <- t$statistic[[1]]
     result$chisq.p <- t$p.value
     result$fischer <- t$p.value
     result$missing <- mis

     # print stat result for interactive mode
     catret("")
     cat("Chi2:",t$statistic,"(", fmtpval(t$p.value,digits = get_option("stat_digits")),")" )
     if (fisher) {

        cat(" Fisher exact :",fmtpval(f,digits = get_option("stat_digits")))
     }
     catret("")
     if (!missing) {
       catret("Missing (NA):",mis," (",round(mis/tot*100, digits = 2),"%)\n")
     }
     invisible(result)
   }
}


#' sumstats
#'
#' Produce a summary
#'
#' @param what Variable to be analysed
#' @param cond Optionnal logical expression to filter data
#'
#' @return summary
#' @export
#'
#' @examples
#' sumstats(gastro$age)
#'
sumstats <- function(what,cond) {
  r <- as.list(match.call())
  coldata <- getvar(r$what)
  colfullname <- getvar()
  if (! missing(cond)) {
    tcond <- deparse(substitute(cond))
    expr = paste0(colfullname,"[",tcond,"]")
    df <- getdf()
    coldata <- eval_expr(expr,df)
  }
  if (!is.null(coldata)) {
    catret("Summary for",colfullname)
    summary(coldata)
  }

}



# epifield documentation using roxygen2
#' @title
#' Reorder data for epi table ( 2by2 table).
#' @description
#' \code{epiorder} Rearrange order of factor variable to produce classical epi table
#'  1/0  Yes/No  +/-
#'
#'
#' @name epiorder
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{epitable}} for cross tabulation
#' @export
#' @param var  Variable to reorder (will be converted as factor).
#' @param mode Label plan for the new factor variable
#'         "yesno" for Yes, No
#'         "10"    for 1, 0
#'         "+-"    for +,-
#'         "truefalse"  for TRUE, FALSE
#'         or any set of two labels on the form c("A","B")
#' @param levels  Custom set of levels as vector : c(1,0)
#'        This levels will replaced by the lables then levels and labels should have the same
#'        length and the same order
#' @param update if TRUE (the default) Then the original dataset is updated with new value
#'        The recoded column is replaced by the returned value
#'        With this option, there is no need to reassign the retruned value,
#'        but original data are lost.
#' @param reverse if TRUE labels are reordered to better fit epidemiological tables
#'        with 1 before 0 , Yes before No etc...
#'        Other type of label are not changed, existing factor are not changed
#'
#' @return A vector reordered Can be nested as in \code{freq(epioredr(case))}
#' @examples
#' \dontrun{
#' epiorder(c(0,1,0,1,1))
#' }
#'
#'
epiorder <- function(var,
                     mode = "yesno",
                     levels = NULL,
                     update = TRUE,
                     reverse = FALSE) {
  r <- as.list(match.call())
  coldata <- getvar(r$var)
  colname <- getvarname()
  colfullname <- getvar()
  lab <- NULL
  # colname <- as.character(substitute(var))
  if (!is.null(coldata)) {
    if (length(mode) > 1 & is.character(mode)) {
      lab <- mode
    } else {
      switch(
        mode ,
        "yesno" = {
          lab <- c("No","Yes")
        } ,
        "auto" = {
          lab <- ""
        } ,
        "10" = {
          lab <- c("0","1")
        } ,
        "+-" = {
          lab <- c("-","+")
        } ,
        "truefalse" = {
          lab <- c("FALSE","TRUE")
        } ,
        {
          cat("Mode:", mode, " Incorrect. See help(epiorder)")
          lab <- NULL
        }
      )
    }

    if (!is.null(lab)) {
      dfname <- get_option("last_df")
      if (!dfname == "") {
        df = get(dfname)
      }

      fvar <- is.factor(coldata)
      if ( fvar ) {
          if (is.null(levels)) {
            clevels <- levels(coldata)
            nlevels <- nlevels(coldata)
            if (nlevels == 2 & substr(toupper(sort(clevels)[1]),1,1) == "N" )  {
               clevels <- clevels
            } else {
               lab <- NULL
            }
          } else {
            clevels <- levels
            reverse <- FALSE
          }
      } else {
        coldata <- factor(coldata)
        # test for type of levels  (otherwise calling it two time will erase all values)
        clevels <- levels(coldata)
        nlevels <- nlevels(coldata)
        if (is.null(levels)) {
          if (nlevels == 2 ) {
            first <- sort(clevels)[1]
            if (first == "0") {
              clevels <- c(0,1)
            } else if ( substr(toupper(sort(clevels)[1]),1,1) == "N" ) {
              clevels <- sort(clevels)
            } else {
              lab <- NULL
            }
          } else if (nlevels > 2) {
            if (mode == "yesno" ) {
              # we keep base factor
              lab <- NULL
            } else if (nlevels == length(mode) & nlevels == length(lab)) {
              # we use the levels
            } else lab <- NULL
          } else {
            catret("Check your data to verify that you can transform",
                   colname,
                   "as factor")
            coldata <- NULL
          }
        } else {
          clevels <- levels
        }
        if (!nlevels == length(clevels)) {
          catret(
            "Check your data to verify that number of categories is correct and match your recoding"
          )
          coldata <- NULL
        }
        if (!is.null(lab)) {
          if (!length(lab) == length(clevels)) {
             catret("Numbers of categories/levels must be equal to number of labels")
             coldata <- NULL
          }
        }
      }
    }
  }
  if (!is.null(coldata)) {
    if (!is.null(lab)) {
      if (reverse) {
         clevels <- rev(clevels)
         lab <- rev(lab)
      }
      coldata <-
        factor(coldata,
               levels = clevels ,
               labels = lab)
    }
    if (update & is.data.frame(df)) {
      df[, colname] <- coldata
      # assign(dfname,df,inherits = TRUE )
      push.data(dfname, df)

      bold(colfullname)
      normal(" Reordered with labels: ")
      catret(levels(coldata))

    }
      # exp <- paste0(substitute(var),"<- coldata")
      # r <- try(evalq(parse(text = exp), envir = df, enclos = .GlobalEnv),TRUE)
      # r
      # df
    invisible(coldata)
  } else {
    catret(r$var," is not a variable or data.frame column")
  }

}


fmtpval <-function(pvalue,digits) {
  res <- round(pvalue,digits)
  if (res==0) {
    res <- paste0("< ",format(1/10^digits,scientific=FALSE) )
  }
  res
}



#' Title
#'
#' @param xvar  The variable to unfactor
#' @param levels The desired levels as numeric values given in the order where
#' label appear
#' @param update if TRUE, the default, the data.frame is automaticaly updated
#' with the new numeric variable
#'
#' @return The unfactored variable
#' @export
#'
#' @examples
#' tvar<- c("Yes","No")
#' tvar <- factor(tvar)
#' unfactor(tvar)
unfactor <- function(xvar , levels, update =TRUE) {
  r <- as.list(match.call())
  vartorec <- getvar(r$xvar)
  if (is.null(vartorec)) {
    stop()
  }
  vartorecname <- getvarname()
  vartorecfname <- getvar()
  df <- getdf()
  dfname <- get_option("last_df")

  if (missing(levels)) {
    nlev <- length(unique(vartorec))
    levels <- seq(0,nlev-1,1)
  }
  vartorec <- levels[vartorec]
  if (update & is.data.frame(df)) {
    df[, vartorecname] <- vartorec
    # assign(dfname,df,inherits = TRUE )
    push.data(dfname, df)

  }
  invisible(vartorec)
}

push.data <- function(dfname,df) {

  exp = call("<-",dfname,df)
  eval(exp,envir=.GlobalEnv)

}
