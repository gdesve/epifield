# testfile ignored by BUILD for testing function

tempf <- function(all="",b=2) {
  d <- 5
  print("match.call....")
  r <- as.list(match.call())

  str(r)
  if (!is.null(r[["all"]])) {
     print("ALL")
  } else print("NOTALL")

#  cat("Currently set values defined in call or formals\n")
#  print(allargs())
#  cat("Values as defined at the time of the call\n")
#  print(allargs(T))
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

fff = function(x, ...) {
  args = as.list(match.call()) # contains a=1, b=2
  print(unlist(args))
}

#internal function to retrieve dataset variables
# use of lapply could be more efficient than loop ?
adddf <- function(explist) {
  arglist <- as.list(explist)
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

#inline assignement (returned value must be the passed objet)
`recode<-` <- function(x, where, value) {

  r <- if (missing(where))
    rep_len(TRUE, nrow(x))
  else {
    e <- substitute(where)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'where' must be logical")
    r & !is.na(r)
  }

  x[r,] <- value
  x
}


test2 <- function() {
   print(sys.calls())
   3
}



dat = data.frame(sCode = c("CA", "CA", "AC"))
nrow(dat[dat$sCode == "CA",])
length(dat$sCode[dat$sCode == "CA"])
sum(which(dat$sCode == "CA"))
