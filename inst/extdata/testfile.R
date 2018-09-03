# testfile ignored by BUILD for testing function

tempf <- function(first="essai",arg2,arg3, arg4,...) {
  r <- as.list(match.call())
  l <- length(r)
  cat("match.call....", l , " Args\n")
  print(r)

  params <- names(r)
  cat(params)

  cat("Details ------\n")
  if (l>1) {
    for ( i in 2:l ) {
      arg <- r[[i]]
      mod <- mode(arg)
      switch (mod ,
      "character" = {
        name <- arg
      } ,
      "call" =  {
        name <- deparse(substitute(arg))
      } ,
      "name" = {
        name <- as.character(substitute(arg))
      } ,
      "logical" = {
        name <- as.character(substitute(arg))
      } ,
      { name <- arg
        } )
      cat("arg",i-1," : ",names(r[i]) , "Val:", as.character(arg) ,"(",name,") Mode : ", mode(arg),"Exists ? ",ifelse(mod=="name",exists(name),FALSE),"\n")
    }
  }
}

test_freq <- function() {
  zou <- c(1,1,2,2,2,3)
  freq(zou)
}

which2.envir <- function(what) {
  cat("two :" , sys.nframe(), "\n")
  cat("liste ", list=ls(sys.frame(-1),pattern=what)  ,"\n"  )
  cat(exists(what), "\n")
  print(apropos(glob2rx(what)))
}

wich.envir <- function(what) {
  vlocal = 5
  cat("one :" , sys.nframe(), "\n")
  cat("liste ", list=ls(pattern=what)  ,"\n"  )
  cat(exists(what), "\n")
  which2.envir(what)
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


tabular <- function(df) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  issue <- paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
                 contents, "\n}\n", sep = "")
  print(issue)


}

eps.recode <- function(data, var, where, by, gen="")
{
  df <- data
  gen <- as.character(substitute(gen))

  L <- length(where);
  if (is.list(var)) {
    vn <- var
  } else {
    vn <- list(var)
  }

  # Creating a new column if necessary
  if (gen != "") {
    df <- cbind(df, XNEWCOLX=df[,var]);
    names(df)[names(df)=="XNEWCOLX"] <- gen;
    vn <- gen;
  }

  for (N in 1:length(vn)) {
    Name <- vn[[N]]

    # for each conditionnal statement
    for (i in 1:L) {
      Exp <- sprintf("df[,'%s'] %s", Name, where[[i]])
      r <- eval(parse(text=Exp))
      r <- replace(r, is.na(r), FALSE);
      df[r, Name] <- by[i];
    }
  }
  df
}


dat = data.frame(sCode = c("CA", "CA", "AC"))
nrow(dat[dat$sCode == "CA",])
length(dat$sCode[dat$sCode == "CA"])
sum(which(dat$sCode == "CA"))
