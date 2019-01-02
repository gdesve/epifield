#' checkMissing
#'
#'
#' @param what a data.frame, a vector or a vector of colnames
#' @param ...  a list of variable to be tested. Is used only if "what" is not a dataframe
#' @param sort boolean If TRUE the output table is sorted 'NOT IMPLEMENTED'
#' @param showall boolean if FALSE, the default, then variables with no missing values are
#' not included in the table
#'
#' @return list of 2 tables
#' @export checkmissing
#'
#' @author Jean Pierre Decorps adapted for epifield by Gilles Desve
#'
#' @examples
#' # still nothing
checkmissing <- function(what, ..., sort=FALSE,showall=FALSE ) {
  r <- as.list(match.call())

  i <- match("sort",names(r))
  if (!is.na(i)) r[i]<-NULL

  i <- match("showall",names(r))
  if (!is.na(i)) r[i]<-NULL

  r[1]<-NULL    # this suppress the first argument (function name)

  if (missing(what)) {
    what <- getdata()
  }

  df <- try(eval(r[[1]]),TRUE)

  if (is.data.frame(df)) {
    vars = names(what)
  } else {
    if ( length(r) > 0 ) {
      nb <- length(r)
      vars <- list()
      showall <- TRUE
      for (i in 1:nb) {
         name <- r[[i]]
         # if vars is long syntax then as.character doesn't work   XXX
         vars[i] <- as.character(name)
      }

    }
  }
  # if df was passed don't try to get var  with getvar because of ambigous names
  if ( length(vars) > 0 ) {
     name <- getvar(vars[[1]])
     df <- getlastdf()
  }

  # all this as to be restructured to accept vector (and df to be used as df[i]) XXX
  effectif = nrow(df);  # var1 if single var

  res1  = list();
  i <- 0
  nomiss <- c()
  for (name in vars) {
    i <- i + 1
    if (any(name==colnames(df)) ) {
      miss1 <- sum(is.na(df[,name]))
      if (miss1 > 0 | showall ) {
        pmiss1 <- round((miss1 / effectif) * 100, digits = 2)
        res1 <- rbind(res1, c(miss1,pmiss1))
      } else {
        nomiss <- cbind(nomiss,i)
      }
    } else {
      res1 <- rbind(res1, c(NA,NA))
    }
  }
  if ( length(nomiss) > 0 ) {
    vars <- vars[-nomiss]
  }
  vars <- lapply(vars,lpad,width=16)
  rownames(res1) <- vars
  colnames(res1) <- c("Nb Missing", "% Missing")
  names(dimnames(res1)) <- c("variables","Missing")
  # if (sort == TRUE) {
  #   o <- res1[, "Nb Missing"]
  #   o <- order( sapply(o, "[", 2) )
  #   res1 <- res1[rev(o),]
  #   row.names(res1) <- c(1:nrow(res1))
  # }
  return(res1);

  # sortBy <- function(a, field) a[order(sapply(a, "[[", i = field))]
  # sortBy(a, "day")

  # missbyrow <- function(vars, effectif) {
  #   counts = apply(df, 1, function(x) sum(is.na(x[vars])));
  #   d <- as.data.frame(counts);
  #   t <- table(counts);
  #   vals = names(t);
  #   res1 <- cbind(t, round(prop.table(t)*100,2));
  #   Cum <- cumsum(res1[,2]);
  #   res2 <- cbind(vals, res1[,1], res1[,2], Cum);
  #   names(res2) <- c("Nb missing values", "Frequency", "Percent", "Cumul")
  #   return(res2)
  # }
  #
  # res1 <- missdesc(vars, effectif)
  # res2 <- missbyrow(vars, effectif)
  # result <- list(missing=res1, missbyrow=res2)
  # result

}
