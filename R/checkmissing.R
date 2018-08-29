checkMissing <- function(data, vars=NULL, sort) {
  Names = names(data)
  if (is.null(vars) == FALSE) {
    Names = vars;
  }

  # Simple description of missing values
  # ------------------------------------
  missdesc <- function(Names) {
    Colnames = c("Variable", "Missing", "% Missing");
    Effectif = nrow(data);
    Missing  = c();
    PMissing = c();

    for (N in Names) {
      Miss = sum(is.na(data[,N]));
      PMiss = sprintf("%5.2f", (Miss / Effectif) * 100);
      Missing = c(Missing, Miss);
      PMissing = c(PMissing, PMiss);
    }

    DF = data.frame(cbind(Names, as.numeric(Missing), PMissing))
    names(DF) <- Colnames
    DF[,2] <- as.numeric(as.character(DF[,2]))
    DF[,3] <- as.numeric(as.character(DF[,3]))
    if (sort == TRUE) {
      DF <- DF[rev(order(DF[, "Missing"])),]
      row.names(DF) <- c(1:nrow(DF))
    }
    return(DF);
  }


  missbyrow <- function(Names, Effectif) {
    Colnames = c("Nb missing values", "Frequency", "Percent", "Cumul");
    counts = apply(data, 1, function(x) sum(is.na(x[Names])));
    d <- as.data.frame(counts);
    t <- table(counts);
    vals = names(t);
    t <- cbind(t, round(prop.table(t)*100,2));
    DF1 <- as.data.frame(t);
    Cum <- cumsum(DF1[,2]);
    DF2 <- data.frame(cbind(vals, DF1$t, DF1$V2, Cum));
    names(DF2) <- Colnames;
    return(DF2);
  }

  .df1 <- missdesc(Names)
  .df2 <- missbyrow(Names, Effectif)
  .L <- list(df1=.df1, df2=.df2)
  .L

}
