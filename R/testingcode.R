# file for testing code

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
