% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basetools.R
\name{read}
\alias{read}
\title{@title Read a data.frame from disk into memory}
\usage{
read(filename = "", factorise = FALSE, lowercase = FALSE, label = NULL, ...)
}
\arguments{
\item{filename}{Name of file to be read. Type is defined by extension}

\item{factorise}{Indicate if character variable should be read as factor. If false, the default
The column is read as character without transformation. Further transformation can be done
with factor base function or with epiorder function.}

\item{lowercase}{If TRUE, variable names are set to lowercase otherwise they are not changed}

\item{label}{Label to be added as attribute to data.frame. This label will be used as description}

\item{...}{other standard read options}
}
\description{
Read a file of various format and return a data.frame.
 The function try to identify the file structure in order to call the appropriate specific
 command
 Currently accepted extension are : csv, dta, rec, rda
}
\details{
For csv files the first line is analysed to identify the separator.
 Accepted separator are  ,  or  ;

 @return  The data.frame is returned (and should be assign to a variable)
}
\examples{
fil <- tempfile(fileext = ".data")
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
sep = "\n")
df <- read(filename=fil)
unlink(fil) # tidy up

}
