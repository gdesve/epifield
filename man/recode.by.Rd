% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataman.R
\name{recode.by}
\alias{recode.by}
\title{Title}
\usage{
recode.by(xvar, tovar = "", by = 10, update = TRUE)
}
\arguments{
\item{xvar}{The numeric variable to recode}

\item{tovar}{Optionnaly, the new variable to create in the data frame}

\item{by}{Either a numeric representing the size of each categorie
or a vector with the cutoff}

\item{update}{if TRUE, the default then original data.frame is updated}
}
\value{
The new variable
}
\description{
Title
}
\examples{
recode.by(gastro$age,agegr,by=10)
}
