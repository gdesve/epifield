# epifield documentation using roxygen2
#' @title
#' histogram
#' @description
#' \code{histogram} Display an histogram of frequency distribution.
#' #'
#'
#' @name histogram
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{bargraph}} for bar graph
#' @export
#' @import ggplot2
#'
#' @param xvar As numbers, factors or text.
#' @param df As data.frame
#' @return An array containing  resulting graph
#' #' @examples
#' histogram(c(3,1,2,2,5))
#'
histogram <- function(xvar, df=as.data.frame() ) {

  ggplot(data=df, aes(x=xvar) ) + geom_histogram(color="white",fill="blue3",binwidth = 1) +
    expand_limits( y = 0) +
    scale_y_continuous(expand = c(0, 0) , limits = c(0,NA)  ) +
    labs(title="Histogram") + epitheme()

}

# epifield documentation using roxygen2
#' @title
#' bargraph
#' @description
#' \code{bargraph} Display a bar graph of frequency distribution.
#' #'
#'
#' @name bargraph
#'
#' @author Gilles Desve
#' @references Based on: \emph{Epi6} and \emph{Stata} functionnality,
#' available at \url{https://github.com/}.
#'
#' @seealso \code{\link{histogram}} for bar graph
#' @export
#' @import ggplot2
#'
#' @param xvar As numbers, factors or text.
#' @param df As data.frame
#' @return An array containing  resulting graph
#' #' @examples
#' bargraph(c(3,1,2,2,5))
#'
bargraph <-function(xvar, df=as.data.frame())  {
  df$fvar <- as.factor((xvar))
  ggplot(data=df, aes(x=fvar) ) + geom_bar(color="white",fill="blue3") +
    expand_limits( y = 0) +
    scale_y_continuous(expand = c(0, 0) , limits = c(0,NA)  ) +
    labs(title="Frequency distribution") + epitheme()
}

epitheme <- function() {

theme(plot.title=element_text(size=20, face="bold"),
     axis.text.x=element_text(size=13),
     axis.text.y=element_text(size=13),
     axis.title.x=element_text(size=16),
     axis.title.y=element_text(size=16),
     axis.line = element_line(color = "black"),

     #panel.ontop = TRUE , # line on top of graph
     #panel.grid.major.y = element_line(color="white",size=0.5),
     #panel.grid.minor.y = element_line(color="white",size=0.5),

     # panel.spacing.y = 1 ,
     panel.background = element_blank(),
     # panel.background = element_rect(fill = "white"),

     panel.grid.major = element_line(colour = "grey", size=0),
     panel.grid.minor = element_line(colour = "grey", size=0)
  )

}

#ggplot(cars, aes(x=speed,y=dist), color="blue")+geom_point()+geom_smooth()+
#  labs(title="Scatterplot", x="Speed", y="Distance") + theme1



