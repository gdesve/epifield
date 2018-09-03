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
#' @seealso \code{\link{barchart}} for bar graph
#' @export
#'
#' @param ... As numbers, factors or text.
#' @return An array containing  resulting graph
#' #' @examples
#' histogram(c(3,1,2,2,5))
#'
histogram <- function(xvar, df=as.data.frame() ) {

  ggplot(df, aes(x=xvar)) + geom_bar(color="white",fill="blue3") + labs(title="Frequency distribution") + epitheme()
    # theme_minimal() # epitheme()

}

epitheme <- function() {

theme(plot.title=element_text(size=20, face="bold"),
     axis.text.x=element_text(size=13),
     axis.text.y=element_text(size=13),
     axis.title.x=element_text(size=16),
     axis.title.y=element_text(size=16),
     panel.background = element_rect(fill = "grey95"),
     panel.grid.major = element_line(colour = "grey", size=0.5),
     panel.grid.minor = element_line(colour = "grey", size=0.3)
  )

}

#ggplot(cars, aes(x=speed,y=dist), color="blue")+geom_point()+geom_smooth()+
#  labs(title="Scatterplot", x="Speed", y="Distance") + theme1



