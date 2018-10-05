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
#' @param title As character : main title
#' @param ylab Y axis label
#' @param width numeric, define width of each bar using the unit of the variable
#'        width = 5 to have histogram of age by 5 years
#' @param color Colors can be specified as color eg "red" "blue" or as a hexadecimal RGB triplet,
#'    such as "#0066CC". The first two digits are the level of red, the next two green, and the last two blue.
#'    The value for each ranges from 00 to FF in hexadecimal (base-16) notation,
#'    which is equivalent to 0 and 255 in base-10.
#'    For example, “#FFFFFF” is white and “#990000” is a deep red.
#' @return An array containing  resulting graph
#' #' @examples
#' histogram(c(3,1,2,2,5))
#'
histogram <- function(xvar, title, ylab="count" ,width=NULL, color="#000099"  ) {
  r <- as.list(match.call())
  var <- getvar(r$xvar)
  varlab <- getvarname()
  df <- getdf()
  if ( missing(title) ) { title <- paste0("Distribution of ",getvar()) }
  suppressWarnings( ggplot(data=df, aes(x=var) ) +
#    geom_histogram(stat="count",binwidth = width, color="white",fill=color) +
  geom_histogram(binwidth = width, color="white",fill=color) +
    expand_limits( y = 0) +
    scale_y_continuous(expand = c(0, 0) , limits = c(0,NA)  ) +
    labs(x = varlab, y = ylab) +
    labs(title=title) + epitheme()
  )
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
#' @param title As character : main title
#' @param ylab Y Axis label
#' @param color Colors can be specified as color eg "red" "blue" or as a hexadecimal RGB triplet,
#'    such as "#0066CC". The first two digits are the level of red, the next two green, and the last two blue.
#'    The value for each ranges from 00 to FF in hexadecimal (base-16) notation,
#'    which is equivalent to 0 and 255 in base-10.
#'    For example, “#FFFFFF” is white and “#990000” is a deep red.
#' @return An array containing  resulting graph
#' #' @examples
#' bargraph(c(3,1,2,2,5))
#'
bargraph <-function(xvar,title,ylab="count", color ="#000099" )  {
  r <- as.list(match.call())
  var <- getvar(r$xvar)
  varlab <- getvarname()
  df <- getdf()
  if ( missing(title) ) { title <- paste0("Distribution of ",getvar()) }

  var <- as.factor((var))
  ggplot(data=df, aes(x=var) ) + geom_bar(color="white",fill=color) +
    expand_limits( y = 0) +
    scale_y_continuous(expand = c(0, 0) , limits = c(0,NA)  ) +
    labs(x = varlab, y = ylab) +
    labs(title=title) + epitheme()
}

epitheme <- function() {

theme(plot.title=element_text(size=18), # ,face="bold"
     axis.text.x=element_text(size=10),
     axis.text.y=element_text(size=12),
     axis.title.x=element_text(size=16),
     axis.title.y=element_text(size=16),
     axis.line = element_line(color = "black"),

     #panel.ontop = TRUE , # line on top of graph
     #panel.grid.major.y = element_line(color="white",size=0.5),
     #panel.grid.minor.y = element_line(color="white",size=0.5),

     # panel.spacing.y = 1 ,
     panel.background = element_blank(),
     # panel.background = element_rect(fill = "white"),

     panel.grid.major = element_line(colour = "white", size=0),
     panel.grid.minor = element_line(colour = "white", size=0)
  )

}

#ggplot(cars, aes(x=speed,y=dist), color="blue")+geom_point()+geom_smooth()+
#  labs(title="Scatterplot", x="Speed", y="Distance") + theme1

#' Title
#'
#' @param name Name of the file
#' @param type Type of saved image (png,jpeg,pdf,bmp,wmf)
#'
#' @return nothing
#' @export
#' @importFrom grDevices dev.copy dev.off png
#'
#' @examples
#' savegraph("test")
savegraph <- function(name,type=png) {
    if (! regexpr("png",name)>0) {
      name<-paste0(name,'.png')
    }
    dev.copy(png,name)
    dev.off()
}

