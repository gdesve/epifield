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
#' @importFrom graphics abline axis barplot hist mtext
#'
#' @param xvar As numbers, factors or text.
#' @param title As character : main title
#' @param ylab Y axis label
#' @param xlab X axis label
#' @param by   For date values by parameters allows to plot histogram "by" days,months or years
#'
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
histogram <- function(xvar, title, ylab="count" , xlab,by='days', width=1, color="#000099"  ) {
  r <- as.list(match.call())
  var <- getvar(r$xvar)
  varlab <- getvarname()
  if (missing(xlab) ) {
    xlab <- varlab
  }
  df <- getdf()
  if ( missing(title) ) { title <- paste0("Distribution of ",getvar()) }

  minx <- min(var,na.rm = TRUE)
  maxx <- max(var,na.rm = TRUE)

  if (! inherits(var,'Date') ) {
     minx <- minx - (minx%%width) -1
     maxx <- maxx + (width - (maxx%%width)) -1
     cut <-  seq(from=minx, to=maxx, by = width)
  } else {
    size <- maxx - minx
    if (size > (365*3)) {
        cut <- "years"
        fmt <- "%Y"
    } else if (size > 365 ) {
        cut <- "months"
        fmt <- "%m-%Y"
    } else {
        cut <- "days"
        fmt <- "%d-%m"
    }
    if (! missing(by)) {
      cut <- by
    }
  }



  my_hist <- hist(var , plot=F, breaks = cut, include.lowest = TRUE)
  maxy <- max(my_hist$count ,na.rm = TRUE)

  if (! inherits(var,'Date') ) {
    maxx <- maxx / width
    xlim <- c(0,maxx-(maxx/5))
    my_hist$labs <- ceiling(my_hist$mids)
  } else {
    my_hist$lab <- as.Date.numeric(my_hist$mids, origin=as.Date("1970-01-01"))
    my_hist$labs<-format(my_hist$lab,fmt)
    xlim<-c(0,maxx)
  }

  barplot(my_hist$counts, names.arg = my_hist$labs , ylim= c(0,maxy*1.2) ,space=0,
          col = color, ylab=ylab , main = title )

  axis(side=1, line=0.1, at=(0.5:(length(cut)-0.5)),lwd=2,lwd.ticks = 1,
       col="white",col.ticks="black", labels=FALSE)

  mtext(xlab,side=1,line=2)  # adj = 0/1
  # abline(h=0,lwd=2)
  invisible(my_hist)
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
  var_count <- table(var)
  maxy <- max(var_count)

  barplot(var_count,ylim= c(0,maxy*1.2),col = color , ylab = ylab, main =title )
  mtext(varlab,side=1,line=2)
  abline(h=0,lwd=1)


  # ggplot(data=df, aes(x=var) ) + geom_bar(color="white",fill=color) +
  #   expand_limits( y = 0) +
  #   scale_y_continuous(expand = c(0, 0) , limits = c(0,NA)  ) +
  #   labs(x = varlab, y = ylab) +
  #   labs(title=title) + epitheme()
}



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

