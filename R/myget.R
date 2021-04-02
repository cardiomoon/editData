#' Return the Value of a Named data.frame
#' @param x Name of data.frame
#' @export
#' @examples
#' myget("iris")
#' myget("mtcars")
myget=function(x){
     if (!is.null(x) && nzchar(x) &&
         exists(x) && is.data.frame(get(x))) {
          get(x)
     }
}
