#' Circle Area and Circumference
#'
#' @param r radius of a circle you want
#'
#' @return Area and circumference of certain radii.
#' @export
#'
#' @examples
#' x=c(2,3,4)
#' circle(x)

circle=function(r){
  Area=pi*r^2
  Circumference=2*pi*r
  return(list(Area=Area, Circumference=Circumference))
}
