
#' Title:Power and root matrix
#'
#' @param x any numbers
#'
#' @return fifth power to five root matrix of a number
#' @export
#'
#' @examples
#' power_root_matrix(64)
power_root_matrix= function(x){
  data.frame(fifthpower=(x)^5,
             fouthpower=(x)^4,
             cube=(x)^3,
             squareroot=sqrt(x),
             threeroot=(x)^(1/3),
             fourroot=(x)^(1/4),
             fiveroot=(x)^(1/5))
}
