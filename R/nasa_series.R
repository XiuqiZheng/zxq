#' NASA asteriod data
#'
#' @return asteriod near the earth information
#' @export
#'
#' @examples
#' nasa_series()
nasa_series=function(){
  api_key = "91RalvZvH26nVUuSX5NUrcJIMsaL9int2ago72cL"
  START_DATE = "2020-01-01"
  END_DATE="2020=01-08"
  URL="https://api.nasa.gov/neo/rest/v1/feed"
  parameters2 = paste(
    "?api_key=",api_key,
    "&start_date",START_DATE,
    "end_date",END_DATE,
    sep="")
  PATH = paste0(URL, parameters2)

  initialquery2 = jsonlite::fromJSON(PATH)
  initialquery2
}
