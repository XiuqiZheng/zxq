
#' Title:unemployment rate data from FRED
#'
#' @return return unemployment rate data from 2010-01-01 to current
#' @export
#'
#' @examples
#'fred_get_series()

fred_get_series <- function(){
  URL = "https://api.stlouisfed.org/fred/series/observations"
  APIkey = "ab0b41ebf51f53db54d2977340a4bb50"
  series_id = "UNRATE"
  observation_start = "2010-01-01"

  parameters = paste(
    "?series_id=",series_id,
    "&api_key=", APIkey,
    "&file_type=json",
    "&observation_start=",observation_start,
    sep = "")
  PATH = paste0(URL, parameters)

  initialquery = jsonlite::fromJSON(PATH)
  df = initialquery$observations
  rownames(df) <- df$date  #Change index to a Date

  df = df[c("value")] # Just use value column
  df$value <- as.numeric(df$value) #change characters to numbers

  return(df)
}
