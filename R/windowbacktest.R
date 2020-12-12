
#'Window-backtest
#'
#' @param model the model being backtested
#' @param data time series data
#' @param orig orig
#' @param h forecast steps
#' @param xreg xreg
#' @param fixed fixed
#' @param inc.mean mean
#' @param reest reest
#'
#' @return return RMSE of out-of-sample forecasts and Mean absolute error of out-of-sample forecasts
#' @export
#'
#' @examples
#' timeseriesdata=datasets::AirPassengers
#' model=arima(timeseriesdata,order=c(2,1,1))
#' backtest=window_backtesting(model,timeseriesdata,orig=120,h=12)

window_backtesting <- function(model, data, orig, h, xreg=NULL,fixed = NULL,
                               inc.mean = TRUE,  reest = 1){
  if(!inherits(data,"ts"))stop("data must be a time series object")
  arma_order <- model$arma
  regor = arma_order[c(1, 6, 2)]
  seaor = list(order = arma_order[c(3, 7, 4)],  period = arma_order[5])
  T = length(data)
  if (orig > T)
    orig = T
  if (h < 1)
    h = 1
  rmse = numeric(h)
  mabso = numeric(h)

  nori = T - orig
  err = matrix(0, nori, h)
  fcst = matrix(0, nori, h)
  jlast = T - 1
  time_vec <- time(data)
  ireest <- reest
  for (n in orig:jlast) {
    jcnt = n - orig + 1
    x <- window(data, time_vec[jcnt], time_vec[n])
    if (is.null(xreg))
      pretor = NULL
    else pretor = xre[jcnt:n]
    if (ireest == reest) {
      mm = arima(x, order = regor, seasonal = seaor, xreg = pretor,
                 fixed = fixed, include.mean = inc.mean)
      ireest <- 0
    }
    else {
      ireest <- ireest + 1
    }
    if (is.null(xreg)) {
      nx = NULL
    }
    else {
      nx = xreg[(n + 1):(n + h)]
    }
    fore = predict(mm, h, newxreg = nx)
    kk = min(T, (n + h))
    nof = kk - n
    pred = fore$pred[1:nof]
    obsd = data[(n + 1):kk]
    err[jcnt, 1:nof] = obsd - pred
    fcst[jcnt, 1:nof] = pred
  }
  for (i in 1:h) {
    iend = nori - i + 1
    tmp = err[1:iend, i]
    mabso[i] = sum(abs(tmp))/iend
    rmse[i] = sqrt(sum(tmp^2)/iend)
  }
  print("RMSE of out-of-sample forecasts")
  print(rmse)
  print("Mean absolute error of out-of-sample forecasts")
  print(mabso)
  backtest <- list(origin = orig, error = err, forecasts = fcst,
                   rmse = rmse, mabso = mabso, reest = reest)
}
