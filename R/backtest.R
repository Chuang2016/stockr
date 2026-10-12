# stockr: backtesting (pure base R)

#' Trend signal used by backtest/tuning
#'
#' Long (1) when close > MA(slow) and MA(fast) > MA(slow), else flat (0).
#' @param df OHLCV data.frame. @param fast,slow windows.
#' @return 0/1 vector.
#' @export
sig_trend <- function(df, fast = 10, slow = 30) {
  cl <- df$close
  sig <- as.numeric(cl > ma(cl, slow) & ma(cl, fast) > ma(cl, slow))
  sig[is.na(sig)] <- 0
  sig
}

#' Backtest a 0/1 signal
#'
#' @param df OHLCV data.frame. @param signal 0/1 position vector.
#' @param cost one-way transaction cost rate.
#' @return list(equity, returns, metrics, trades).
#' @export
backtest <- function(df, signal, cost = 0.0003) {
  cl <- df$close
  r <- c(NA, diff(log(cl)))
  pos <- c(0, signal[-length(signal)])          # trade on next open
  turnover <- c(0, abs(diff(signal)))
  strat <- pos * r - turnover * cost
  strat[is.na(strat)] <- 0
  eq <- cumprod(1 + strat)
  list(equity = eq, returns = strat,
       metrics = perf_metrics(strat),
       trades = sum(abs(diff(signal)) > 0) / 2)
}

#' Performance metrics for a return series
#' @param r numeric daily returns.
#' @return named numeric vector.
#' @export
perf_metrics <- function(r) {
  r <- r[!is.na(r)]
  eq <- cumprod(1 + r)
  dd <- eq / cummax(eq) - 1
  ann_r <- mean(r) * 250
  ann_v <- stats::sd(r) * sqrt(250)
  c(total_ret = prod(1 + r) - 1,
    ann_ret = ann_r,
    ann_vol = ann_v,
    sharpe = ifelse(ann_v > 0, ann_r / ann_v, NA_real_),
    max_dd = min(dd),
    win_rate = mean(r > 0),
    calmar = ifelse(min(dd) < 0, ann_r / abs(min(dd)), NA_real_))
}
