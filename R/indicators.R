# stockr: technical indicators (pure base R, NA-padded)

#' Simple moving average
#' @param x numeric vector. @param n window length.
#' @return numeric vector, NA-padded.
#' @export
ma <- function(x, n = 20) {
  x <- as.numeric(x); n <- as.integer(n)
  if (n <= 1) return(x)
  # stats::filter(sides=1) already returns a full-length NA-padded vector
  as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
}

#' Exponential moving average
#' @param x numeric vector. @param n span.
#' @return numeric vector, NA-padded.
#' @export
ema <- function(x, n = 20) {
  x <- as.numeric(x); n <- as.integer(n)
  k <- 2 / (n + 1)
  out <- rep(NA_real_, length(x))
  ok <- which(!is.na(x))
  if (!length(ok)) return(out)
  e <- x[ok[1]]
  for (i in ok) {
    e <- if (i == ok[1]) x[i] else x[i] * k + e * (1 - k)
    out[i] <- e
  }
  out[seq_len(min(n - 1L, length(x)))] <- NA_real_
  out
}

#' Wilder RSI
#' @param x numeric vector (usually close). @param n period, default 14.
#' @return numeric vector 0-100, NA-padded.
#' @export
rsi <- function(x, n = 14) {
  x <- as.numeric(x); n <- as.integer(n)
  d <- diff(x)
  gain <- pmax(d, 0); loss <- pmax(-d, 0)
  ag <- ema(c(NA, gain), n); al <- ema(c(NA, loss), n)
  rs <- ag / al
  out <- 100 - 100 / (1 + rs)
  out[al == 0] <- 100
  out
}

#' MACD
#' @param x numeric vector. @param fast,slow,signal periods.
#' @return data.frame(dif, dea, hist).
#' @export
macd <- function(x, fast = 12, slow = 26, signal = 9) {
  dif <- ema(x, fast) - ema(x, slow)
  dea <- ema(dif, signal)
  data.frame(dif = dif, dea = dea, hist = 2 * (dif - dea))
}

#' Bollinger Bands
#' @param x numeric vector. @param n window, @param k sigma multiplier.
#' @return data.frame(mid, upper, lower, pctb).
#' @export
bollinger <- function(x, n = 20, k = 2) {
  x <- as.numeric(x); n <- as.integer(n)
  mid <- ma(x, n)
  sd_roll <- vapply(seq_along(x), function(i) {
    if (i < n || any(is.na(x[(i - n + 1):i]))) return(NA_real_)
    stats::sd(x[(i - n + 1):i])
  }, numeric(1))
  data.frame(mid = mid, upper = mid + k * sd_roll,
             lower = mid - k * sd_roll,
             pctb = (x - (mid - k * sd_roll)) / (2 * k * sd_roll))
}

#' Average True Range
#' @param df data.frame with high, low, close. @param n period.
#' @return numeric vector, NA-padded.
#' @export
atr <- function(df, n = 14) {
  h <- df$high; l <- df$low; c <- df$close
  pc <- c(NA, c[-length(c)])
  tr <- pmax(h - l, abs(h - pc), abs(l - pc), na.rm = FALSE)
  tr[1] <- h[1] - l[1]
  ema(tr, n)
}

#' Rate of change (%)
#' @param x numeric vector. @param n lookback.
#' @return numeric vector, NA-padded.
#' @export
roc <- function(x, n = 10) {
  x <- as.numeric(x)
  c(rep(NA_real_, n), 100 * (x[-(1:n)] / x[1:(length(x) - n)] - 1))
}
