# stockr: buy/sell points (pure base R)

.cross_up <- function(a, b) {
  # TRUE on days a crosses above b
  n <- length(a)
  out <- rep(FALSE, n)
  ap <- c(NA_real_, a[-n]); bp <- c(NA_real_, b[-n])
  ok <- !is.na(a) & !is.na(b) & !is.na(ap) & !is.na(bp)
  out[ok & a > b & ap <= bp] <- TRUE
  out
}

.cross_dn <- function(a, b) .cross_up(b, a)

#' Identify buy/sell points from KDJ and MACD
#'
#' Buy rules: KDJ golden cross (K crosses above D while K < 50),
#' J crossing back above 20 from oversold, MACD golden cross
#' (DIF crosses above DEA), MACD histogram turning positive.
#' Sell rules mirror: KDJ death cross (K crosses below D while K > 50),
#' J crossing below 80 from overbought, MACD death cross,
#' MACD histogram turning negative.
#' Reasons on the same day are netted (buys minus sells).
#' @param df OHLCV data.frame from stk_history().
#' @param fast,slow kept for compatibility, unused.
#' @return data.frame(date, close, signal, reason); signal in {-1, 0, 1}.
#' @export
trade_points <- function(df, fast = 10, slow = 30) {
  n <- nrow(df)
  mc <- macd(df$close)
  kd <- kdj(df)
  K <- kd$k; D <- kd$d; J <- kd$j

  buy <- matrix(FALSE, n, 4); sell <- matrix(FALSE, n, 4)
  colnames(buy) <- colnames(sell) <- c("kdj_cross", "j_turn", "macd_cross", "macd_hist")

  buy[, "kdj_cross"] <- .cross_up(K, D) & !is.na(K) & K < 50
  sell[, "kdj_cross"] <- .cross_dn(K, D) & !is.na(K) & K > 50
  buy[, "j_turn"] <- .cross_up(J, rep(20, n))
  sell[, "j_turn"] <- .cross_dn(J, rep(80, n))
  buy[, "macd_cross"] <- .cross_up(mc$dif, mc$dea)
  sell[, "macd_cross"] <- .cross_dn(mc$dif, mc$dea)
  buy[, "macd_hist"] <- .cross_up(mc$hist, rep(0, n))
  sell[, "macd_hist"] <- .cross_dn(mc$hist, rep(0, n))

  signal <- sign(rowSums(buy) - rowSums(sell))
  nm_b <- c(kdj_cross = "KDJ\u91d1\u53c9", j_turn = "J\u503c\u8d85\u5356\u56de\u5347",
            macd_cross = "MACD\u91d1\u53c9", macd_hist = "MACD\u7ea2\u67f1\u8f6c\u6b63")
  nm_s <- c(kdj_cross = "KDJ\u6b7b\u53c9", j_turn = "J\u503c\u8d85\u4e70\u56de\u843d",
            macd_cross = "MACD\u6b7b\u53c9", macd_hist = "MACD\u7eff\u67f1\u8f6c\u8d1f")
  reason <- vapply(seq_len(n), function(i) {
    paste(c(nm_b[colnames(buy)[buy[i, ]]], nm_s[colnames(sell)[sell[i, ]]]),
          collapse = "+")
  }, character(1))

  data.frame(date = df$date, close = df$close, signal = signal, reason = reason,
             stringsAsFactors = FALSE)
}

#' Convert event signals to 0/1 holdings
#'
#' A buy (+1) opens a long position held until a sell (-1) closes it.
#' @param tp data.frame from trade_points().
#' @return 0/1 vector aligned with tp rows.
#' @export
position_from_points <- function(tp) {
  pos <- numeric(nrow(tp)); cur <- 0
  for (i in seq_len(nrow(tp))) {
    if (tp$signal[i] > 0) cur <- 1
    else if (tp$signal[i] < 0) cur <- 0
    pos[i] <- cur
  }
  pos
}

#' Latest actionable buy/sell point
#'
#' @param df OHLCV data.frame.
#' @return list(date, price, side, reason), or NULL when no signal exists.
#' @export
latest_point <- function(df) {
  tp <- trade_points(df)
  nz <- which(tp$signal != 0)
  if (!length(nz)) return(NULL)
  i <- max(nz)
  list(date = tp$date[i], price = tp$close[i],
       side = if (tp$signal[i] > 0) "\u4e70\u5165" else "\u5356\u51fa",
       reason = tp$reason[i])
}
