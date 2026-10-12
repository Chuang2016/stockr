# stockr basic tests (base R; network tests skipped gracefully offline)
library(stockr)

# indicators on synthetic data
x <- 1:50
stopifnot(length(ma(x, 5)) == 50, isTRUE(all.equal(tail(ma(x, 5), 1), 48)))
stopifnot(length(ema(x, 10)) == 50)
r <- rsi(1:30, 14); stopifnot(all(r[!is.na(r)] >= 0 & r[!is.na(r)] <= 100))
stopifnot(nrow(macd(1:60)) == 60, nrow(bollinger(1:60)) == 60)
stopifnot(length(atr(data.frame(high = 1:30 + 1, low = 1:30 - 1,
                                close = 1:30))) == 30)

# scoring bounds on synthetic trend
set.seed(1)
cl <- cumsum(rnorm(300, 0.001, 0.02)) + 10
kd <- kdj(data.frame(high = cl + 0.1, low = cl - 0.1, close = cl))
stopifnot(nrow(kd) == 300, all(kd$k[!is.na(kd$k)] >= 0 & kd$k[!is.na(kd$k)] <= 100),
          all(kd$d[!is.na(kd$d)] >= 0 & kd$d[!is.na(kd$d)] <= 100))
df <- data.frame(date = Sys.Date() - 300:1, open = cl, high = cl + 0.1,
                 low = cl - 0.1, close = cl, volume = rep(1e6, 300))
ts <- trend_score(df); stopifnot(ts >= 0, ts <= 100)
ms <- momentum_score(df); stopifnot(ms >= 0, ms <= 100)
vs <- value_score("600502"); stopifnot(vs >= 0, vs <= 100)

# backtest mechanics
bt <- backtest(df, sig_trend(df))
stopifnot(all(c("total_ret", "sharpe", "max_dd") %in% names(bt$metrics)))

# buy/sell points
tp <- trade_points(df)
stopifnot(all(tp$signal %in% c(-1, 0, 1)), nrow(tp) == nrow(df))
pos <- position_from_points(tp)
stopifnot(all(pos %in% c(0, 1)), length(pos) == nrow(df))
lp <- latest_point(df)
if (!is.null(lp)) stopifnot(lp$side %in% c("\u4e70\u5165", "\u5356\u51fa"))

# portfolio math on synthetic returns
Rm <- matrix(rnorm(250 * 3, 0, 0.01), 250, 3,
             dimnames = list(NULL, c("a", "b", "c")))
for (fn in list(opt_max_sharpe, opt_min_var, opt_risk_parity, opt_equal)) {
  w <- fn(Rm)$weights
  stopifnot(abs(sum(w) - 1) < 1e-8, all(w >= -1e-12))
}

# live-data smoke tests (skip on network failure)
ok <- tryCatch({ d <- stk_history("600502", 30); !is.null(d) && nrow(d) > 20 },
               error = function(e) FALSE)
if (ok) {
  d <- stk_history("600502", 120)
  stopifnot(all(c("date", "open", "high", "low", "close", "volume") %in% names(d)))
  ad <- advice("600502")
  stopifnot(ad$total >= 0, ad$total <= 100, nchar(ad$label) > 0)
  cat("live-data tests passed\n")
} else {
  cat("live-data tests skipped (offline)\n")
}
cat("ALL TESTS PASSED\n")
