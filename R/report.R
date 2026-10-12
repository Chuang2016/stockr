# stockr: reporting (pure base R)

.stars <- function(total) {
  n <- if (is.na(total)) 0 else if (total >= 75) 5 else if (total >= 60) 4 else
       if (total >= 45) 3 else if (total >= 35) 2 else if (total >= 25) 1 else 0
  paste0(strrep("\u2605", n), strrep("\u2606", 5 - n))
}

#' Print a ranked investment-advice report
#'
#' @param ad data.frame from advice_universe().
#' @return invisible(ad).
#' @export
advice_report <- function(ad) {
  cat("============ \u6295\u8d44\u5efa\u8bae\u62a5\u544a ============\n")
  cat("\u57fa\u51c6\u65e5\u671f: ", format(Sys.Date(), "%Y-%m-%d"),
      "  (\u6743\u91cd \u8d8b\u52bf50%/\u4f30\u503c30%/\u52a8\u91cf20%)\n\n", sep = "")
  for (i in seq_len(nrow(ad))) {
    cat(sprintf("#%d %s(%s)  %s\n", ad$rank[i], ad$name[i], ad$code[i],
                .stars(ad$total[i])))
    cat(sprintf("   \u73b0\u4ef7 %.2f  \u6da8\u8dcc %+.2f%%  |  \u8d8b\u52bf %.0f  \u4f30\u503c %.0f  \u52a8\u91cf %.0f  =>  \u7efc\u5408 %.1f  [%s]\n",
                ad$price[i], ad$pct_chg[i], ad$trend[i], ad$value[i],
                ad$momentum[i], ad$total[i], ad$label[i]))
  }
  cat("\n\u8bf4\u660e: \u8d8b\u52bf=\u6280\u672f\u9762(MA/MACD/RSI/\u5e03\u6797/\u91cf\u4ef7), \u4f30\u503c=\u57fa\u672c\u9762(PE/PB/ROE/\u80a1\u606f), \u52a8\u91cf=60\u65e5\u6da8\u8dcc\u5386\u53f2\u5206\u4f4d.\n")
  cat("\u98ce\u9669\u63d0\u793a: \u91cf\u5316\u8bc4\u5206\u4ec5\u4f9b\u53c2\u8003, \u4e0d\u6784\u6210\u6295\u8d44\u5efa\u8bae; \u5e02\u573a\u6709\u98ce\u9669, \u5165\u5e02\u9700\u8c28\u614e.\n")
  invisible(ad)
}

#' Four-panel technical chart
#'
#' Price + MA/Bollinger, volume, RSI, MACD.
#' @param df OHLCV data.frame. @param code title code.
#' @param file optional PNG path; plots to screen when NULL.
#' @return invisible(df).
#' @export
plot_stock <- function(df, code = "", file = NULL) {
  cl <- df$close
  bb <- bollinger(cl); r <- rsi(cl); mc <- macd(cl)
  if (!is.null(file)) grDevices::png(file, width = 1200, height = 900, res = 110)
  on.exit(if (!is.null(file)) grDevices::dev.off(), add = TRUE)
  graphics::par(mfrow = c(4, 1), mar = c(1.5, 4, 2.5, 1), oma = c(3, 0, 0, 0))
  graphics::plot(cl, type = "l", main = paste(code, "\u65e5K\u6280\u672f\u56fe"),
                 ylab = "\u4ef7\u683c", xaxt = "n")
  graphics::lines(ma(cl, 10), col = "orange"); graphics::lines(ma(cl, 30), col = "blue")
  graphics::lines(bb$upper, col = "gray", lty = 2); graphics::lines(bb$lower, col = "gray", lty = 2)
  graphics::barplot(df$volume, border = NA, col = "steelblue", ylab = "\u6210\u4ea4\u91cf", xaxt = "n")
  graphics::plot(r, type = "l", ylim = c(0, 100), ylab = "RSI", xaxt = "n", col = "purple")
  graphics::abline(h = c(30, 70), lty = 2, col = "gray")
  graphics::barplot(mc$hist, border = NA, col = ifelse(mc$hist > 0, "red", "green"),
                    ylab = "MACD", names.arg = "")
  graphics::axis(1, at = seq(1, nrow(df), length.out = 6),
                 labels = format(df$date[seq(1, nrow(df), length.out = 6)], "%m-%d"))
  invisible(df)
}
