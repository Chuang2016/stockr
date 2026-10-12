# stockr: scoring -> investment advice (pure base R)

.last <- function(x) x[length(x)]

#' Trend score (0-100) from price/volume structure
#'
#' Composite of MA trend, MACD, RSI zone, Bollinger position and volume.
#' @param df OHLCV data.frame from stk_history.
#' @param fast,slow MA windows.
#' @return score 0-100.
#' @export
trend_score <- function(df, fast = 10, slow = 30) {
  cl <- df$close; n <- nrow(df)
  m_f <- ma(cl, fast); m_s <- ma(cl, slow); m_60 <- ma(cl, 60)
  mc <- macd(cl); bb <- bollinger(cl)
  r <- rsi(cl); v <- df$volume; v_rat <- ma(v, 5) / ma(v, 60)
  i <- n
  s <- 0
  s <- s + if (!is.na(m_60[i]) && cl[i] > m_60[i]) 25 else
           if (!is.na(m_60[i]) && cl[i] > 0.97 * m_60[i]) 12 else 0
  s <- s + if (!is.na(m_f[i]) && !is.na(m_s[i]) && m_f[i] > m_s[i]) 20 else 0
  s <- s + if (!is.na(mc$hist[i]) && mc$hist[i] > 0) 15 else 0
  s <- s + if (!is.na(mc$hist[i]) && i > 1 && !is.na(mc$hist[i-1]) &&
               mc$hist[i] > mc$hist[i-1]) 5 else 0
  s <- s + if (!is.na(r[i]) && r[i] >= 50 && r[i] <= 70) 15 else
           if (!is.na(r[i]) && ((r[i] >= 40 && r[i] < 50) ||
                                (r[i] > 70 && r[i] <= 80))) 8 else 3
  s <- s + if (!is.na(bb$mid[i]) && cl[i] > bb$mid[i]) 10 else
           if (!is.na(bb$lower[i]) && cl[i] > bb$lower[i]) 5 else 2
  s <- s + if (!is.na(v_rat[i]) && v_rat[i] > 1.2 && cl[i] > df$open[i]) 10 else 4
  min(100, max(0, s))
}

#' Time-series momentum score (0-100)
#'
#' Percentile of the current 60-day ROC against its own 250-day history.
#' @param df OHLCV data.frame.
#' @return score 0-100.
#' @export
momentum_score <- function(df) {
  r60 <- roc(df$close, 60)
  cur <- .last(r60)
  hist <- r60[!is.na(r60)]
  if (is.na(cur) || length(hist) < 60) return(50)
  100 * mean(hist <= cur)
}

#' Value score (0-100) from the fundamental snapshot
#'
#' Lower PE/PB, higher ROE and dividend yield score higher.
#' @param code 6-digit stock code.
#' @return score 0-100.
#' @export
value_score <- function(code) {
  f <- fund_snapshot()
  row <- f[f$code == gsub("\\D", "", code), ]
  if (!nrow(row)) return(NA_real_)
  pe <- row$pe_ttm; pb <- row$pb; roe <- row$roe; dy <- row$div_yield
  s_pe <- if (is.na(pe)) 6 else if (pe <= 6) 40 else if (pe <= 10) 32 else
          if (pe <= 15) 24 else if (pe <= 25) 14 else 6
  s_pb <- if (is.na(pb)) 4 else if (pb < 0.5) 25 else if (pb < 0.8) 20 else
          if (pb < 1.2) 14 else if (pb < 2) 8 else 4
  s_roe <- if (is.na(roe)) 0 else if (roe > 10) 20 else if (roe > 7) 15 else
           if (roe > 3) 10 else if (roe > 0) 5 else 0
  s_dy <- if (is.na(dy)) 2 else if (dy > 4) 15 else if (dy > 2) 10 else
          if (dy > 0.5) 6 else 2
  s_pe + s_pb + s_roe + s_dy
}

.advice_label <- function(total) {
  if (is.na(total)) return(NA_character_)
  if (total >= 75) "\u5f3a\u70c8\u63a8\u8350" else      # \u5f3a\u70c8\u63a8\u8350
  if (total >= 60) "\u63a8\u8350" else                  # \u63a8\u8350
  if (total >= 45) "\u4e2d\u6027\u504f\u591a" else      # \u4e2d\u6027\u504f\u591a
  if (total >= 35) "\u4e2d\u6027" else                  # \u4e2d\u6027
  if (total >= 25) "\u8c28\u614e" else                   # \u8c28\u614e
                   "\u56de\u907f"                      # \u56de\u907f
}

#' Investment advice for one stock
#'
#' Combines trend (technical), value (fundamental) and momentum scores.
#' @param code 6-digit code.
#' @param w weights c(trend, value, momentum), default c(0.5, 0.3, 0.2).
#' @param n history length for scoring.
#' @return one-row data.frame with scores, total and label.
#' @export
advice <- function(code, w = c(0.5, 0.3, 0.2), n = 300) {
  code6 <- gsub("\\D", "", code)
  df <- stk_history(code6, n)
  if (is.null(df) || nrow(df) < 80)
    return(data.frame(code = code6, name = NA_character_, price = NA_real_,
                      pct_chg = NA_real_, trend = NA_real_, value = NA_real_,
                      momentum = NA_real_, total = NA_real_,
                      label = NA_character_, stringsAsFactors = FALSE))
  nm <- stk_universe(); nm <- nm$name[nm$code == code6]
  if (!length(nm)) nm <- code6
  tr <- trend_score(df); mo <- momentum_score(df); va <- value_score(code6)
  total <- w[1] * tr + w[2] * va + w[3] * mo
  tp <- trade_points(df)
  nz <- which(tp$signal != 0)
  sig_txt <- if (length(nz)) {
    i <- max(nz)
    paste0(if (tp$signal[i] > 0) "\u4e70\u5165" else "\u5356\u51fa",
           "@", format(tp$date[i], "%m-%d"))
  } else "-"
  data.frame(code = code6, name = nm, price = .last(df$close),
             pct_chg = 100 * (.last(df$close) / df$close[nrow(df) - 1] - 1),
             trend = round(tr, 1), value = round(va, 1),
             momentum = round(mo, 1), total = round(total, 1),
             label = .advice_label(total), signal = sig_txt,
             stringsAsFactors = FALSE)
}

#' Ranked advice for the whole universe
#'
#' @param w weights passed to advice().
#' @param n history length.
#' @return data.frame sorted by total score, with rank column.
#' @export
advice_universe <- function(w = c(0.5, 0.3, 0.2), n = 300) {
  u <- stk_universe()
  out <- do.call(rbind, lapply(u$code, advice, w = w, n = n))
  out <- out[order(-out$total), ]
  out$rank <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}
