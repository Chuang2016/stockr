# stockr: data acquisition (pure base R, zero dependencies)

# ---- internal helpers -------------------------------------------------

.stk_get <- function(url) {
  # Fetch a URL as text with a browser-like UA; NULL on failure.
  h <- tryCatch({
    con <- url(url, open = "r",
               headers = c("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    paste(readLines(con, warn = FALSE), collapse = "\n")
  }, error = function(e) NULL)
  h
}

# ---- universe -----------------------------------------------------------

#' Four-stock default universe
#'
#' The four A-shares this package ships a workflow for.
#' @return data.frame with columns code (6-digit) and name.
#' @export
stk_universe <- function() {
  data.frame(
    code = c("600502", "601669", "600966", "601908"),
    name = c("\u5b89\u5fbd\u5efa\u5de5", "\u4e2d\u56fd\u7535\u5efa",
             "\u535a\u6c47\u7eb8\u4e1a", "\u4eac\u8fd0\u901a"),
    stringsAsFactors = FALSE
  )
}

#' Normalise a stock code to exchange-prefixed form
#'
#' @param x 6-digit code, optionally already prefixed.
#' @return e.g. "sh600502".
#' @export
stk_code <- function(x) {
  x <- gsub("\\D", "", as.character(x))
  ifelse(substr(x, 1, 1) %in% c("6", "9"), paste0("sh", x), paste0("sz", x))
}

# ---- fundamentals snapshot ----------------------------------------------

#' Static fundamental snapshot for the universe
#'
#' Values collected Oct 2026 (2026H1 reports + market data). Update by
#' editing this function after each earnings season.
#' @return data.frame with pe_ttm, pb, roe, rev_g, profit_g, div_yield.
#' @export
fund_snapshot <- function() {
  data.frame(
    code       = c("600502", "601669", "600966", "601908"),
    name       = c("\u5b89\u5fbd\u5efa\u5de5", "\u4e2d\u56fd\u7535\u5efa",
                   "\u535a\u6c47\u7eb8\u4e1a", "\u4eac\u8fd0\u901a"),
    pe_ttm     = c(5.0, 9.3, 44.9, NA),
    pb         = c(0.37, 0.70, 1.01, 1.14),
    roe        = c(9.05, 7.20, 1.10, -5.42),
    rev_g      = c(1.67, 3.10, 3.26, -43.49),
    profit_g   = c(2.08, -8.50, -31.31, -83.31),
    div_yield  = c(5.5, 2.1, 0.5, 0.0),
    updated    = "2026-10-12",
    stringsAsFactors = FALSE
  )
}

# ---- historical daily klines (Sina) -------------------------------------

.sina_field <- function(obj, field) {
  m <- regmatches(obj, regexpr(paste0('"', field, '":"?([^",}]+)'),
                               obj, perl = TRUE))
  if (!length(m)) return(NA_character_)
  sub(paste0('.*"', field, '":"?'), "", m)
}

#' Fetch historical daily K-lines
#'
#' Uses Sina's public quote API (daily scale). Pure base R.
#' @param code 6-digit code or exchange-prefixed code.
#' @param n number of recent trading days (max ~2000).
#' @return data.frame(date, open, high, low, close, volume), oldest first.
#' @export
stk_history <- function(code, n = 250) {
  px <- stk_code(code)
  url <- paste0("https://quotes.sina.cn/cn/api/openapi.php/",
                "CN_MarketDataService.getKLineData?symbol=", px,
                "&scale=240&ma=no&datalen=", as.integer(n))
  txt <- .stk_get(url)
  if (is.null(txt) || !grepl('"code":0', txt)) {
    warning("stk_history: failed to fetch ", code)
    return(NULL)
  }
  objs <- regmatches(txt, gregexpr("\\{[^{}]+\\}", txt, perl = TRUE))[[1]]
  objs <- objs[grepl('"day"', objs)]
  if (!length(objs)) return(NULL)
  df <- data.frame(
    date   = as.Date(vapply(objs, .sina_field, character(1), field = "day")),
    open   = as.numeric(vapply(objs, .sina_field, character(1), field = "open")),
    high   = as.numeric(vapply(objs, .sina_field, character(1), field = "high")),
    low    = as.numeric(vapply(objs, .sina_field, character(1), field = "low")),
    close  = as.numeric(vapply(objs, .sina_field, character(1), field = "close")),
    volume = as.numeric(vapply(objs, .sina_field, character(1), field = "volume")),
    stringsAsFactors = FALSE
  )
  df[order(df$date), ]
}

# ---- real-time quote (Tencent) ------------------------------------------

#' Fetch a real-time quote
#'
#' Uses Tencent's public quote API. Pure base R.
#' @param code 6-digit code or exchange-prefixed code.
#' @return named list: name, price, pct_chg, open, high, low, volume, amount.
#' @export
stk_quote <- function(code) {
  px <- stk_code(code)
  txt <- .stk_get(paste0("https://qt.gtimg.cn/q=", px))
  if (is.null(txt)) {
    warning("stk_quote: failed to fetch ", code)
    return(NULL)
  }
  txt <- tryCatch(iconv(txt, from = "GBK", to = "UTF-8"), error = function(e) txt)
  f <- strsplit(sub('^v_[^=]*="', "", sub('";\\s*$', "", txt)), "~")[[1]]
  num <- function(i) suppressWarnings(as.numeric(f[i]))
  list(
    code    = gsub("\\D", "", code),
    name    = if (length(f) >= 2) f[2] else NA_character_,
    price   = num(4), prev_close = num(5), open = num(6),
    high    = num(34), low = num(35),
    pct_chg = num(33),
    volume  = num(37),            # \u624b
    amount  = num(38),            # \u4e07\u5143
    time    = if (length(f) >= 31) f[31] else NA_character_
  )
}
