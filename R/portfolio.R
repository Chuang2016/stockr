# stockr: portfolio construction & optimisation (pure base R, long-only)

#' Aligned log-return matrix for a set of codes
#'
#' @param codes character vector of 6-digit codes.
#' @param n history length per stock.
#' @return list(R = return matrix, prices = aligned closes, dates).
#' @export
ret_matrix <- function(codes, n = 250) {
  codes <- gsub("\\D", "", codes)
  pls <- lapply(codes, function(cd) {
    df <- stk_history(cd, n)
    if (is.null(df)) return(NULL)
    df[, c("date", "close")]
  })
  ok <- !vapply(pls, is.null, logical(1))
  pls <- pls[ok]; codes <- codes[ok]
  dates <- as.Date(Reduce(intersect, lapply(pls, `[[`, "date")),
                  origin = "1970-01-01")
  dates <- sort(dates)
  px <- do.call(cbind, lapply(pls, function(d) {
    d$close[match(dates, d$date)]
  }))
  colnames(px) <- codes
  R <- apply(log(px), 2, diff)
  list(R = R, prices = px, dates = dates[-1])
}

#' Annualised per-asset statistics
#' @param R return matrix (daily). @param rf annual risk-free rate.
#' @return data.frame(code, ann_ret, ann_vol, sharpe).
#' @export
ann_stats <- function(R, rf = 0.02) {
  data.frame(
    code = colnames(R),
    ann_ret = apply(R, 2, mean, na.rm = TRUE) * 250,
    ann_vol = apply(R, 2, stats::sd, na.rm = TRUE) * sqrt(250),
    stringsAsFactors = FALSE
  ) -> s
  s$sharpe <- (s$ann_ret - rf) / s$ann_vol
  s
}

.rand_w <- function(k) {
  x <- stats::rexp(k); x / sum(x)
}

.port_eval <- function(w, mu, Sg, rf) {
  rp <- sum(w * mu); vp <- as.numeric(t(w) %*% Sg %*% w)
  c(ret = rp, vol = sqrt(vp), sharpe = (rp - rf) / sqrt(vp))
}

#' Maximum-Sharpe long-only portfolio (random search)
#' @param R daily return matrix. @param rf annual risk-free rate.
#' @param n_iter random portfolios. @param seed RNG seed.
#' @return list(weights, ret, vol, sharpe).
#' @export
opt_max_sharpe <- function(R, rf = 0.02, n_iter = 20000, seed = 42) {
  set.seed(seed)
  mu <- colMeans(R, na.rm = TRUE) * 250
  Sg <- stats::cov(R, use = "pairwise.complete.obs") * 250
  best <- list(sharpe = -Inf)
  for (i in seq_len(n_iter)) {
    w <- .rand_w(ncol(R))
    e <- .port_eval(w, mu, Sg, rf)
    if (e["sharpe"] > best$sharpe)
      best <- list(weights = w, ret = e["ret"], vol = e["vol"], sharpe = e["sharpe"])
  }
  names(best$weights) <- colnames(R)
  best
}

#' Minimum-variance long-only portfolio (random search)
#' @param R daily return matrix. @param n_iter,seed as in opt_max_sharpe.
#' @return list(weights, ret, vol, sharpe).
#' @export
opt_min_var <- function(R, rf = 0.02, n_iter = 20000, seed = 42) {
  set.seed(seed)
  mu <- colMeans(R, na.rm = TRUE) * 250
  Sg <- stats::cov(R, use = "pairwise.complete.obs") * 250
  best <- list(vol = Inf)
  for (i in seq_len(n_iter)) {
    w <- .rand_w(ncol(R))
    e <- .port_eval(w, mu, Sg, rf)
    if (e["vol"] < best$vol)
      best <- list(weights = w, ret = e["ret"], vol = e["vol"],
                   sharpe = (e["ret"] - rf) / e["vol"])
  }
  names(best$weights) <- colnames(R)
  best
}

#' Risk-parity long-only portfolio (cyclical coordinate descent)
#' @param R daily return matrix. @param max_iter iterations. @param tol tolerance.
#' @return list(weights, ret, vol, sharpe).
#' @export
opt_risk_parity <- function(R, rf = 0.02, max_iter = 100, tol = 1e-8) {
  Sg <- stats::cov(R, use = "pairwise.complete.obs") * 250
  mu <- colMeans(R, na.rm = TRUE) * 250
  k <- ncol(R)
  w <- rep(1 / k, k)
  for (it in seq_len(max_iter)) {
    w_old <- w
    mrc <- as.numeric(Sg %*% w)          # marginal risk contribution
    rc <- w * mrc                         # risk contributions
    target <- sum(rc) / k
    w <- w * sqrt(target / pmax(rc, 1e-12))
    w <- w / sum(w)
    if (max(abs(w - w_old)) < tol) break
  }
  e <- .port_eval(w, mu, Sg, rf)
  names(w) <- colnames(R)
  list(weights = w, ret = unname(e["ret"]), vol = unname(e["vol"]),
       sharpe = unname(e["sharpe"]))
}

#' Equal-weight portfolio
#' @param R daily return matrix (only column names used).
#' @return list(weights, ...).
#' @export
opt_equal <- function(R, rf = 0.02) {
  k <- ncol(R)
  w <- rep(1 / k, k); names(w) <- colnames(R)
  mu <- colMeans(R, na.rm = TRUE) * 250
  Sg <- stats::cov(R, use = "pairwise.complete.obs") * 250
  e <- .port_eval(w, mu, Sg, rf)
  list(weights = w, ret = unname(e["ret"]), vol = unname(e["vol"]),
       sharpe = unname(e["sharpe"]))
}

#' Grid-search tuning of trend-following parameters
#'
#' Walk-forward style: first 60% of history tunes, last 40% validates.
#' @param code 6-digit code. @param fast_grid,slow_grid candidate windows.
#' @param n history length.
#' @return list(best = c(fast, slow), train_sharpe, test_sharpe, grid).
#' @export
tune_trend <- function(code, fast_grid = c(5, 10, 15),
                       slow_grid = c(20, 30, 60), n = 500) {
  df <- stk_history(gsub("\\D", "", code), n)
  if (is.null(df) || nrow(df) < 200) stop("tune_trend: not enough history")
  cut <- floor(nrow(df) * 0.6)
  grid <- expand.grid(fast = fast_grid, slow = slow_grid)
  grid <- grid[grid$fast < grid$slow, ]
  ev <- function(d, f, s) {
    sig <- as.numeric(d$close > ma(d$close, s) & ma(d$close, f) > ma(d$close, s))
    sig[is.na(sig)] <- 0
    r <- c(NA, diff(log(d$close)))
    sr <- sig * r
    m <- mean(sr, na.rm = TRUE); v <- stats::sd(sr, na.rm = TRUE)
    if (is.na(v) || v == 0) return(NA_real_)
    m / v * sqrt(250)
  }
  grid$train <- mapply(function(f, s) ev(df[1:cut, ], f, s),
                       grid$fast, grid$slow)
  ok <- !is.na(grid$train)
  if (!any(ok)) stop("tune_trend: no valid parameter combo")
  bi <- which.max(grid$train)
  bf <- grid$fast[bi]; bs <- grid$slow[bi]
  list(best = c(fast = bf, slow = bs),
       train_sharpe = grid$train[bi],
       test_sharpe = ev(df[(cut + 1):nrow(df), ], bf, bs),
       grid = grid[ok, ])
}
