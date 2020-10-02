
calc_quants_prev <- function(x, var, at = 520, mult = 1, round = 1, qnt.low = 0.025, qnt.high = 0.975) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(x$epi[[var]][at, ])*mult
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}

calc_quants_ir <- function(x, var, qnt.low = 0.025, qnt.high = 0.975, round = 2) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(colMeans(tail(x$epi[[var]], 52)))
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  out <- sprintf("%.2f", out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}

calc_quants_ci <- function(x, var, mult = 1,
                           qnt.low = 0.025, qnt.high = 0.975, round = 2) {
  if (is.null(x$epi[[var]])) {
    stop("var ", var, " does not exist on x", call. = FALSE)
  }
  out <- as.numeric(colSums(x$epi[[var]]))*mult
  out <- quantile(out, c(0.5, qnt.low, qnt.high), names = FALSE)
  format <- paste0("%.", round, "f")
  out <- sprintf(format, out)
  out <- paste0(out[1], " (", out[2], ", ", out[3], ")")
  return(out)
}

