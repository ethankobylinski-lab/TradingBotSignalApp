#' Null coalescing helper
#'
#' @param x Primary value.
#' @param y Fallback value when `x` is NULL or length zero.
#' @return `x` when present, otherwise `y`.
#' @export
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
