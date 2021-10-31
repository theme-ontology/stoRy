#' @param weights A list assigning nonnegative weights to choice, major, and
#' minor theme levels. The default weighting
#' `list(choice = 3, major = 2, minor = 1)` counts each \emph{choice} usage
#' three times, each \emph{major} theme usage twice, and each \emph{minor}
#' theme usage once. Use the uniform weighting
#' `list(choice = 1, major = 1, minor = 1)` weights theme usages equally
#' regardless of level. At least one weight must be positive.
