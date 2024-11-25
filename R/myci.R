#' Create a 95 percent CI for mu
#'
#' @param x A sample
#'
#' @return A 95 percent confidence interval for the sample
#'
#' @examples
#' myci(c(1, 2, 3, 4, 5))
#'
#' @importFrom stats qt sd
#'
#' @export

myci = function(x) {
  # Plus or Minus
  pm = c(-1, 1)
  # Compute t
  t = qt(1 - 0.05/2, length(x) - 1)
  # Compute and output the interval
  mean(x) + pm * t * sd(x) / sqrt(length(x))
}
