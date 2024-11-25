#' Create normal distribution curve and lower tail probability
#'
#' @param x the value such that P(Y < x)
#' @param mean the mean of the data
#' @param sd the standard deviation of the data
#'
#' @return a graph of the distribution and area
#'
#' @examples
#' myncurve(5, 10, 3)
#'
#' @importFrom graphics curve
#' @importFrom stats optimize dnorm pbinom pnorm
#' @importFrom graphics polygon abline lines
#'
#' @export
myncurve = function(x = 5, mean = 10, sd = 3) {
  curve(dnorm(x, mean = mean, sd = sd), xlim = c(mean - 3*sd, mean + 3*sd),
        ylab = "Normal Density", xlab = "X")

  xcurve1 = seq(mean - 3*sd, x, length = 1000)
  ycurve1 = dnorm(xcurve1, mean = mean, sd = sd)

  polygon(c(mean - 3*sd, xcurve1, x), c(0, ycurve1, 0), col = "Red")
  area = round(pnorm(x, mean = mean, sd = sd), 4)

  print(area)
}
