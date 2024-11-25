#' Create discrete and continuous plots and relevant data to find the optimal tickets sold
#'
#' @param N the number of seats in the flight
#' @param gamma the probability a plane will be truly overbooked
#' @param p the probability of a passenger showing up
#'
#' @return Plots of discrete data, continuous data, and a list of the relevant data
#'
#' @examples
#' ntickets(N = 200, gamma = 0.02, p = 0.95)
#'
#' @importFrom stats optimise dnorm pbinom pnorm
#' @importFrom graphics polygon abline lines
#'
#'@export
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {

  # Discrete function for objective values
  obj_discrete <- function(n) {
    1 - gamma - pbinom(N, size = n, prob = p)
  }

  # Continuous function for objective values
  obj_continuous <- function(n) {
    mean <- n * p
    sd <- sqrt(n * p * (1 - p))
    1 - gamma - pnorm(N + 0.5, mean = mean, sd = sd)
  }

  # Function for absolute values (only needed for optimise())
  f <- function(n) {
    abs(obj_continuous(n))
  }

  # Find nd using which.min()
  n_values <- seq(N, 2*N)
  discrete_values <- sapply(n_values, obj_discrete)
  nd <- n_values[which.min(abs(discrete_values))]  # Find n with value closest to 0

  # Find nc using optimise()
  nc_optim <- optimise(f, interval = c(N, N+N*0.1))
  nc <- nc_optim$minimum

  # Plot discrete model
  plot(n_values, discrete_values, type = "b", col = "blue", pch = 16, xlim = c(N, N+N*0.1),
       xlab = "n", ylab = "Objective",
       main = paste0("Objective Vs. n to find optimal tickets sold \n (",
                     nd, ") gamma = ",
                     gamma, " N = ", N, " (Discrete)"))
  lines(n_values, discrete_values, col = "black", lty = 2, xlim = c(N, N+N*0.1)) # Plot trend of the data
  abline(h = 0, v = nd, col = "red")  # Plot intersection of nd and 0 on the graph

  # Plot continuous model
  continuous_values <- sapply(n_values, obj_continuous)
  plot(n_values, continuous_values, type = "l", col = "black", xlim = c(N, N+N*0.1),
       xlab = "n", ylab = "Objective",
       main = paste0("Objective Vs. n to find optimal tickets sold \n (",
                     nc, ") gamma = ",
                     gamma, " N = ", N, " (Continuous)"))
  abline(h = 0, v = nc, col = "blue")  # Plot intersection of nc and 0 on the graph

  # Return relevant data
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  return(result)
}
