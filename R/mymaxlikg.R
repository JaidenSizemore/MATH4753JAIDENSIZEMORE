#' Create plot of max likelihood using a given function and theta
#'
#' @param lfun the fuction to be used
#' @param theta the theta value to be used
#'
#' @return a graph of the max likelihood and corresponding data
#'
#' @examples
#' logbin2=function(theta){log(dbinom(3,prob=theta,size=6)) + log(dbinom(5,prob=theta,size=10))}
#' mymaxlikg(theta = seq(0, 1, length=10000))
#'
#' @importFrom graphics axis
#'
#' @export

mymaxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}
