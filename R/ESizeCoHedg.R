#' @title Calculating the Effect Size of Cohen's d & Hedges' g
#' @description \code{ESizeCoHedg} calculate the Effect Size & draw the dnorm curve
#'
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats dnorm
#' @importFrom utils read.csv
#' @importFrom graphics curve
#' @importFrom graphics legend
#' @param x1 omitted scores of group1
#' @param x2 omitted scores of group2
#' @return Effect Size of Cohen's d & Hedges' g
#' @export
#' @examples
#' # ESizeCoHedg(X1, X2)

ESizeCoHedg <- function(){

  dat <- read.csv(file.choose(), header=T)

  x1 <- na.omit(dat[,2])
  x2 <- na.omit(dat[,3])
  m1 <- mean(x1)
  m2 <- mean(x2)
  n1 <- length(x1)
  n2 <- length(x2)
  s1 <- sd(x1)
  s2 <- sd(x2)

  sp1 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2))
  d <- abs(m1 - m2)/sp1

  sp2 <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
  g <- abs(m1 - m2)/sp2

  # x-axix settings
  xmn <- min(c(m1,m2))-max(c(s1,s2))*4
  xmx <- max(c(m1,m2))+max(c(s1,s2))*4

  n <- 1000
  xlay1 <- seq(xmn, xmx, length=n)
  mx1 <- max( dnorm(xlay1,m1,s1) )
  mx2 <- max( dnorm(xlay1,m2,s2) )
  mx <- max(mx1,mx2) * 1.1

  # draw the normal distributions
  curve(dnorm(x,m1,s1),xmn,xmx,col = "blue",lwd=1,xlab="", ylab="", ylim=c(0,mx))
  curve(dnorm(x,m2,s2),xmn,xmx,add = TRUE, col = "red",lwd=1)
  legend("topleft",
         legend=c("1", "2"),
         lty=c(1,1),
         col=c("blue", "red")
  )
  return(list(Cohens_d=d, Hedges_g=g))
}

