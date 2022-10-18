rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions
setwd("C:/Users/Lenovo/Desktop/IE522/HW7/")

dat <- read.csv("ZMTSLA.csv")
t = 1/252
n = 356

###### NIG (Normal Inverse Gaussian)
## theta[1] = alpha, theta[2] = beta,theta[3] = delta, theta[4] = mu
initialvalueNIG=c(10,0,2,0)
initialvalueNIG=c(15.3997719, 4.0914736, 7.5013727, -0.7332863)
NIG=function(x,theta){theta[1]*theta[3]*t/pi*
    besselK(theta[1]*sqrt(theta[3]^2*t^2
                          +(x-theta[4]*t)^2),1)/
    (sqrt(theta[3]^2*t^2+(x-theta[4]*t)^2))*
    exp(theta[3]*sqrt(theta[1]^2-theta[2]^2)*t+
          theta[2]*(x-theta[4]*t))}
resultNIG=optim(initialvalueNIG,fn=function(theta)
{-sum(log(NIG(dat$rZM[2:357], theta)))},method="L-BFGS-B")

###### Laplace, theoretical MLE
## theta[1] = mu, theta[2] = b
laplace = function(x, theta){1/(2*theta[2]) * exp(-abs(x-theta[1])/theta[2]) }

mu_lp = median(dat$rZM, na.rm = TRUE)
b_lp = 1/n * sum(abs(dat$rZM[2:357] - mu_lp))

##### visualize
hist(dat$rZM, probability = TRUE, breaks = 30, ylim = c(0,20))
thetaNIG = resultNIG$par
thetaLAPLACE = c(mu_lp, b_lp)
curve(NIG(x, thetaNIG), add = TRUE, col = "red", lwd =3, lty=1) ## NIG
curve(laplace(x, thetaLAPLACE), add = TRUE, col = "cyan", lwd =3, lty = 3) ## Laplace
