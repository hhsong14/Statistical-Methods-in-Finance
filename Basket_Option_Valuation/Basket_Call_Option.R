rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions

rho = 0.1
T = 1
r = 0.01
K = 250

S10 = 100
q1 = 0
vol1 = 0.5
sigma1 = 0.5*sqrt(T)

S20 = 150
q2 = 0.03
vol2 = 0.4
sigma2 = 0.4*sqrt(T)

BasketCallOption = function(n, alpha){
  X = 0
  Y = 0
  for (i in 1:n) {
    z1 = rnorm(5000)
    z = rnorm(5000)
    z2 = rho*z1 + sqrt(1-rho^2)*z
    
    S1T = S10*exp((r-q1-0.5*vol1^2)*T + vol1*sqrt(T)*z1)
    S2T = S20*exp((r-q2-0.5*vol2^2)*T + vol2*sqrt(T)*z2)
    samp = sample(S1T+S2T-K,1)
    
    X_temp = exp(-r*T)*max(samp, 0)
    X = (1-1/i)*X + 1/i * X_temp
    
    Y = (1-1/i)*Y + 1/i *X_temp^2 
    
  }
  se = (1/(n-1) * (Y-X^2))^(1/2)
  
  lb = X - qnorm(1-alpha/2)*se
  ub = X + qnorm(1-alpha/2)*se

  result = data.frame(X,se,lb,ub)
  return(result)
}



