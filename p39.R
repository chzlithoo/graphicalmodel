M <- array(c(15,48,18,84,21,3,1,2),
               dim=c(2,2,2),
               dimnames<-list(c("h1","h2"),c("d1","d2"),c("s1","s2"))
)

resultAry <- M 
maxiter <- 12
iter <- 0
marginChecks <- rep(1, 12)
margins <- seq(1, 12)

while(iter < maxiter) {
  for(margin in margins) { 
    marginTotal <- apply(resultAry, margin, sum) 
    marginCoeff <- Margins_[[margin]]/marginTotal 
    marginCoeff[is.infinite(marginCoeff)] <- 0 
    resultAry <- sweep(resultAry, margin, marginCoeff, "*") 
    marginChecks[margin] <- sum(abs(1 - marginCoeff)) 
  }     
  iter <- iter+1
  print("iteration:"+iter)
  print(resultAry)
}