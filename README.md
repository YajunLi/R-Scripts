numint=function(a,b,l,m,N){  # Monte Carlo method in use
  # N is the number of random generating, a,b,l,m are the bounds of area 
  
  set.seed(100)   # it is optional, the purpose is to get the same number if rerun the code
  area<-(b-a)*(m-l)
  x<-runif(N,a,b)
  y<-runif(N,l,m)
  isbelow<-y<(1/sqrt(2*pi))*exp(-x^2/2)  # don't know if there is inline function as in Matlab
  fraction<-sum(isbelow)/N
  ans<-fraction*area
  return(ans)
  
}
