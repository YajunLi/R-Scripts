# generate a series of simulated c,s and x data
# rho is a measure of curvature of consumption function 
# as rho tends to zero, curvature is dampened
# RS0 is period zero total saving, it serves as an initial condition for period one
# n is the number of period
# R is gross interest rate
# sig is the standard deviation of income process

Sim=function(RS0,R,rho,n,sig){
 
  X<-c(1:n) # or a matrix form x<-matrix(NA,n,1) 
            # if three dimention, see i.e. a<-array(1:20,dim = c(5,5,5))
  C<-c(1:n) # the same as above
  y<-rnorm(n,100,sig) # to simulate the process of income in each period n
                     # may also try y<-runif(n,0,100)
  x<-RS0             # the first realization
  c<-0
  
  for (i in 1:n){
    
    x<-R*(x-c)+y[i]
    
    if (x<99|x==99){ # to compute consumption c
      c<-x
    } else {
      c<-99+(log(1+rho*x-99)) # alternatively, try c<90+(x-90)^(1-rho)/(1-rho)
    }  
    
    x<-x             # to update the variables
    c<-c
    X[i]<-x          
    C[i]<-c
     
  } 
  X<-X-C
  t<-matrix(1:n,n,1)
  return(data.frame(t,X,C,y))
  # or return(data.frame(matrix(cbind(X,C,y),n,3)))
}
  # to plot them in one graph

 #yrange<-range(X,C,y)
 #plot(X,ylim = yrange,type="l",col="darkgreen",xlab="periods",ylab="X C y",main="Simulation of income, consumption, and asset with i.i.d. normal uncertainty of income")
 #lines(C,col="firebrick")
 #lines(y,col="darkslateblue")
 
 
 
 