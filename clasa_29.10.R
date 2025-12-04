l<-1
dx<- 0.005
dt<- 0.005
c<- -0.5
si<-c*dt/dx
si
n<- 1/dx
n
N <-100
#conditia initiala
ff <-function(x)
  return<- 0.4*exp(-300*(x-0.5)^2)+0.1*exp(-300*(x-0.65)^2)
u0<- vector("numeric",length=n)
for (i in 1:n){
  u0[i] <- ff(i*dx)
}
u0[100]

val_x<-vector("numeric", length = n)
for (i in 1:n){
  val_x[i]<- i*dx
}

plot(val_x,u0,xlim = c(0,1),ylim = c(-0.1,0.8),type='l',xlab ='x', ylab='',las=1)
lines(c(0,1),c(0,0), lwd=1)
#def matricea solutiilor aproximative in nodurile interioare
u<- array(0,dim=c(N,n)) 
#prima iteratie
for (i in 1:(n-1)){
  u[1,i]<- -si*u0[i+1]+(si+1)*u0[i]
}
u[1,199]

for (j in 1:(N-1)){
  for (i in 1:(n-1)){
    u[j+1,i]<- -si*u[j,i+1]+ (si+1)*u[j,i]
  }
} 
u[75,109]

val_iter<-vector("numeric", length = n)
for (i in 1:(n)){
  val_iter[i]<- u[20,i]
}

lines(val_x,val_iter,col='2')

legend(locator(1),c("t=0","t-0.1","t=0.2"), col=c(1,2,3),pch=16,cex=0.7)

#reprez sol exacta

t1<-0.2
ffe<-function(x)
  return<-0.4*exp(-300*(x-c*t1-0.5)^2)+0.1*exp(-300*(x-c*t1-0.65)^2)

sol_ex <- vector("numeric", length=n)
for(i in 1:n){
  sol_ex[i] <- ffe(i*dx)
}
sol_ex[100]
u0[100]

lines(val_x, sol_ex, col='6')

#pe de o parte sol ex dep numai de val caracteristicii ei, iar val ei este val din nodul xm - ctj
#val aprox calc cu schema numerica dep de sirul de val din nodurile coresp cond initiale
