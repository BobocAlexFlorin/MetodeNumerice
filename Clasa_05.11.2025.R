c<-1
dx<-1/90
dt<-0.01
si<-c*dt/dx
si
N<-200
l<-1
n<-1/dx
n

ff<-function(x)
  return<-exp(-400*(x-0.3)^2)
fg<-function(x)
  return<-0
a<-fg(0.3)
a

u0<-vector("numeric",length = n)
for(m in 1:n){
  u0[m]<-ff(m*dx)
}
u0[20]
u1<-vector("numeric", length = n)
u1[1]<-0.5*si^2*u0[2]+(1-si^2)*u0[1]+fg(dx)*dt

for(m in 2:(n-2)){
  u1[m]<-0.5*si^2*u0[m+1]+(1-si^2)*u0[m]+0.5*si^2*u0[m-1]+fg(m*dx)*dt
}
u1[n-1]<-(1-si^2)*u0[n-1]+0.5*si^2*u0[n-1]+fg((n-1)*dx)*dt

u1[20]

val_x<-vector("numeric",length = n)
for(m in 1:n) {
  val_x[m]<-m*dx
}
plot(val_x,u0,xlim=c(0,1),ylim=c(-1,1.2),type='l',xlab='x',ylab='u',las=1)
lines(val_x,u1,col=2)

#definim matricea u ce contine valorile aprox in nodurile interioare

u <- array(0, dim=c(N,n))
for(i in 1:n){
  u[1,i]<-u1[i]
}

#iteratia 2
u[2,1]<-si^2*u[1,2]+2*(1-si^2)*u[1,1]-u0[1]
for(m in 2:(n-2)){
  u[2,m] <- si^2*u[1, m+1]+2*(1-si^2)*u[1,m]+si^2*u[1,m-1]-u0[m]
}

u[2,n-1] <- 2*(1-si^2)*u[1, n-1]+si^2*u[1,n-2]-u0[n-1]

u[2,89]
u1[20]

#interatiile 3, 4, 5...N

for(j in 2:(N-1)){
  u[j+1, 1] <- si^2*u[j,2]+2*(1-si^2)*u[j,1]-u[j-1,1]
  for(m in 2:(n-2)){
    u[j+1, m]<-si^2*u[j, m+1]+2*(1-si^2)*u[j,m]+si^2*u[j,m-1]-u[j-1,m]
  }
u[j+1,n-1] <- 2*(1-si^2)*u[j,n-1]+si^2*u[j,n-2]-u[j-1,n-1]
}

u[5,8]

#graficele iteratilor

val_iter <- vector("numeric", length=n)
for(i in 1:(n)){
  val_iter[i]<-u[20, i]
}
lines(val_x, val_iter, col="3")

legend(locator(1),c("0","0.1","0,2","0.3","0.4","0.5"), col=c(1,2,3,4,5,6,7),pch=16,cex=0.7)

par(mfrow=c(2,3))

plot(val_x,u0,xlim=c(0,1),ylim=c(-0.5,1.2),type='l', xlab='x', ylab='u')
plot(val_x, val_iter, xlim=c(0,1), ylim=c(-0.5,1.2),type='l', xlab='x', ylab='u')

legend(locator(1), c("t=0.2"), col=c(1), pch=16, cex=0.5)

par(mfrow=c(2,3))
