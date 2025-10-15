# exercitii
# exercitiul 1

# conditii initiale
l<-1
dx<-0.01
dt<-0.00005
mu<-dt/dx^2
mu

# numar de noduri
n<-l/dx
n
n1<-(1/5)/dx
n2<-(7/10)/dx
n1
n2
N<-1000

# definire distributie initiala de temperatura
f1<-function(x)
  return<- -(x)

f2<-function(x)
  return<- x-2/5

f3<-function(x)
  return<- 1-x

piecewise_function <- function(n,dx,n1,n2,f1,f2,f3){
  x <- seq(dx, n * dx, by = dx)  # vectorul de abscise
  temp0 <- numeric(length(x))  # initializare vector rezultat temp init
  # aplica ramura corespunzatoare fiecarui interval
  for (i in seq_along(x)) {
    if (i <= n1) {
      temp0[i] <- f1(x[i])
    } else if (i <= n2) {
      temp0[i] <- f2 (x[i])
    } else {
      temp0[i] <- f3(x[i])
    }
  }
  return(temp0)
}

TempInitiala <- piecewise_function (n, dx, n1, n2, f1, f2, f3)
plot(TempInitiala, type = "l", col = "blue" , lwd = 2,
     main = "Piecewise Function = Initial Temperature" , xlab = "Index" , ylab = "Temp_0")

# grafic distributia initiala
val_x<-vector("numeric",length = n)
for (i in 1:n) {
  val_x [i] <- i*dx
}
plot(val_x,TempInitiala,xlim = c(0,1), type = 'l',xlab = 'x', ylab = 'temperatura initiala')
lines(c(0, 1), c(0, 0),lwd =2)

# conditiile la limita - omogene
alfa<-vector("numeric",length=N)
for(i in 1:N){
  alfa[i] <- 0
}

alfa[1]
alfa[2]

# frontiera dreapta
beta<-vector("numeric", length=N)
for(i in 1:N){
  beta[i]<- 0
}

beta[1]
beta[2]

# calculam valorile aperoximative ale temperaturii
u<-array(0, dim=c(1000, n-1))

u[1,1]<-(1-2*mu)*TempInitiala[1]+mu*TempInitiala[2]
u[1,1]

for (i in 2:(n-1)){
  u[1,i] <- mu*TempInitiala[i-1]+(1-2*mu)*TempInitiala[i]+mu*TempInitiala[i+1]
}

u[1,2]
u[1,3]
u[1,4]
u[1,5]
u[1,6]

u[1,n-1]

for( j in 2:N){
  u[j,1]<-(1-2*mu)*u[j-1,1]+mu*u[j-1,2]
  
  for(i in 2:(n-2)){
    u[j,i]<- mu*u[j-1, i-1]+(1-2*mu)*u[j-1,i]+mu*u[j-1,i+1]
  }
  
  u[j,n-1]<-mu*u[j-1,n-2]+(1-2*mu)*u[j-1,n-1]
}

u[1,1]
u[1,2]
u[1,3]
u[1,4]
u[1,5]
u[1,6]

# grafic la un timp oarecare

val_t<-vector("numeric", length=n)
for(i in 1:(n-1)){
  val_t[i] <- u[100, i]
}
lines(val_x, val_t, xlim =c(0,1), type="l", col='2')

#plot(val_x, val_t, xlim =c(0,1), type="l", col='2')

legend(locator(1), c("0", "10", "100", "500", "1000"), col=c(1,2,3,4,5), pch=16, cex=0.7)

SolExact<-function(t,x)
  return<-0.088*exp(-t*pi^2)*sin(x*pi)-0.192*exp(-t*4*pi^2)*sin(x*2*pi)-0.028*exp(-9*pi^2*t)*sin(3*pi*x)

val_y<-SolExact(100*dt, val_x)

lines(val_x, val_y, col=4)