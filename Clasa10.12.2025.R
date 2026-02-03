#Minimizare si elemente finite

u_stea<-function(x)
  result<-x*(pi-x)*(x+pi-3)/6

w_stea<-function(x)
  result<-(2-4/pi)*sin(x)-0.25*sin(2*x)

varindep<-seq(0,pi,0.1)

plot(varindep, u_stea(varindep), type='l',xlab='x',ylab='u*(x)')
lines(varindep, w_stea(varindep), col='red')
legend(locator(1), c("u*", "w*"), col=c(1,2),pch=16,cex=0.8)