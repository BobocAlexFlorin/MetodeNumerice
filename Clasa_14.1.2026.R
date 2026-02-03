omega<-function(x,y){
  1-x-y
}

h<-0.1

aff<-array(0,dim=c(10,10))
for(i in 1:10){
  for(j in 1:(10-i))
    aff[i,j]<-omega(i*h,j*h)
}

library(plot3D)
par(mar = c(0,0,0,2))
persp3D(z = aff)


omega2<-function(x,y){
  x
}

h2<-0.01
af2<-array(0,dim=c(100,100))
for(i in 1:100){
  for(j in 1:(100-i))
    af2[i,j]<-omega2(i*h2,j*h2)
}
persp3D(z=af)
#persp3D(z=af2, theta=90, zlim=c(0,1))
persp3D(z=af2, theta=155, zlim = c(0, 1))

omega3<-function(x,y)
  return<-y

h2<-0.01
af3<-array(0,dim=c(100,100))
for(i in 1:100){
  for(j in 1:(100-i))
    af3[i,j]<-omega3(i*h2,j*h2)
}
persp3D(z=af3)
