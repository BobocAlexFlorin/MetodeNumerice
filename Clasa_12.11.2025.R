m<-4 #nr noduri pe x
n<-4 #nr noduri pe y

dx<- 1/4
dy<- 1/4

ro<-dx/dy

#definim functia f(x,y) din ec. Poission
ff<-function(x,y)
  return(pi*pi*y*sin(pi*x))

#definim functia pentru frontiera
fg<-function(x)
  return(sin(pi*x))

#matricea sistemului
N<-(m-1)*(n-1)
A <- array(0,dim=c(N,N))

#calculam elemtele matricii A cu N=9
for(i in 1:N){
  A[i,i]=2*(1+ro^2)
}

for(i in 1:(N-1)){
  A[i, i+1] <- -1
}

#corectam cu 0 din 3 in 3
for(i in 1:floor((N-1)/3)){
  A[3*i, 3*i+1]<-0
}

for(i in 1:(N-1)){
  A[i+1, i] <- -1
}

for(i in 1:floor((N-1)/3)){
  A[3*i+1, 3*i]<-0
}
A

#punem -1 pe a 3-a diagonala
for(i in 1:(N-3)){
  A[i, i+3]<--ro^2
}
A
for(i in 1:(N-3)){
  A[i+3, i]<- -ro^2
}
A


f<-array(0, dim=c((m-1), (n-1)))
for(i in 1:(m-1)){
  for(j in 1:(n-1)){
    f[i,j] <- ff(i*dx, j*dy)
  }
}
f

library(plot3D)
persp3D(z = f)
dx
dy

#vectorul cu termenii din dreapta si cond pe frontiera
fc<-vector("numeric", length=(N))

#calculam fc general
for(j in 1:(n-2)){
  for(i in 1:(m-1)){
    fc[3*(j-1)+i]<-f[i,j]*dx^2
  }
}
fc

for(i in 1:3){
  fc[2*3+i]<-f[i,3]*dx^2+fg(i*dx)*ro^2
}
fc

fc<-vector("numeric", length=(N))

#Algoritm de eliminare Gauss cu pivotare partiala
##N=(m-1)(n-1)
N
for(k in 1:(N-1)){
  am<-abs(A[k,k])
  l<-k
  for(k1 in (k+1):N){
    if(abs(A[k1, k])>am){
      am<-abs(A[k1,k])
      l<-k1
    }
  }
  if(l>k){
    for(j1 in k:N){
      hold<-A[k,j1]
      A[k,j1]<-A[l, j1]
      A[l, j1] <- hold
    }
    
    hold <-fc[k]
    fc[k]<-fc[l]
    fc[l]<-hold
  }
  
  #eliminarea necunoscutelor
  hold2<-A[k,k]
  if(hold2 == 0) {
    stop("Division by zero encountered during Gaussian elimination. Matrix A might be singular.")
  }
  fc[k]<-fc[k]/hold2
  for(j1 in k:N){
    A[k, j1]<-A[k,j1]/hold2 
  }
  
  for(i1 in (k+1):N){
    fc[i1]<-fc[i1]-A[i1,k]*fc[k]
    for(j1 in (k+1):N){
      A[i1, j1]<-A[i1,j1]-A[i1, k]*A[k,j1]
    }
  }
}

hold<-A[N,N]
if(hold == 0) {
  stop("Division by zero encountered for the last element in Gaussian elimination. Matrix A might be singular.")
}
fc[N]<-fc[N]/hold
A
fc

#substitutia inversa
xx<-vector("numeric", length=N)
xx[N]<-fc[N]
xx[N]
for (i in (N-1):1){
  su<-0
  for(i1 in (i+1):N){
    su<-su+A[i,i1]*xx[i1]
  }
  xx[i] <- fc[i]-su
}
xx

u<-array(0, dim=c((m-1),(n-1)))

#calculam u mai general
for(j in 1:(n-1)){
  for(i in 1:(m-1)){
    u[i,j]<-xx[3*(j-1)+i]
  }
}

library(plot3D) 
par(mar = c(0,0,0,2))
persp3D(z = u)
persp3D(z = u, theta = 45, zlim = c(0,1)) 
u

#def solutie exacta
udexy<-function(x,y)
  return<-y*sin(pi*x)

u_analitic <- array(0, dim=c((m-1),(n-1)))
for(i in 1:(m-1)){
  for(j in 1:(n-1)){
    u_analitic[i,j]<-udexy(i*dx, j*dy)
  }
}

u_analitic[2,2]
persp3D(z=u_analitic)


#erorile
u_er <- array(0, dim=c((m-1),(n-1)))
for(i in 1:(m-1)){
  for(j in 1:(n-1)){
    u_er[i,j]<-u_analitic[i,j]-u[i,j]
  }
}

max(u_er)
min(u_er)

u_er
