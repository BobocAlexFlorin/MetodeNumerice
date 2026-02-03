# Exercitiu

l<-1
n<-10
ka<-function(x)
  return<-x+1

h <- l/n
h

#fixam nodurile interioare
varx<-vector("numeric", length=(n-1))

for(i in 1:(n-1))
{
  varx[i]=i*h
}

#Calculam elementele sj, si, bj
s<-vector("numeric", length=(n-1))
for(j in 1:(n-1)){
  s[j]<-h+(j+0.5)*h^2
}
s
s0<-h+0.5*h^2
s0
b<-h
b

#Construim Matricea K

K<-array(0, dim=c((n-1), (n-1)))
K[1,1] <- (s0+s[1])/h^2

for(i in 2:(n-1)){
  K[i,i] <- (s[i-1]+s[i])/h^2
}

for(i in 1:(n-2)){
  K[i, i+1] <- -s[i]/h^2
}

for (i in 1:(n-2)){
  K[i+1, i] <- -s[i]/h^2
}
K

bv <- vector("numeric", length=(n-1))

for(i in 1:(n-1)){
  bv[i]<-b
}
bv

#Rezolvam sistemul Kc=b
#Algoritm de eliminare Gauss cu pivotare partiala

N=(n-1)

for(k in 1:(N-1)){# primul for
  am<-abs(K[k,k])# caut cel mai mare element pe coloana k=1,2,.... de la diagonala de jos
  l<-k
  for(k1 in (k+1):N){
    if(abs(K[k1,k])>am){
      am<-abs(K[k1,k])
      l<-k1
    }# end if
  }# end for 2
  if(l>k) { # schimba randurile l cu k
    for(j1 in k:N){ # for 3, ne plimbam pe cele 2 linii
      hold<-K[k,j1]
      K[k,j1]<-K[l,j1]
      K[l,j1]<-hold
    } # end for 3
    hold<-bv[k]#schimba si elementele l cu k in vectorul fc
    bv[k]<-bv[l]
    bv[l]<-hold
  }# end if pentru permutare linii
  ### eleminarea necunoscutelor pe rand
  
  ### eleminarea necunoscutelor pe rand
  
  ### impart ecuatia k cu pivotul A[k,k]
  hold2<-K[k,k]
  bv[k]<-bv[k]/hold2
  for(j1 in k:N) ## for 4
  {
    K[k,j1]<-K[k,j1]/hold2
  } ## end for 4
  for(i1 in (k+1):N) ## for 5 ciclu dupa linile ramase
  {
    bv[i1]<-bv[i1]-K[i1,k]*bv[k]
    for(j1 in (k+1):N) ## for 6 ma plimb pe linile am mutat contrulul k->k
    {
      K[i1,j1]<-K[i1,j1]-K[i1,k]*K[k,j1] ## scad din linie i1 linia k j matricia la pivot si inmulita cu prim elem din linia j1
      
    } 
  }
}
hold<-K[N,N]
K[N,N]<-K[N,N]/hold
bv[N]<-bv[N]/hold
K

xx<-vector("numeric",length= N)
xx[N]<-bv[N]
for(i in (N-1):1)
{
  su<-0
  for(i1 in (i+1):N)
  {
    su<-su+K[i,i1]*xx[i1]
  }
  xx[i]<-bv[i]-su
}
xx

c<-vector("numeric", length=N)
for(i in 1:N){
  c[i]<-xx[i]
}
c

vx<-seq(0,1,0.01)
u<-function(x)
  return<- -x+log(x+1)/log(2)

plot(vx, u(vx),type='l', xlab='x', ylab="u(x), w(x)")
lines(varx,c,col="2")

lines(varx,c,col="3")
lines(varx,c,col="4")
legend(locator(1), c("u", "w_5", "w_10", "w_20"), col=c(1,2,3,4),pch=16,cex=0.8)

#Calculam erori in noduri
u_er <- vector("numeric", length=N)
for(i in 1:N){
  u_er[i] <- c[i]-i(i*h)
}
u_er
