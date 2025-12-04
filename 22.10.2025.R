# # ------------------------------
# # ECUAȚIA DIFUZIEI: ∂u/∂t = 5 ∂²u/∂x²
# # Schema explicită (FTCS)
# # ------------------------------
#
# # Ce face codul
#
# # 1. Rezolva PDE-ul cu conditiile initiale
# # 2. Foloseste schema explicita FTCS
# # 3. Reprezinta grafic solutiile functilor in functie de domeniul interzis
# 
# # Parametrii
# alpha <- 5        # coeficientul de difuzie
# L <- 3            # lungimea domeniului
# dx <- 0.1         # pas spațial
# x <- seq(0, L, by = dx)
# N <- length(x)
# 
# Condiție inițială
# u0 <- function(x) { x * (x - 1) * (x - 3) }
# 
# simulate_FTCS <- function(dt, t_max, plot_times) {
#   r <- alpha * dt / dx^2
#   if (r > 0.5) {
#     cat("*** ATENȚIE: r =", r, "> 0.5 -> schema va fi instabilă ***\n")
#   } else {
#     cat("r =", r, "(stabil)\n")
#   }
#   
#   u <- u0(x)
#   u[1] <- 0
#   u[N] <- 0
#   times <- c(0)
#   snapshots <- list(u)
#   
#   t <- 0
#   nmax <- ceiling(t_max / dt)
#   
#   for (n in 1:nmax) {
#     un <- u
#     u[2:(N-1)] <- un[2:(N-1)] + r * (un[3:N] - 2 * un[2:(N-1)] + un[1:(N-2)])
#     u[1] <- 0
#     u[N] <- 0
#     t <- t + dt
#     # salvăm la timpii doriți
#     if (any(abs(t - plot_times) < dt / 2)) {
#       times <- c(times, t)
#       snapshots[[length(times)]] <- u
#     }
#   }
#   return(list(times = times, snapshots = snapshots, r = r))
# }
# 
# # Momente la care vrem grafice
# plot_times <- c(0.0, 0.01, 0.05, 0.1)
# 
# # --- Caz stabil ---
# dt_good <- 0.0005   # Δt permis (r = 0.25)
# tmax <- 0.2
# result_good <- simulate_FTCS(dt_good, tmax, plot_times)
# 
# # --- Caz instabil ---
# dt_bad <- 0.002     # Δt interzis (r = 1.0)
# result_bad <- simulate_FTCS(dt_bad, tmax, plot_times)
# 
# ------------------------------
#Funcție pentru afișarea graficelor
#------------------------------
# plot_snapshots <- function(x, result, title_prefix) {
# colors <- rainbow(length(result$times))
# plot(x, result$snapshots[[1]], type = "l", lwd = 2, col = colors[1],
# ylim = range(unlist(result$snapshots)),
# xlab = "x", ylab = "u(x,t)",
# main = sprintf("%s (r = %.3f)", title_prefix, result$r))
# for (i in 2:length(result$times)) {
# lines(x, result$snapshots[[i]], lwd = 2, col = colors[i])
#   }
#   legend("topright", legend = sprintf("t = %.4f", result$times),
#     col = colors, lwd = 2)
#   grid()
# }
# ------------------------------
# Grafice
#------------------------------
#par(mfrow = c(2, 1))
#plot_snapshots(x, result_good, sprintf("Solutie FTCS stabila, dt = %.4f", dt_good))
#plot_snapshots(x, result_bad, sprintf("Solutie FTCS instabila, dt = %.4f", dt_bad))

#Clasa

#Exercitiu - clasa - Ecuatia Caldurii rezolvata cu cursul 4 - Metoda Implicita

###curs 4
l<-1
dx<-0.01
dt<-0.0005
mu<-dt/dx^2
mu
## numar noduri pe directia x
n<-l/dx
n
n1<-(1/5)/dx
n1
n2<-(7/10)/dx
n2


####numar noduri pe timp
N<-100

###def distributia initiala de temperatura
f1<-function(x)
  return<- -x
f2<-function(x)
  return<- x-2/5

f3<-function(x)
  return<- 1-x

####sau mai elegant
piecewise_function <- function(n, dx, n1, n2, f1, f2, f3) {
  
  x <- seq(dx, n * dx, by = dx)   # vectorul cu abscise
  temp0 <- numeric(length(x))     # inițializare vector rezultat temp init
  
  # Aplica ramura corespunzătoare fiecărui interval
  for (i in seq_along(x)) {
    if (i <= n1) {
      temp0[i] <- f1(x[i])
    } else if (i <= n2) {
      temp0[i] <- f2(x[i])
    } else {
      temp0[i] <- f3(x[i])
    }
  } # inchide for
  
  return(temp0)
}


n
dx
n1
n2

TempInitiala <- piecewise_function(n, dx, n1, n2, f1, f2, f3)

plot(TempInitiala, type = "l", col = "blue", lwd = 2,
     main = "Piecewise Function = Initial Temperature", xlab = "Index", ylab = "Temp_0")

##grafic distributia inititiala
val_x<-vector("numeric",length = n)
for (i in 1:n) {
  val_x[i] <- i*dx
}

plot(val_x,TempInitiala,xlim = c(0,1),type = 'l',xlab = 'x',ylab = 'temperatura')

lines(c(0,1),c(0,0))

#valori in noduri interioare domeniului
u <- array(0,dim=c(N,n-1))

a<-vector("numeric",length = (n-1))
b<-vector("numeric",length = (n-1))
c<-vector("numeric",length = (n-1))
d<-vector("numeric",length = (n-1))

for (i in 2:(n-1))
{
  a[i]<--mu
}
a[3]

for(i in 1:(n-1))
{
  b[i]<-1+2*mu
}
b[3]

for(i in 1:(n-1))
{
  c[i]<--mu
}
c[3]

for(i in 1:(n-1))
{
  d[i]<-TempInitiala[i]
}
d[3]

beta<-b[1]
beta

c[1]
c[1]<-c[1]/beta
c[1]

d[1]
d[1]<-d[1]/beta
d[1]

for(i in 2:(n-2))
{
  beta<-b[i]-a[i]*c[i-1]
  c[i]<-c[i]/beta
  d[i]<-(d[i]-a[i]*d[i-1])/beta
}
d[n-1]<-(d[n-1]-a[n-1]*d[n-2])/(b[n-1]-a[n-1]*c[n-2])


for(i in (n-2):1)
{
  d[i]<-d[i]-c[i]*d[i+1]
}
d


val_d<-vector("numeric", length = n)
for(i in 1:(n-1))
{
  val_d[i]<-d[i]
}
lines(val_x, val_d, col='2')

for(i in 1:(n-1))
{
  u[1,i]<-d[i]
}


for(j in 2:N){
  beta<-b[1]
  c[1]<-c[1]/beta
  d[1]<-d[1]/beta
  
  for(i in 2:(n-2)){
    beta<-b[i]-a[i]*c[i-1]
    c[i]<-c[i]/beta
    d[i]<-(d[i]-a[i]*d[i-1])/beta
  }
  
  d[n-1]<-(d[n-1]-a[n-1]*d[n-2])/(n[n-1]-a[n-1]*c[n-2])
  
  for(i in (n-2):1){
    d[i]<-d[i]-c[i]*d[i+1]
  }
  
for(k in 1:(n-1)){
  u[j,k]<-d[k]
  }
}

val_iter<-vector("numeric", length=n)
for(i in 1:(n-1)){
  val_iter[i] <- u[2,i]
}

lines(val_x, val_iter, col="8")
lines(c(0, 1), c(0, 0), lwd = 1)
