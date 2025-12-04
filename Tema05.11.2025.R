# Pentru a) si b)

# Parametri
a <- 3# viteza
dx <- 0.1
x <- seq(-10, 10, by = dx)
dt <- 0.03  # astfel incat sigma = a*dt/dx = 0.9 < 1
sigma <- a * dt / dx
tmax <- 1.5
nt <- ceiling(tmax / dt)

# Condiția inițială
u <- 1 / (1 + x^2)

# Stocăm soluțiile la momentele dorite
times <- c(0, 0.5, 1, 1.5)
sol <- list()
sol[[1]] <- u

# Evoluția în timp (schema upwind)
for (n in 1:nt) {
  u_new <- u
  for (i in 2:length(x)) {
    u_new[i] <- u[i] - sigma * (u[i] - u[i-1])
  }
  u <- u_new
  if (abs(n * dt - times[length(sol)+1]) < dt/2)
    sol[[length(sol)+1]] <- u
}

# Soluția exactă
uexact <- 1 / (1 + (x - 3*1.5)^2)

# Plot
par(mfrow=c(1,2))
matplot(x, do.call(cbind, sol), type='l', lwd=2, lty=1,
        main='Soluții numerice la momente diferite',
        xlab='x', ylab='u(x,t)')

plot(x, sol[[length(sol)]], type='l', lwd=2, col='blue',
     main='Comparatie la t=1.5',
     xlab='x', ylab='u')
lines(x, uexact, col='red', lwd=2, lty=2)


# pentru c)

# Cazul neuniform
dx <- 0.1
x <- seq(-10, 10, by=dx)
dt <- 0.02
c <- 4 / (1 + x^2)
u <- 1 / (1 + x^2)
tmax <- 1.5
nt <- ceiling(tmax / dt)
times <- c(0, 0.5, 1, 1.5)
sol2 <- list(u)

for (n in 1:nt) {
  u_new <- u
  for (i in 2:length(x)) {
    sigma <- c[i] * dt / dx
    u_new[i] <- u[i] - sigma * (u[i] - u[i-1])
  }
  u <- u_new
  if (abs(n * dt - times[length(sol2)+1]) < dt/2)
    sol2[[length(sol2)+1]] <- u
}

matplot(x, do.call(cbind, sol2), type='l', lwd=2, lty=1,
        main='Transport neuniform',
        xlab='x', ylab='u(x,t)')
