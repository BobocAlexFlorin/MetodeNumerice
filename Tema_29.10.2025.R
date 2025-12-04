# Parametrii
L <- 1
dx <- 0.1
x <- seq(-L, L, by = dx)
Nx <- length(x)
dt <- 0.005  # pentru schema explicită (stabila)
r <- dt / dx^2

# Condiție inițială
u0 <- abs(x)^(1/2) - x^2
u0[x == -L | x == L] <- 0

# Timpuri de ieșire
times <- c(0, 0.01, 0.02, 0.05, 0.1, 1)
Nt <- ceiling(max(times) / dt)

# ---------------- SCHEMA EXPLICITA ----------------
u <- u0
results_explicit <- list(t0 = u)

for (n in 1:Nt) {
  unew <- u
  for (i in 2:(Nx - 1)) {
    unew[i] <- u[i] + r * (u[i+1] - 2*u[i] + u[i-1])
  }
  unew[1] <- 0
  unew[Nx] <- 0
  u <- unew
  if (any(abs(n*dt - times) < 1e-6)) results_explicit[[paste0("t", round(n*dt, 3))]] <- u
}

# ---------------- SCHEMA IMPLICITA (BTCS) ----------------
dx <- 0.1
dt <- 0.1  # poate fi mai mare
r <- dt / dx^2

# Matrice tridiagonala
A <- matrix(0, nrow = Nx-2, ncol = Nx-2)
diag(A) <- 1 + 2*r
diag(A[-1, ]) <- -r
diag(A[, -1]) <- -r

u <- u0
results_implicit <- list(t0 = u)
Nt_imp <- ceiling(max(times) / dt)

for (n in 1:Nt_imp) {
  b <- u[2:(Nx-1)]
  u_new_inner <- solve(A, b)
  u <- c(0, u_new_inner, 0)
  if (any(abs(n*dt - times) < 1e-6)) results_implicit[[paste0("t", round(n*dt, 3))]] <- u
}

# ---------------- VIZUALIZARE ----------------
plot(x, results_explicit$t0, type='l', col='red', ylim=c(-1,1),
     ylab="u(x,t)", xlab="x", main="Comparatie schema explicita/implicita",
     lwd=1.5)
lines(x, results_implicit$t0, col='blue', lwd=1.5)
legend("topright", legend=c("Explicita","Implicita"), col=c("red","blue"), lty=1, lwd=1.5)

