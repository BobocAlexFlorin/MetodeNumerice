# ---------- 1. Funcții ----------
u1 <- function(x) log(x)
u2 <- function(x) cos(x)

# Derivate exacte
du1_exact <- function(x) 1/x
du2_exact <- function(x) -sin(x)
d2u1_exact <- function(x) -1/x^2

# Pași
h <- c(0.1, 0.01, 0.001)
x0 <- 1

# ---------- 1. Diferență progresivă ----------
forward_diff <- function(u, x, h) (u(x + h) - u(x)) / h

# ---------- 2. Diferență centrată ----------
central_diff <- function(u, x, h) (u(x + h) - u(x - h)) / (2 * h)

# ---------- 3. Derivata a doua centrată ----------
second_central <- function(u, x, h) (u(x + h) - 2*u(x) + u(x - h)) / h^2

# ---------- 4. Formula cu u(x), u(x+h), u(x+3h) ----------
approx_custom <- function(u, x, h) (-3*u(x) + 4*u(x + h) - u(x + 3*h)) / (2*h)

# ---------- Calcul numeric ----------
results <- data.frame(
  h = h,
  ln_forward = sapply(h, function(h) forward_diff(u1, x0, h)),
  ln_central = sapply(h, function(h) central_diff(u1, x0, h)),
  ln_second = sapply(h, function(h) second_central(u1, x0, h)),
  cos_forward = sapply(h, function(h) forward_diff(u2, x0, h)),
  cos_central = sapply(h, function(h) central_diff(u2, x0, h)),
  cos_custom = sapply(h, function(h) approx_custom(u2, x0, h))
)

print(round(results, 6))

# ---------- Erori relative ----------
exact_ln1 <- du1_exact(1)
exact_cos1 <- du2_exact(1)
exact_ln2 <- d2u1_exact(1)

error_ln_forward <- abs(results$ln_forward - exact_ln1)
error_ln_central <- abs(results$ln_central - exact_ln1)
error_ln_second <- abs(results$ln_second - exact_ln2)
error_cos_forward <- abs(results$cos_forward - exact_cos1)
error_cos_central <- abs(results$cos_central - exact_cos1)
error_cos_custom <- abs(results$cos_custom - exact_cos1)

cbind(h, error_ln_forward, error_ln_central, error_ln_second,
      error_cos_forward, error_cos_central, error_cos_custom)
