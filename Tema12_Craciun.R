# Parametrii problemei
L <- 2
n_elements <- 20 # Poți modifica aici pentru w_10 sau w_20
h <- L / n_elements
nodes <- seq(0, L, by = h)
n_nodes <- length(nodes)
n_inner <- n_nodes - 2 # Nodurile interioare (fără 0 și L)

# 1. Soluția Exactă
u_exact_func <- function(x) {
  c1 <- (exp(2) + 1) / (exp(2) - 1)
  c2 <- -2 * exp(2) / (exp(2) - 1)
  return((c1 + 1 - x) * exp(x) + c2)
}

# 2. Construirea matricelor MEF (Sistemul K*u = F)
K <- matrix(0, n_inner, n_inner)
F_vec <- numeric(n_inner)

for (i in 1:n_inner) {
  # Nodul curent în contextul matricii este i, dar în contextul total este i+1
  xi <- nodes[i+1]
  
  # Vectorul forțelor (F_i = integrala funcției hat phi_i)
  F_vec[i] <- h 
  
  # Matricea de rigiditate K (calculată prin integrare pe elemente)
  # Aproximăm e^(-x) prin valoarea în mijlocul fiecărui sub-element
  
  # Elementul din stânga [x_{i}, x_{i+1}]
  mid_left <- xi - h/2
  K[i, i] <- K[i, i] + (1/h^2) * exp(-mid_left) * h
  
  # Elementul din dreapta [x_{i+1}, x_{i+2}]
  mid_right <- xi + h/2
  K[i, i] <- K[i, i] + (1/h^2) * exp(-mid_right) * h
  
  # Cuplajul cu nodul din stânga
  if (i > 1) {
    K[i, i-1] <- -(1/h^2) * exp(-mid_left) * h
  }
  
  # Cuplajul cu nodul din dreapta
  if (i < n_inner) {
    K[i, i+1] <- -(1/h^2) * exp(-mid_right) * h
  }
}

# 3. Rezolvarea sistemului pentru nodurile interioare
u_inner <- solve(K, F_vec)
u_approx <- c(0, u_inner, 0) # Adăugăm condițiile la limită u(0)=0, u(2)=0

# 4. Plotare și Comparație
x_fine <- seq(0, L, length.out = 200)
plot(x_fine, u_exact_func(x_fine), type = "l", lwd = 2, 
     col = "black", xlab = "x", ylab = "u(x), w(x)", 
     main = "Comparație Soluție Exactă vs MEF")
lines(nodes, u_approx, col = "red", type = "b", pch = 19)
legend("bottomleft", legend = c("Exactă", paste("MEF (n=", n_elements, ")", sep="")), 
       col = c("black", "red"), lty = 1, pch = c(NA, 19))
