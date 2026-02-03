                                                                                     exact
#                                                                                            ​
# PARTEA A III-a
# --- REZOLVARE PROBLEMA 1 (Verificare Numerică) ---

# 1. Definim parametrii rețelei
n <- 100            # Numărul de intervale
L <- 1              # Lungimea domeniului


h <- L / n          # Pasul spațial
x <- seq(0, L, by=h) # Nodurile rețelei

# 2. Definim termenul sursă f(x) = x
f_func <- function(x) { x }

# 3. Definim Soluția Exactă u*(x) = -x^3/6 + x/6 (din Curs 11)
u_exact <- -(x^3)/6 + x/6

# 4. Definim o funcție perturbată (o funcție care respectă condițiile la limită, dar nu e soluția)
# Exemplu: u_pert = u_exact + sin(pi*x) * 0.1
perturbare <- sin(pi * x) * 0.1
u_perturbat <- u_exact + perturbare

# 5. Definim Funcționala de Energie Q[u] (forma discretizată)
# Q[u] = Integral( 0.5*(u')^2 - f*u ) dx
calculeaza_energie <- function(u_vec, f_vec, h) {
  n <- length(u_vec)
  
  # a) Calculăm derivata u' folosind diferențe finite
  # u'(x) ~ (u_{i+1} - u_i) / h
  du_dx <- diff(u_vec) / h
  
  # b) Integrala termenului 1: 0.5 * (u')^2
  # Sumăm (0.5 * du^2) * h
  termen_energie_elastica <- sum(0.5 * du_dx^2) * h
  
  # c) Integrala termenului 2: f * u
  # Aproximăm folosind regula trapezului sau sumă simplă pe noduri interioare
  termen_lucru_mecanic <- sum(f_vec * u_vec) * h
  
  # Q total
  Q <- termen_energie_elastica - termen_lucru_mecanic
  return(Q)
}

# Calculăm valorile sursei pe grilă
f_valori <- f_func(x)

# 6. Comparăm Energiile
Q_exact <- calculeaza_energie(u_exact, f_valori, h)
Q_perturbat <- calculeaza_energie(u_perturbat, f_valori, h)

cat("=== REZULTATE PROBLEMA 1 ===\n")
cat("Energia soluției exacte Q(u*):    ", Q_exact, "\n")
cat("Energia soluției perturbate Q(v): ", Q_perturbat, "\n")

if (Q_exact < Q_perturbat) {
  cat("CONCLUZIE: Principiul Dirichlet verificat! Soluția exactă are energia MINIMĂ.\n")
} else {
  cat("Ceva nu a funcționat.\n")
}

# Vizualizare
plot(x, u_perturbat, type="l", col="red", lty=2, ylab="u(x)", main="Comparatie Solutii")
lines(x, u_exact, col="blue", lwd=2)
legend("topright", legend=c("Exact (Minim)", "Perturbat"), col=c("blue", "red"), lty=1:2)


  #Problema 2: Metoda Elementului Finit

# 1. Definim parametrii generici
h <- 0.5   # Alegem un pas arbitrar (rezultatul simbolic trebuie să fie 2/h = 4)
x_i <- 1.0 # Poziția nodului i

# Nodurile vecine
x_im1 <- x_i - h
x_ip1 <- x_i + h

# 2. Definim Derivata funcției Hat (phi_i')
# Conform teoriei:
# Pe [x_{i-1}, x_i]: panta este +1/h
# Pe [x_i, x_{i+1}]: panta este -1/h
# În rest: 0

derivata_phi <- function(x, x_prev, x_curr, x_next, h) {
  # Folosim Vectorize pentru a permite integrarea în R
  val <- numeric(length(x))
  for(k in seq_along(x)) {
    val_x <- x[k]
    if (val_x > x_prev && val_x < x_curr) {
      val[k] <- 1/h
    } else if (val_x > x_curr && val_x < x_next) {
      val[k] <- -1/h
    } else {
      val[k] <- 0
    }
  }
  return(val)
}

# 3. Calculăm termenul diagonal k_ii
k_ii = Integral( (phi_i)^2 ) #dx pe intervalul [x_{i-1}, x_{i+1}]

integrand_diag <- function(x) {
  dphi <- derivata_phi(x, x_im1, x_i, x_ip1, h)
  return(dphi^2)
}

rezultat_kii <- integrate(integrand_diag, lower = x_im1, upper = x_ip1)

# 4. Calculăm termenul din afara diagonalei k_i,i+1 (Optional, pentru completitudine)
k_ij = Integral( phi_i * phi_{i+1} )  
#pe intervalul comun [x_i, x_{i+1}]
# Pe acest interval: phi_i' = -1/h, phi_{i+1}' = 1/h

integrand_offdiag <- function(x) {
  # Derivata phi_i (panta negativă pe intervalul drept)
  dp_i <- -1/h 
  # Derivata phi_{i+1} (panta pozitivă pe intervalul stâng al vecinului)
  dp_ip1 <- 1/h
  return(dp_i * dp_ip1)
}

rezultat_kij <- integrate(integrand_offdiag, lower = x_i, upper = x_ip1)

# 5. Afișare și Verificare cu Teoria
valoare_teoretica_kii <- 2/h
valoare_teoretica_kij <- -1/h

cat("Pasul h ales:", h, "\n")
cat("Calcul Integrala k_ii (R):   ", result_kii$value, "\n")
cat("Valoare Teoretică (2/h):     ", valoare_teoretica_kii, "\n")
cat("Eroare:", abs(rezultat_kii$value - valoare_teoretica_kii), "\n\n")

cat("Calcul Integrala k_i,i+1 (R):", rezultat_kij$value, "\n")
cat("Valoare Teoretică (-1/h):    ", valoare_teoretica_kij, "\n")

# Construirea Matricei K (Exemplu pentru 3 noduri interne, cum era in Partea I)
# K = (1/h) * | 2 -1  0 |
#             |-1  2 -1 |
#             | 0 -1  2 |
# Observăm că factorul 1/h scos în față înseamnă că elementele sunt 2/h și -1/h