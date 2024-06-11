#############################################

# Exercice 2.5.1

#############################################

# Définition des fonctions pour les signaux

t <- seq(-10, 10, by=0.01)

# Fonction rect(u)
rect <- function(u) {
  ifelse(abs(u) <= 0.5, 1, 0)
}

# Signal x(t)
x_t <- function(t) {rect((t - pi / (2 * pi)))}
# Signal y(t)
y_t <- function(t) exp(-t^2/2)/sqrt(2*pi)
# Signal z(t) pour différentes valeurs de f0
z1_t <- function(t) cos(2 * pi * 1 * t)^2
z2_t <- function(t) cos(2 * pi * 2 * t)^2
z3_t <- function(t) cos(2 * pi * 3 * t)^2

#############################################

# 1) Tracer les signaux

#############################################

plot(t, sapply(t, x_t), type="l", main="x(t)", xlab="t", ylab="x(t)")
plot(t, sapply(t, y_t), type="l", main="y(t)", xlab="t", ylab="y(t)")
plot(t, sapply(t, z1_t), type="l", main="z1(t) avec f0=1", xlab="t", ylab="z1(t)")
plot(t, sapply(t, z2_t), type="l", main="z2(t) avec f0=2", xlab="t", ylab="z2(t)")
plot(t, sapply(t, z3_t), type="l", main="z3(t) avec f0=3", xlab="t", ylab="z3(t)")


#############################################

# 2) Implémenter une méthode de calcul de TF

#############################################

TF <- function(signal, t, f) {
  # Define the real parts of the Fourier transform
  XRe <- function(f) {
    n <- length(t)
    real_part <- signal(t) * cos(2 * pi * f * t)
    result <- sum(real_part) / n
    return(result)
  }
  # Define the imaginary parts of the Fourier transform
  XIm <- function(f) {
    n <- length(t)
    imag_part <- signal(t) * sin(2 * pi * f * t)
    result <- sum(imag_part) / n
    return(result)
  }
  
  # Calculate the Fourier transform
  X <- complex(real = XRe(f), imaginary = XIm(f))
  
  return(X)
}

#############################################

# 3) A l'aide de la TF pour chaque signal tracer les spectres d'amplitude et de phase 

#############################################

freq <- seq(-10, 10, by=0.01)

# Calcul TF pour x(t), y(t), z1(t), z2(t), and z3(t)
X <- sapply(freq, function(f) TF(x_t, t, f))
Y <- sapply(freq, function(f) TF(y_t, t, f))
Z1 <- sapply(freq, function(f) TF(z1_t, t, f))
Z2 <- sapply(freq, function(f) TF(z2_t, t, f))
Z3 <- sapply(freq, function(f) TF(z3_t, t, f))

calculate_amplitude <- function(complex_numbers) {
  sqrt(Re(complex_numbers)^2 + Im(complex_numbers)^2)
}

# Function to calculate the phase (angle) of complex numbers in radians
calculate_phase <- function(complex_numbers) {
  
  atan2(Im(complex_numbers), Re(complex_numbers))
}

plot(freq, calculate_amplitude(X), type="l", main="Spectre d'amplitude de x(t)")
plot(freq, calculate_phase(X), type="l", main="Spectre de phase de x(t)")

plot(freq, calculate_amplitude(Y), type="l", main="Spectre d'amplitude de y(t)")
plot(freq, calculate_phase(Y), type="l", main="Spectre de phase de y(t)")


plot(freq, calculate_amplitude(Z1), type="l", main="Spectre d'amplitude de Z1(t)")
plot(freq, calculate_phase(Z1), type="l", main="Spectre de phase de Z1(t)")

plot(freq, calculate_amplitude(Z2), type="l", main="Spectre d'amplitude de Z2(t)")
plot(freq, calculate_phase(Z2), type="l", main="Spectre de phase de Z2(t)")

plot(freq, calculate_amplitude(Z3), type="l", main="Spectre d'amplitude de Z3(t)")
plot(freq, calculate_phase(Z3), type="l", main="Spectre de phase de Z3(t)")

#############################################

# Exercice 2.5.2

# 1) Implémenter une méthode de calcul de TF^-1 : transformation de Fourier inverse

#############################################

TF_inverse <- function(X, f, t) {
  delta_f <- f[2] - f[1]
  signal <- Re(sum(X * exp(1i*2*pi*f*t) * delta_f))
  return(signal)
}

#############################################

# 2) Donner x(t)

#############################################

X_f <- function(f) { 1 / (1 + f^2) }
X_values <- X_f(freq)
X_t <- sapply(t, function(t) TF_inverse(X_values, freq, t))
plot(t, X_t, type="l", main="x(t) quest 2")

#############################################

# 3) A l'aide des TF calculer dans l'exercice précédent, retrouver les signaux originels 

#############################################

xt_retrouve <- sapply(t, function(t) TF_inverse(X, freq, t))
plot(t, xt_retrouve, type="l", main="x(t) retrouvé")

yt_retrouve <- sapply(t, function(t) TF_inverse(Y, freq, t))
plot(t, yt_retrouve, type="l", main="y(t) retrouvé")

z1_retrouve <- sapply(t, function(t) TF_inverse(Z1, freq, t))
plot(t, z1_retrouve, type="l", main="z1(t) retrouvé")

z2_retrouve <- sapply(t, function(t) TF_inverse(Z2, freq, t))
plot(t, z2_retrouve, type="l", main="z2(t) retrouvé")

z3_retrouve <- sapply(t, function(t) TF_inverse(Z3, freq, t))
plot(t, z3_retrouve, type="l", main="z3(t) retrouvé")