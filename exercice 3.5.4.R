if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}


tfd <- function(x) {
  result <- numeric(length(x))
  N <- length(x)
  n <- 0:(N-1)
  for (k in n) {
    tmp = 0
    for (nn in n) {
      tmp = tmp + x[nn+1] * exp(-1i * 2 * pi * k * nn / N)
    }
    result <- append(result, tmp)
  }
  return(result)
}


fft_cooley_tukey <- function(x) {
  N <- length(x)
  
  if (N <= 1) {
    return(x)
  }
  
  even <- fft_cooley_tukey(x[seq(2, N, by = 2)])
  odd <- fft_cooley_tukey(x[seq(1, N, by = 2)])
  
  twiddle <- exp(-2i * pi * seq(0, N/2 - 1) / N)
  
  return(c(
    even + twiddle * odd,
    even - twiddle * odd
  ))
}

chemin_du_fichier <- "data/validation_groupby_JOUR.csv"
donnees <- read.csv2(chemin_du_fichier)
#donnees <- head(donnees, n = 256)


sizes <- list()
times_tfd <- list()
times_fft <- list()
for (i in c(2, 4, 8, 16, 32, 64, 128, 256)){
  donnees_tmp <- head(donnees, n = i)
  fe <- 1
  N <- length(donnees_tmp$NB_VALD)
  n <- seq(0, N - 1)
  freq <- (n*fe)/N
  values <- c(donnees_tmp$NB_VALD)
  
  start_time <- Sys.time()
  fft_cooley_tukey(values)
  end_time <- Sys.time()
  sizes <- append(sizes, N)
  times_fft <- append(times_fft, as.numeric(end_time - start_time))
  
  start_time <- Sys.time()
  tfd(values)
  end_time <- Sys.time()
  times_tfd <- append(times_tfd, as.numeric(end_time - start_time))
}

plot(sizes, unlist(times_fft)/unlist(times_tfd), type = "o", col = "blue", xlab = "size", ylab = "rapport", main = "Rapport du temps d'exécution en fonction de la taille de l'échantillon entre la méthode FFT et TFD")
legend("topright", legend = c("tfd", "fft"), col = c("blue", "red"), lty = 1)

donnees <- head(donnees, n = 256)
fe <- 1
N <- length(donnees$NB_VALD)
n <- seq(0, N - 1)
freq <- (n*fe)/N
values <- c(donnees$NB_VALD)

tfd_result = tfd(values)
magnitude_tfd <- Mod(tfd_result)
phase_tfd <- Arg(tfd_result)

df_tfd <- data.frame(Fréquence = freq, Magnitude = magnitude_tfd, Phase = phase_tfd)

# Créer df_tfd graphique de magnitude
ggplot(df_tfd, aes(Fréquence, Magnitude)) +
  geom_bar(stat = "identity") +
  labs(x = "Fréquence", y = "Magnitude") +
  ggtitle("Magnitude de la TFD")

# Créer le graphique de phase
ggplot(df_tfd, aes(Fréquence, Phase)) +
  geom_bar(stat = "identity") +
  labs(x = "Fréquence", y = "Phase") +
  ggtitle("Phase de la TFD")

fft_result = fft_cooley_tukey(values)
magnitude_fft <- Mod(fft_result)
phase_fft <- Arg(fft_result)

# Créer un dataframe pour les données
df_fft <- data.frame(Fréquence = freq, Magnitude = magnitude_fft, Phase = phase_fft)

# Créer le graphique de magnitude
ggplot(df_fft, aes(Fréquence, Magnitude)) +
  geom_bar(stat = "identity") +
  labs(x = "Fréquence", y = "Magnitude") +
  ggtitle("Magnitude de la FFT")
  #scale_x_continuous(breaks = freq)

# Créer le graphique de phase
ggplot(df_fft, aes(Fréquence, Phase)) +
  geom_bar(stat = "identity") +
  labs(x = "Fréquence", y = "Phase") +
  ggtitle("Phase de la FFT")

