is_prob_func <- function(f){
  if(typeof(f) == "closure"){
    max_delta <- 1e-3
    
    val_integrala <- integrate(f, lower = -Inf, upper = Inf)
    
    return(1 - max_delta <= val_integrala$value && val_integrala$value <= 1 + max_delta)
  }
  else if(typeof(f) == "double"){
    if(min(f) < 0 || max(f) > 1){
      return(FALSE)
    }
    
    return(sum(f) == 1)
  }
}

#9.1
a <- c(7, 4, 2, 11, 2, 1, 2, 1, 6, 6, 0, 1, 3, 9, 7, 0, 1, 14, 0, 5, 1, 5, 2, 4, 3, 1, 0, 0, 26, 1)
a_mediana <- median(a)
a_medie <- mean(a)
a_deviatie_standard <- sd(a)
b <- c(-1.91, -0.97, 4.59, 2.19, -0.86, -0.74, -0.60, -1.29, 0.93, 1.42, 2.14, -2.01, 2.60, 1.45, 2.60, -3.32, -3.62, 3.09, 2.91, 3.60, -0.83, -0.27, 1.82, -1.38, -1.76, 1.43, -0.59, -1.34, 2.07, 1.02)
b_mediana <- median(b)
b_medie <- mean(b)
b_deviatie_standard <- sd(b)
c <- c(0.90, 8.91, 0.06, 1.85, 1.61, 6.50, 0.26, 0.04, 0.62, 1.01, 3.42, 1.45, 3.44, 0.46, 0.55, 0.09, 2.22, 0.65, 0.61, 6.45, 0.27, 4.81, 2.27, 0.34, 4.51, 0.42, 3.71, 2.59, 0.42, 11.18)
c_mediana <- median(c)
c_medie <- mean(c)
c_deviatie_standard <- sd(c)
d <- c(4.83, 4.37, 5.57, 4.22, 5.96, 5.11, 5.52, 4.81, 5.19, 4.19, 4.73, 5.92, 5.63, 4.53, 4.67, 4.84, 5.25, 5.06, 5.98, 5.25, 4.60, 4.11, 4.32, 5.09, 5.25, 5.10, 4.36, 5.40, 5.33, 4.65)
d_mediana <- median(d)
d_medie <- mean(d)
d_deviatie_standard <- sd(d)
e <- c(11, 11, 10, 10, 10, 6, 5, 9, 11, 10, 14, 8, 11, 6, 13, 9, 14, 16, 14, 10, 7, 7, 11, 12, 9, 5, 12, 15, 9, 12)
e_mediana <- median(e)
e_medie <- mean(e)
e_deviatie_standard <- sd(e)


custom_hist <- function(va){
  va_mediana <- median(va)
  va_medie <- mean(va)
  va_deviatie_standard <- sd(va)
  
  hist(va, breaks=26, xlab="Frecventa", ylab="Valori", main="Histograma setului")
  abline(v = va_medie, col = "red", lwd = 2)
  abline(v = va_mediana, col = "pink", lwd = 2)
  abline(v = va_medie + va_deviatie_standard, col = "blue", lwd = 2)
  abline(v = va_medie - va_deviatie_standard, col = "blue", lwd = 2)
  legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)
}



hist(a, breaks=26, xlab="Frecventa", ylab="Valori", main="Histograma setului a")
abline(v = a_medie, col = "red", lwd = 2)
abline(v = a_mediana, col = "pink", lwd = 2)
abline(v = a_medie + a_deviatie_standard, col = "blue", lwd = 2)
abline(v = a_medie - a_deviatie_standard, col = "blue", lwd = 2)
legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)

hist(b, breaks=30, xlab="Frecventa", ylab="Valori", main="Histograma setului b")
abline(v = b_medie, col = "red", lwd = 2)
abline(v = b_mediana, col = "pink", lwd = 2)
abline(v = b_medie + b_deviatie_standard, col = "blue", lwd = 2)
abline(v = b_medie - b_deviatie_standard, col = "blue", lwd = 2)
legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)

hist(c, breaks=20, xlab="Frecventa", ylab="Valori", main="Histograma setului c")
abline(v = c_medie, col = "red", lwd = 2)
abline(v = c_mediana, col = "pink", lwd = 2)
abline(v = c_medie + c_deviatie_standard, col = "blue", lwd = 2)
abline(v = c_medie - c_deviatie_standard, col = "blue", lwd = 2)
legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)

hist(d, breaks=20, xlab="Frecventa", ylab="Valori", main="Histograma setului d")
abline(v = d_medie, col = "red", lwd = 2)
abline(v = d_mediana, col = "pink", lwd = 2)
abline(v = d_medie + d_deviatie_standard, col = "blue", lwd = 2)
abline(v = d_medie - d_deviatie_standard, col = "blue", lwd = 2)
legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)

hist(e, breaks=12, xlab="Frecventa", ylab="Valori", main="Histograma setului e")
abline(v = e_medie, col = "red", lwd = 2)
abline(v = e_mediana, col = "pink", lwd = 2)
abline(v = e_medie + e_deviatie_standard, col = "blue", lwd = 2)
abline(v = e_medie - e_deviatie_standard, col = "blue", lwd = 2)
legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)


find_repartition <- function(va){
  va_norm <- fitdistr(va, "normal")
  #va_gamma <- fitdistr(va, "gamma")
  va_poisson <- fitdistr(va, "Poisson")
  va_geo <- fitdistr(va, "geometric")
  va_exp <- fitdistr(va, "exponential")
  
  va_medie <- mean(va)
  va_sd <- sd(va)
  
  delta <- 1e-3
  
  if(abs(va_norm$estimate[1] - va_medie) <= delta && abs(va_norm$estimate[2] - va_sd) <= delta){
    rez <- list("Normal", va_norm$estimate)
    return(rez)
  }
  if(abs(va_poisson$estimate - va_medie) <= delta)
  {
    rez <- list("Poisson", va_poisson$estimate)
    return(rez)
  }
  #va_gamma_medie = va_gamma$estimate[1] * va_gamma$estimate[2]
  #va_gamma_sd = sqrt(va_gamma$estimate[1] * va_gamma$estimate[2]^2)
  #if(abs(va_gamma_medie - va_medie) <= delta && abs(va_gamma_sd - va_sd) <= delta){
  # rez <- list("Gamma", va_gamma$estimate)
  #return(rez)
  #}
  va_exponential_rate <- 1 / (va_medie + va_sd^2)
  if(abs(va_exp$estimate - va_exponential_rate) <= delta){
    rez <- list("Exponential", va_exp$estimate)
    return(rez)
  }
  va_geometric_medie <- (1 / va_geo$estimate) - 1
  if(abs(va_geometric_medie - va_medie) <= delta)
  {
    rez <- list("Geometric", va_geo$estimate)
    return(rez)
  }
}

metoda_mom_rep_normala <- function(x){
  sigma <- sd(x)
  miu <- mean(x)
  
  #return()
  f_repartitie <- function(x){
    x * exp(-((x - miu)^2) / 2 * sigma ^ 2) / sigma * sqrt(2 * pi)
  }
  curve(expr = f_repartitie, from = 0, to=30)
  rez <- integrate(f_repartitie, 0, Inf)
  
  return(rez)
}


metoda_verosimilitatii_max <- function(va){
  sigma <- sd(va)
  miu <- mean(va)
  
  f_repartitie <- function(x){
    x * exp(-((x - miu)^2) / 2 * sigma ^ 2) / sigma * sqrt(2 * pi)
  }
  
  curve(log(f_repartitie(x), base = exp(1)), from=-10, to=40)
  
  max_val <- max(log(f_repartitie(va), base = exp(1)))
  print(max_val)
  
  index <- which(log(f_repartitie(va), base = exp(1)) == max_val)
  print(index)
  
  print(va[index])
  print(mean(va[index]))
  
  rez <- mean(va[index])
  
  return(rez)
  
}
