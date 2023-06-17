find_origin_repartition <- function(va){
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