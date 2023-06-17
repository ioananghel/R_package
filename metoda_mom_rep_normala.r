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
