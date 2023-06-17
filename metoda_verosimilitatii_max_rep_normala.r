metoda_verosimilitatii_max_rep_normala <- function(va){
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