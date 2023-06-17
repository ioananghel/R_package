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

