############ Problema 9.3
#9.3 Presupuneți că valorile au fost extrase dintr-o repartiție normală de parametri 
#necunoscuți. Folosind metoda verosimilității maxime și respectiv metoda momentelor
#estimați acești parametri ȋn baza celor 5 eșantioane. Comparați estimările obținute prin 
#cele două metode și comentați rezultatele. 

# Metoda verosimilitatii maxime este folosita pentru a aproxima anumiti parametrii ai functiilor de repartitie.
# In cazul nostru, vom estima media repartitiilor, presupunand ca seturile de date provin dintr-o repartitie normala.
# Logaritmul natural este o functie crescatoare si injectiva. Asadar, aplicarea acestuia asupra unei functii nu ii schimba monotonia,
# iar daca avem f(xmax) = ymax, unde ymax este un punct de maxim global, ln(f(xmax)) va fi si el, la randul sau un punct de maxim global.
# Logaritmarea se aplica si pentru a usura procesul de derivare, spargand produsul de functii intr-o suma de logaritmi.
# Metoda presupune logaritmarea in baza e a functiei de repartitie, iar apoi aflarea punctului de maxim global.
# Cand calculele sunt facute manual, acest lucru se poate face utilizand derivate, pentru a analiza panta curbei, insa in R, putem afla direct maximul global
# si indicii pentru care se atinge.

metoda_verosimilitatii_max_rep_normala <- function(va){		# Vom primi ca parametru variabila aleatoare
  sigma <- sd(va)											# Ii vom calcula deviatia standard
  miu <- mean(va)											# Ii vom calcula media
  
  f_repartitie <- function(x){								# Exprimam functia de repartitie, presupunand ca provine dintr-o repartitie normala
    x * exp(-((x - miu)^2) / 2 * sigma ^ 2) / sigma * sqrt(2 * pi)
  }
  
  curve(log(f_repartitie(x), base = exp(1)), from=-10, to=40)
															# Plotam curba obtinuta prin logaritmarea functiei, pentru a si putea vizualiza punctul de maxim global
  
  max_val <- max(log(f_repartitie(va), base = exp(1)))		# Aflam valoarea maximului global al logaritmului si il pastram in max_val
  #print(max_val)
  
  index <- which(log(f_repartitie(va), base = exp(1)) == max_val)
															# Aflam indecsii pentru care functia atinge punctele de maxim global
  #print(index)
  #print(va[index])
  #print(mean(va[index]))
  
  rez <- mean(va[index])									# Retinem in rez media valorilor pentru care se atinge punctul de maxim global
  
  return(rez)												# Returnam rezultatul
  
}