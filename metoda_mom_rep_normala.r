############ Problema 9.3

#9.3 Presupuneți că valorile au fost extrase dintr-o repartiție normală de parametri 
#necunoscuți. Folosind metoda verosimilității maxime și respectiv metoda momentelor
#estimați acești parametri ȋn baza celor 5 eșantioane. Comparați estimările obținute prin 
#cele două metode și comentați rezultatele. 

# Metoda momentelor este folosita pentru a aproxima anumiti parametrii din functiile de repartitie.
# Aceasta presupune integrarea functiei si estimarea parametrului in functie de medie.
# In cazul nostru, vom estima media repartitiilor, presupunand ca seturile de date provin dintr-o repartitie normala.

metoda_mom_rep_normala <- function(x){			# Primim ca parametru vectorul ce reprezinta variabila aleatoare
  sigma <- sd(x)								# Ii calculam deviatia standard
  miu <- mean(x)								# Ii calculam media
  
  f_repartitie <- function(x){					#Exprimam functia de repartitie, presupunand ca provine dintr-o repartitie normala
    x * exp(-((x - miu)^2) / 2 * sigma ^ 2) / sigma * sqrt(2 * pi)
  }
  curve(expr = f_repartitie, from = 0, to=30)	# Plotam functia de repartitie, pentru o vizualizare directa
  rez <- integrate(f_repartitie, 0, Inf)		# Integram functia si pastram rezultatul(media) in rez
  
  return(rez)									# Returnam media aproximata
}
