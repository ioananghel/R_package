# Importam pachetele de care avem nevoie pentru a rezolva problema
library(survival)
library(MAAS)
library(fitdistrplus)

# Vom folosi functia fitdistr(va, "nume repartitie").
# Acesta aproximeaza parametrii specifici repartitiei mentionate, in incercarea de a fita cat mai eficient vectorul transmis ca parametru.
# Pentru a verifica daca este plauzibil ca vectorul nostru sa apartina unei repartitii aproximate cu tehnica de mai sus, vom verifica daca parametrii 
# rezultati sunt destul de apropiati de cei pe care ii putem calcula direct, folsind valorile variabilei aleatoare.

# Functia creata poate avea o acuratete de aproximare mai scazuta in cazul seturilor de date mici,
# valorile extreme putan sa influenteze mai puternic rezultatele.

find_origin_repartition <- function(va){ 		# Vom primi un vector ce reprezinta variabila aleatoarea a careia dorim sa ii aflam repartitia de provenienta
  va_norm <- fitdistr(va, "normal")				# va_norm contine rezultatele fitarii vectorului la o repartitie Normala
  va_poisson <- fitdistr(va, "Poisson")			# va_poisson contine rezultatele fitarii vectorului la o repartitie Poisson
  va_geo <- fitdistr(va, "geometric")			# va_geo contine rezultatele fitarii vectorului la o repartitie Geometrica
  va_exp <- fitdistr(va, "exponential")			# va_exp contine rezultatele fitarii vectorului la o repartitie Exponentiala
  
  va_medie <- mean(va)							# Calculam media variabilei aleatoare
  va_sd <- sd(va)								# Calculam deviatia standard a variabilei aleatoare
  
  delta <- 1e-3									# Definim o eroare maxima de calcul, cu valoarea 10^(-3)
  
  if(abs(va_norm$estimate[1] - va_medie) <= delta && abs(va_norm$estimate[2] - va_sd) <= delta){
												# Daca media si deviatia standard aproximate prin fitarea datelor la o repartitie normala 
												# sunt destul de apropiate de valorile reale (eroarea <= delta), vom presupune ca valorile au fost
												# extrase dintr-o repartitie normala, cu parametrii aproximati.
    rez <- list("Normal", va_norm$estimate)		# Vom compune rezultatul din numele repartitiei(Normala) si parametrii estimati
    return(rez)
  }
  if(abs(va_poisson$estimate - va_medie) <= delta){
												# Daca media si deviatia standard aproximate prin fitarea datelor la o repartitie Poisson 
												# sunt destul de apropiate de valorile reale (eroarea <= delta), vom presupune ca valorile au fost
												# extrase dintr-o repartitie Poisson, cu parametrii aproximati.
    rez <- list("Poisson", va_poisson$estimate) # Vom compune rezultatul din numele repartitiei(Poisson) si parametrii estimati.
    return(rez)
  }
  # In cazul unei repartitii exponentiale, parametrul aproximat este rata cu care scade probabilitatea.
  # Acesta poate fi calculata si cu ajutorul mediei si a deviatiei standard si are formula exprimata mai jos:
  va_exponential_rate <- 1 / (va_medie + va_sd^2)
  if(abs(va_exp$estimate - va_exponential_rate) <= delta){
												# Daca rata aproximata prin fitarea datelor la o repartitie exponentiala 
												# este destul de apropiata de valoarea reala (eroarea <= delta), vom presupune ca valorile au fost
												# extrase dintr-o repartitie exponentiala, cu parametrul aproximat.
    rez <- list("Exponential", va_exp$estimate) # Vom compune rezultatul din numele repartitiei(Exponentiala) si parametrul estimat.
    return(rez)
  }
  # In cazul unei repartitii geometrice, parametrul aproximat este probabilitate de a obtine un succes pentru prima data.
  # Putem aproxima media distributiei estimate cu ajutorul fitarii, folosind formula exprimata mai jos:
  va_geometric_medie <- (1 / va_geo$estimate) - 1
  if(abs(va_geometric_medie - va_medie) <= delta){
												# Daca media aproximata prin fitarea datelor la o repartitie geometrica 
												# este destul de apropiata de valoarea reala (eroarea <= delta), vom presupune ca valorile au fost
												# extrase dintr-o repartitie geometrica, cu parametrul aproximat.
    rez <- list("Geometric", va_geo$estimate)	# Vom compune rezultatul din numele repartitiei(Geometrica) si parametrul estimat.
    return(rez)
  }
}