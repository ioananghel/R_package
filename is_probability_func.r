#############Problema 1
#Verificarea dacă o funcție introdusă de utilizator este funcție de masă/densitate de 
#probabilitate. Arătați ȋn mod explicit cum diferențiați ȋntre cazul funcției de masă și 
#respectiv al densității de probabilitate.

is_prob_func <- function(f){  		#Utilizatorul transmite ca parametru functia pe care doreste sa o verifice
  if(typeof(f) == "closure"){ 		# in cazul in care functia transmisa este de tip "closure", adica este o functie continua,
									# vom verifica daca este o funcite de densitate
    max_delta <- 1e-3		  		# vom seta o eroare maxima de calcul, in cazul nostru, 10^(-3)
    
    val_integrala <- integrate(f, lower = -Inf, upper = Inf)
									# integram functia pe intervalul (-Inf, Inf), pentru a-i afla aria de sub grafic
    
    return(1 - max_delta <= val_integrala$value && val_integrala$value <= 1 + max_delta)
									# returnam TRUE(este functie de densitate), daca valoarea este ~ 1, cu o eroare <= max_delta, sau FALSE(nu este functie de densitate).
  }
  else if(typeof(f) == "double"){	# In cazul in care functia primita este de tip "double", adica este discreta,
									# vom verifica daca este o functie de masa.
    if(min(f) < 0 || max(f) > 1){	# Valorile unei functii de masa pot apartine doar intervalului [0, 1]
      return(FALSE)					# In cazul in care avem valori care nu apartin acestui interval, vom returna FALSE(nu este o functie de masa)
    }
									# Daca valorile functiei sunt in intervalul dorit, vom verifica in continuare daca este o functie de masa
    return(sum(f) == 1)				# Vom returna TRUE(este o functie de masa), in cazul in care suma valorilor = 1, sau FALSE(nu este o functie de masa) in caz contrar
  }
}

