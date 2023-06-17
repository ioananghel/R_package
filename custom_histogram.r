############# Problema 9
#Se dau seturile următoare de valori:
#a) 7 4 2 11 2 1 2 1 6 6 0 1 3 9 7 0 1 14 0 5 1 5 2 4 3 1 0 0 26 1
#b) -1.91 -0.97 4.59 2.19 -0.86 -0.74 -0.60 -1.29 0.93 1.42 2.14 -2.01 2.60 1.45 2.60 -3.32 -3.62 3.09 2.91 3.60 -0.83 -0.27 1.82 -1.38 -1.76 1.43 -0.59 -1.34 2.07 1.02
#c) 0.90 8.91 0.06 1.85 1.61 6.50 0.26 0.04 0.62 1.01 3.42 1.45 3.44 0.46 0.55 0.09 2.22 0.65 0.61 6.45 0.27 4.81 2.27 0.34 4.51 0.42 3.71 2.59 0.42 11.18
#d) 4.83 4.37 5.57 4.22 5.96 5.11 5.52 4.81 5.19 4.19 4.73 5.92 5.63 4.53 4.67 4.84 5.25 5.06 5.98 5.25 4.60 4.11 4.32 5.09 5.25 5.10 4.36 5.40 5.33 4.65
#e) 11 11 10 10 10 6 5 9 11 10 14 8 11 6 13 9 14 16 14 10 7 7 11 12 9 5 12 15 9 12
#Pentru fiecare dintre seturile de valori de mai sus efectuați:
#9.1. Faceți histograma valorilor. Calculați mediana, media și deviația standard și 
#ilustrați pe desen aceste valori.

# Mai jos formam vectorii cu valorile specificate in cerinte si apoi calculam:
#	mediana -> median(va)
#	media -> mean(va)
#	deviatia standard -> sd(va)
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

# Functia de ilustrare a histogramei si a valorilor calculate mai sus, pe aceasta
custom_hist <- function(va){		# Primim un vector ce reprezinta o variabila aleatoare
  va_mediana <- median(va)			# Ii calculam mediana
  va_medie <- mean(va)				# Ii calculam media
  va_deviatie_standard <- sd(va)	# Ii calculam deviatia standard
  
  hist(va, breaks=26, xlab="Frecventa", ylab="Valori", main="Histograma setului")
									# Creem histograma setului de date, ilustrand pe axa Ox valorile si pe Oy frecventele acestora.
  abline(v = va_medie, col = "red", lwd = 2)
									# Trasam cu o linie rosie pe histograma media setului
  abline(v = va_mediana, col = "pink", lwd = 2)
									# Trasam cu o linie roz pe histograma mediana setului
  abline(v = va_medie + va_deviatie_standard, col = "blue", lwd = 2)
  abline(v = va_medie - va_deviatie_standard, col = "blue", lwd = 2)
									# Marcam cu linii albastre intervalul delimitat de deviatia standard
  legend("topright", legend = c("Media", "Mediana", "Deviația Standard"), col = c("red", "pink", "blue"), lwd = 2, cex=0.5)
									# Afisam legenda histogramei in coltul dreapta, sus
}

# Afisarea histogramelor pentru seturile de date mentionate
custom_hist(a)
custom_hist(b)
custom_hist(c)
custom_hist(d)
custom_hist(e)