# Analyse de la série temporelle de l'indice EPU

prediction <- function(){

  # On convertit le vecteur contenant l'indice EPU en série temporelle 
  # Départ de Jan 2018 à Jan 2021, fréquence 12 pour découper par mois
  epu_fr <- ts(df_france$Occurences, start=c(2018, 1), end=c(2021, 01), frequency=12)
  epu_uk <- ts(df_uk$Occurences, start=c(2018, 1), end=c(2021, 01), frequency=12)
  
  # Décomposition saisonière
  fit_fr <- stl(epu_fr, s.window="period")
  png(filename="plot/season_fr.png")
  plot(fit_fr)
  dev.off()
  
  # Décomposition saisonière et enregistrement des graphiques
  fit_uk <- stl(epu_uk, s.window="period")
  png(filename="plot/season_uk.png")
  plot(fit_uk)
  dev.off()
  
  # Lissage exponentiel avec la méthode HoltWinters avec prise en charge de la saisonnalité
  fit_fr <- HoltWinters(epu_fr)
  fit_uk <- HoltWinters(epu_uk)
  
  # Précision prédictive
  accuracy(forecast(fit_fr))
  accuracy(forecast(fit_uk))
  
  # Prédire les valeurs de l'indice EPU des 5 prochains mois et enregistrement des graphiques
  forecast(fit_fr, 5)
  png(filename="plot/prediction_fr.png")
  prediction_fr <<- plot(forecast(fit_fr, 5), 
                         main = "Prédiction des 5 prochains mois France",
                         xlab = "Années",
                         ylab = "EPU")
  dev.off()
  
  forecast(fit_uk, 5)
  png(filename="plot/prediction_uk.png")
  prediction_uk <<- plot(forecast(fit_uk, 5), 
                         main = "Prédiction des 5 prochains mois Royaume-Uni",
                         xlab = "Années",
                         ylab = "EPU")
  dev.off()
}
