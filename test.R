test <- c("https://www.lesechos.fr/economie-france/economie-france/budget-fiscalite/jean-castex-annonce-la-creation-de-1000-emplois-dans-les-services-departementaux-de-letat-1287855",
          "https://www.lesechos.fr/economie-france/social/emploi-salarie-la-france-limite-les-degats-face-au-covid-1287779", 
          "https://www.lesechos.fr/economie-france/conjoncture/covid-le-deficit-commercial-de-la-france-sest-dangereusement-creuse-en-2020-1287617",
          "https://www.lesechos.fr/economie-france/social/teletravail-le-gouvernement-veut-des-reunions-sans-delai-avec-les-elus-du-personnel-dans-les-entreprises-1287528",
          "https://www.lesechos.fr/economie-france/conjoncture/leconomie-francaise-resiste-mieux-que-prevu-a-lepidemie-de-covid-1287438")



## Application de la fonction pour la date des articles
date <- sapply(test, lesEchosDate)

## Application de la fonction pour le nombre d'occurence
occ <- sapply(test, nbre_occurence)

## Création du dataframe
tableau <- data.frame(article = test, date_article = date, occurences = occ)
row.names(tableau) <- NULL
tableau

write.csv(tableau,"/Users/alancuzon/Desktop/R/EPU_Banque_De_France/data.csv", row.names = FALSE)