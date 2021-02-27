##########################################################################################################################-
# Projet : Utilisation des techniques de text mining pour mesurer les anticipations d’inflation en France et au Royaume-Uni
# Auteur : Alan CUZON
# Mail : cuzonalan@gmail.com
##########################################################################################################################-


## ---------Installation des différents packages et chargement des librairies requises--------- ##

# install.packages("tm")
# install.packages("tidytext")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("devtools")
# install_github("kassambara/r2excel")
# install.packages("rapportools")
library(rvest)
library(stringr)
library(tidytext)
library(tidyverse)
library(tm)
library(dplyr)
library(zoo)
library(xlsx)
library(devtools)
library(r2excel)
library(rapportools)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(scales)
library(forecast)
source("functions/laTribune.R")
source("functions/lesEchos.R")
source("functions/theEconomist.R")
source("functions/theGuardian.R")
source("functions/epu.R")
source("functions/prediction.R")
source("functions/excel.R")

####################################################################################-
# Ce script à pour but de récupérer les données relatives à l'incertitude économique
# grâce à des techniques de web-scrapping et text-mining,
# ainsi que la visualisation des données et la prédiction de la série temporelle.
####################################################################################-


################################# La Tribune (FR) ##################################-

# Création du dataframe 
df_latribune <- data.frame(date=as.Date(character()),occurences=character(),stringsAsFactors=FALSE) 
# Récupération des urls des pages 
LaTribuneUrlsPages()
# Récupération des urls des articles de chaque pages 
urls_article <- unlist(sapply(urls_page, LaTribuneUrlsArticles))
# Récupération des données des articles et calcul du nombre d'occurences
sapply(urls_article, LaTribuneOccurences)
# Enregistrement du dataframe en .csv
names(df_latribune)[1] <- "Date"
names(df_latribune)[2] <- "Occurences"
write.csv(df_latribune,"data/df_latribune.csv", row.names = FALSE)

################################# Les Echos (FR) ###################################-

# Création du dataframe 
df_lesechos <- data.frame(date=as.Date(character()),occurences=character(),stringsAsFactors=FALSE) 
# Récupération des urls des pages 
LesEchosUrlsPages()
# Récupération des urls des articles de chaque pages 
urls_articles_echos <- unlist(sapply(urls_pages_echos, lesEchosUrlsArticles))
# Récupération des données des articles et calcul du nombre d'occurences
sapply(urls_articles_echos, lesEchosOccurences)
# Enregistrement du dataframe en .csv
names(df_lesechos)[1] <- "Date"
names(df_lesechos)[2] <- "Occurences"
write.csv(df_lesechos,"data/df_lesechos.csv", row.names = FALSE)

################################# The Economist (UK) #################################-

# Création du dataframe 
df_economist <- data.frame(date=as.Date(character()),occurences=character(),stringsAsFactors=FALSE)
urls_articles_economist <- character()
# Récupération des urls des pages 
pages_economist <- TheEconomistUrlsPages()
# Récupération des urls des articles de chaque pages 
sapply(pages_economist, TheEconomistUrlsArticles)
urls_articles_economist <- urls_articles_economist[urls_articles_economist!="https://www.economist.com"]
# Récupération des données des articles et calcul du nombre d'occurences
sapply(urls_articles_economist, TheEconomistOccurences)
# Enregistrement du dataframe en .csv
names(df_economist)[1] <- "Date"
names(df_economist)[2] <- "Occurences"
write.csv(df_economist,"data/df_economist.csv", row.names = FALSE)


################################# The Guardian (UK) #################################-

# Création du dataframe 
df_guardian <- data.frame(date=as.Date(character()),occurences=character(),stringsAsFactors=FALSE)
urls_articles_guardian <- character()
# Récupération des urls des pages
pages_guardian <- TheGuardianUrlsPages()
# Récupération des urls des articles de chaque pages 
sapply(pages_guardian, TheGuardianUrlsArticles)
# Récupération des données des articles et calcul du nombre d'occurences
sapply(urls_articles_guardian, TheGuardianOccurences)
# Enregistrement du dataframe en .csv
names(df_guardian)[1] <- "Date"
names(df_guardian)[2] <- "Occurences"
write.csv(df_guardian,"data/df_guardian.csv", row.names = FALSE)


################ Calcul de l'indice EPU et visualisation des données ###############-

# Calcul de l'indice EPU pour la France et le Royaume-Uni
epu()
# Visualisation des données FR
plot_fr
# Visualition des données UK
plot_uk
# Visualisation des données globales (FR + UK)
plot_global
# Prédiction de la série temporelle
prediction()

################ Création du fichier Excel ###############-

# Création du fichier Excel qui contient les occurences présentes au sein des 
# différents journaux, ainsi que l'indice EPU FR et EPU UK
excel()