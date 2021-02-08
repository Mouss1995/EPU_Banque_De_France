# install.packages("tm")
# install.packages("tidytext")
# install.packages("gutenbergr")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("dplyr")
# install.packages("tidyverse")
library(rvest)
library(stringr)
library(tidytext)
library(tidyverse)
library(tm)
library(gutenbergr)
library(stringr)
library(dplyr)

## Récupération du nombre de pages sur le journal "Les Echos"
nombrePageEchos <- function(url = "https://www.lesechos.fr/economie-france"){
  echos_articles <- read_html(url)
  nbr_pages_echos <- echos_articles %>% html_nodes("li.sc-14kwckt-18.cxo6d5-4.kCkBdA") %>% html_nodes("a.sc-1560xb1-0.fUqzcK.cxo6d5-3.hlmATy.active") %>% html_text() %>% as.numeric() 
  nbr_pages_echos <- max(na.exclude(nbr_pages_echos), na.rm = TRUE)
  nbr_pages_echos
}

## Récupération des urls des pages sur le journal "Les Echos" catégorie Économie
lesEchosPages <- function(url = "https://www.lesechos.fr/economie-france"){
  list_of_pages <- str_c(url, '?page=', 1:nombrePageEchos())
  list_of_pages
}

## Récupération des urls des articles sur le journal "Les Echos" catégorie Économie
lesEchosArticles <- function(url){
  liste_articles <- read_html(url)
  liste_urls <- liste_articles %>% html_nodes("a.sc-1560xb1-0.fUqzcK.sc-1ttlxdz-2.aTebi") %>% html_attr("href")
  urls <- unique(paste("https://www.lesechos.fr/economie-france",liste_urls, sep=""))
  urls
}

## Récupération de la date des articles
lesEchosDate <- function(url){
  liste_date <- read_html(url)
  # Récupération de la date de publication des articles
  date <- liste_date %>% html_nodes("span.sc-1i0ieo8-0.dBUkqh") %>% html_text()
  # Split des données pour récupérer le mois et l'année
  date <- strsplit(date[1], split = " ", fixed=TRUE)
  date <- (unlist(date))
  date <- paste(date[4], date[5])
  date
}

## Calcul du nombre d'occurence
nbre_occurence <- function(url){
  eco_html <- read_html(url)
  # Récupération de la date de publication des articles
  eco_texte <- eco_html %>% html_nodes("div.sc-1r87fjh-0.gKBiXL.post-paywall") %>% html_nodes("p") %>% html_text()
  booktm <- eco_texte %>%
    # Transformer en corpus
    VectorSource() %>%
    Corpus() %>% 
    #N ettoyer le corpus
    tm_map(content_transformer(tolower)) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    #Transformer en matrice 
    TermDocumentMatrix() %>%
    as.data.frame.matrix() %>%
    arrange(desc(`1`))
  sumBook <- booktm %>% filter(row.names(booktm) %in% c("incertitude", "incertitudes", "politique", "économique"))
  occurence <- sum(rowSums(sumBook))
  occurence
}

## Application de la fonction pour les urls des articles
urls <- unlist(sapply(lesEchosPages(), lesEchosArticles))

## Application de la fonction pour la date des articles
date <- sapply(urls, lesEchosDate)

## Application de la fonction pour le nombre d'occurence
occ <- sapply(urls, nbre_occurence)

## Création du dataframe
tableau <- data.frame(article = urls, date_article = date, occurences, occ)
row.names(tableau) <- NULL

# Export sous format csv
write.csv(tableau,"/Users/alancuzon/Desktop/R/EPU_Banque_De_France/data.csv", row.names = FALSE)

