
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

## Application de la fonction
urls <- unlist(sapply(lesEchosPages(), lesEchosArticles))


