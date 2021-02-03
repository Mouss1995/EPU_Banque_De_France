install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)

## Récupération du nombre de pages sur le journal "Les Echos"
nombrePageEchos <- function(url = "https://www.lesechos.fr/economie-france"){
  echos_articles <- read_html(url)
  nbr_pages_echos <- echos_articles %>% html_nodes("li.sc-14kwckt-18.cxo6d5-4.kCkBdA") %>% html_nodes("a.sc-1560xb1-0.fUqzcK.cxo6d5-3.hlmATy.active") %>% html_text() %>% as.numeric() 
  nbr_pages_echos <- max(na.exclude(nbr_pages_echos), na.rm = TRUE)
  nbr_pages_echos
}