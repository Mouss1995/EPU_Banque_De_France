## Récupération des urls des pages sur Les Echos
lesEchosScrap <- function(url = "https://www.lesechos.fr/economie-france"){
  list_of_pages <- str_c(url, '?page=', 1:nombrePageEchos())
  list_of_pages
}