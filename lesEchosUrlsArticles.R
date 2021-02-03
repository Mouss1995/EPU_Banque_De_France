## Récupération des urls des articles et du mois / année 
tst <- function(url = "https://www.lesechos.fr/economie-france?page=77"){
  tst_articles <- read_html(url)
  #Récupération des urls des articles
  tst_urls <- tst_articles %>% html_nodes("a.sc-1560xb1-0.fUqzcK.sc-1ttlxdz-2.aTebi") %>% html_attr("href")
  unique(paste(url,tst_urls, sep=""))
  #Récupération de la date des articles et élémination des éléments contenant la date de MAJ
  test_date <- tst_articles %>% html_nodes("span.sc-1i0ieo8-0.iMwrSH") %>% html_text()
  bad <- grepl("Mis à jour", test_date, fixed=TRUE)
  test_date <- test_date[!bad]
  test_date <- substr(test_date, 13, 22)
  test_date
}