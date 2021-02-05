## Récupération des urls des articles et du mois / année --------------------------------------------------
source("lesEchosListePages.R")
# a = c(2, 3, 5) 
# b = c("aa", "bb", "cc") 
# c = c(TRUE, FALSE, TRUE) 
df <- data.frame("urls", "mois", "annee") 


tst <- function(url = "https://www.lesechos.fr/economie-france?page=3"){
  tst_articles <- read_html(url)
  # Récupération des urls des articles
  tst_urls <- tst_articles %>% html_nodes("a.sc-1560xb1-0.fUqzcK.sc-1ttlxdz-2.aTebi") %>% html_attr("href")
  urls <- unique(paste(url,tst_urls, sep=""))
  # Récupération de la date de publication des articles
  test_date <- tst_articles %>% html_nodes("span.sc-1i0ieo8-0.iMwrSH") %>% html_text()
  test_date
  # Supression de la date de MAJ
  bad <- grepl("Mis à jour", test_date, fixed=TRUE)
  test_date <- test_date[!bad]
  # Split des données pour récupérer le mois et l'année
  test_date <- strsplit(test_date, split = " ", fixed=TRUE)
  mois <- sapply(test_date, function(x) x[4])
  annee <- sapply(test_date, function(x) x[5])
  # mois <- test_date[4]
  # annee <- test_date[5]
  df <<- rbind(df, c(urls, mois, annee))
  
  #df$urls <- substr(df$urls, 90, 110)
  df
}

# Pour le test
liste_pr_test <- lesEchosScrap()
y <- lapply(liste_pr_test, tst)
y

