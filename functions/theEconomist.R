## --------- Fonctions relatives au journal The Economist --------- ##

## Récupération du nombre de pages sur le journal "The Economist"
TheEconomistNbrePages <- function(url = "https://www.economist.com/finance-and-economics/"){
  economist_pages <- read_html(url)
  nbr_pages_economist <- economist_pages %>% html_nodes("ul.ds-pagination") %>% html_nodes("li.ds-pagination__item") %>% html_text() 
  nbr_pages_economist <- max(na.exclude(nbr_pages_economist), na.rm = TRUE)
  nbr_pages_economist
}

## Récupération des urls des pages sur le journal "TheEconomist" catégorie Économie
TheEconomistUrlsPages <- function(url = "https://www.economist.com/finance-and-economics/"){
  pages_economist <- str_c(url, '?page=', 1:TheEconomistNbrePages())
  print(pages_economist)
  pages_economist
}


## Récupération des urls des articles sur le journal "TheEconomist" catégorie Économie
TheEconomistUrlsArticles <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status != 500){
    article_economist <- read_html(url)
    liste_urls_economist <- article_economist %>% html_nodes("a.headline-link") %>% html_attr("href")
    urls_articles <- paste("https://www.economist.com",liste_urls_economist, sep="")
    urls_articles_economist <<- append(urls_articles_economist, unlist(urls_articles))
    print(urls_articles)
  }
  else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}

TheEconomistOccurences <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    article <- read_html(url)
    date <- article %>% html_nodes("time.article__dateline-datetime") %>% html_attr("datetime")
    date <- substr(date,1, 10)
    titre <- article %>% html_nodes("span.article__headline") %>% html_text()
    soustitre <- article %>% html_nodes("p.article__description") %>% html_text()
    corps <- article %>% html_nodes("p.article__body-text") %>% html_text()
    corps <- append(corps, titre)
    corps <- append(corps, soustitre)
    # Vecteur des mots à extraire
    mots <- c("economy", "economic", "uncertain", "uncertainty", "policy", "tax", "spending", "regulation", "budget", "deficit")
    # Création du corpus et de la matrice de mot
    booktm <- corps %>%
      # Transformer en corpus
      VectorSource() %>%
      Corpus() %>% 
      # Nettoyer le corpus
      tm_map(content_transformer(tolower)) %>%
      tm_map(stripWhitespace) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>%
      #Transformer en matrice 
      TermDocumentMatrix() %>%
      as.data.frame.matrix() #%>%
    #arrange(desc(url))
    # Trier la matrice par rapport au vecteur de mots 
    sumBook <- booktm %>% filter(row.names(booktm) %in% mots)
    # Calcul du nombre d'occurences
    occurence <- sum(rowSums(sumBook))
    print(paste(date, " ", occurence, sep=""))
    df_economist <<- rbind(df_economist, c(date, occurence))
  }
  if(status == 500){
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}