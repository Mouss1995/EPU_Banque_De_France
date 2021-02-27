## --------- Fonctions relatives au journal Les Echos--------- ##

## Récupération du nombre de pages sur le journal "Les Echos"
LesEchosNbrePages <- function(url = "https://www.lesechos.fr/economie-france"){
  echos_pages <- read_html(url)
  nbr_pages_echos <- echos_pages %>% html_nodes("li.sc-14kwckt-18.cxo6d5-4.kCkBdA") %>% html_nodes("a.sc-1560xb1-0.fUqzcK.cxo6d5-3.hlmATy.active") %>% html_text() %>% as.numeric() 
  nbr_pages_echos <- max(na.exclude(nbr_pages_echos), na.rm = TRUE)
  nbr_pages_echos
}

## Récupération des urls des pages sur le journal "Les Echos" catégorie Économie
LesEchosUrlsPages <- function(url = "https://www.lesechos.fr/economie-france"){
  urls_pages_echos <- character()
  pages_echos <- str_c(url, '?page=', 1:LesEchosNbrePages())
  print(pages_echos)
  urls_pages_echos <<- pages_echos
}

## Récupération des urls des articles sur le journal "Les Echos" catégorie Économie
lesEchosUrlsArticles <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    urls_article_echos <- read_html(url)
    liste_urls_echos <- urls_article_echos %>% html_nodes("a.sc-1560xb1-0.fUqzcK.sc-1ttlxdz-2.aTebi") %>% html_attr("href")
    urls_articles_echos <- unique(paste("https://www.lesechos.fr/economie-france",liste_urls_echos, sep=""))
    print(urls_articles_echos)
    urls_articles_echos
  }else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}

## Récupération du nombres d'occurences des articles
lesEchosOccurences <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    article <- read_html(url)
    # Récupération de la date de publication des articles
    date <- article %>% html_nodes("span.sc-1i0ieo8-0.dBUkqh") %>% html_text()
    # Split des données pour récupérer le mois et l'année
    date <- strsplit(date[1], split = " ", fixed=TRUE)
    date <- (unlist(date))
    date[4] <- 
      if (date[4] == "janv."){date[4] <- "01"} else
        if (date[4] == "févr."){date[4] <- "02"} else 
          if (date[4] == "mars"){date[4] <- "03"} else
            if (date[4] == "avr."){date[4] <- "04"} else
              if (date[4] == "mai"){date[4] <- "05"} else
                if (date[4] == "juin"){date[4] <- "06"} else
                  if (date[4] == "juil."){date[4] <- "07"} else
                    if (date[4] == "août"){date[4] <- "08"} else
                      if (date[4] == "sept."){date[4] <- "09"} else
                        if (date[4] == "oct."){date[4] <- "10"} else
                          if (date[4] == "nov."){date[4] <- "11"} else
                            if (date[4] == "déc."){date[4] <- "12"}
    date_article <- paste(date[5],"-",date[4],"-",date[3],sep="")
    # Vecteur des mots à extraire
    mots <- c("économie", "économies", "économique", "économiques", "incertain", "incertitude", "incertitudes", "politique", "politiques", "fiscalité", "dépenses", "dépense", "réglementation", "réglementations", "budget", "budgets", "déficit")
    # Récupération du texte de l'article (titres, sous-titres, corps)
    titre <- article %>% html_nodes("h1.sc-AxirZ.sc-1ohdft1-0.leQiFa") %>% html_text()
    resume <- article %>% html_nodes("p.sc-AxirZ.sc-1ohdft1-0.ejcVmy") %>% html_text()
    soustitre <- article %>% html_nodes("h3.sc-14kwckt-2.lhSmJG.sc-AxirZ.sc-12m61ps-0.bifBkp") %>% html_text()
    corps <- article %>% html_nodes("div.sc-1r87fjh-0.clsuA-D.post-paywall") %>% html_text()
    corps <- append(corps, titre)
    corps <- append(corps, resume)
    corps <- append(corps, soustitre)
    # Création du corpus et de la matrice de mot
    booktm <- corps %>%
      # Transformer en corpus
      VectorSource() %>%
      Corpus() %>% 
      # Nettoyer le corpus
      tm_map(content_transformer(tolower)) %>%
      tm_map(content_transformer(gsub), pattern = "l'", replacement = "") %>%
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
    df_lesechos <<- rbind(df_lesechos, c(date_article, occurence))
  }else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}