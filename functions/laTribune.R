## --------- Fonctions relatives au journal La Tribune--------- ##

## Récupération du nombre d'années sur le journal "La Tribune" catégorie Économie
LaTribuneNbreAnnees <- function(url = "https://www.latribune.fr/economie-2/page-1"){
  tribune_annees <- read_html(url)
  nbr_annees_tribune <- tribune_annees %>%  html_nodes("ul.pagination-archive.years") %>% html_nodes("li") %>% html_text() %>% as.numeric() 
  nbr_annees_tribune <- sort(nbr_annees_tribune, decreasing = TRUE)
  nbr_annees_tribune <- nbr_annees_tribune [! nbr_annees_tribune %in% format(Sys.Date(), "%Y")]
  print(nbr_annees_tribune)
  nbr_annees_tribune
}

## Récupération des urls des pages sur le journal "La Tribune" catégorie Économie
LaTribuneUrlsPages <- function(){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    mois <- c("janvier", "fevrier", "mars", "avril", "mai", "juin", "juillet", "aout", "septembre", "octobre", "novembre", "decembre")
    my <- character()
    urls_page <- character()
    page_annees <- character()
    # Récupération page d'accueil
    urls_page <- append(urls_page, "https://www.latribune.fr/actualites/economie/economie.html")
    # Récupération des pages du mois actuel
    pages <- read_html("https://www.latribune.fr/economie-2/page-1")
    nbre_pages <- pages %>% html_nodes("ul.pagination-archive.pages") %>% html_nodes("li") %>% html_text()
    nbre_pages <- nbre_pages[-length(nbre_pages)]
    for(val in nbre_pages){
      urls_page <- append(urls_page, paste("https://www.latribune.fr/economie-2/page-",val,sep=""))
    }
    # Récupération des autres pages de l'année actuelle
    nbre_mois <- pages %>% html_nodes("ul.pagination-archive.months") %>% html_nodes("li") %>% html_text()
    nbre_mois <- nbre_mois %>% str_to_lower()
    nbre_mois <- gsub("é", "e", nbre_mois)
    nbre_mois <- gsub("û", "u", nbre_mois)
    nbre_mois <- nbre_mois[-1]
    for(val in nbre_mois){
      page_annees <- append(page_annees, paste("https://www.latribune.fr/economie-2/",val,"-",format(Sys.Date(), "%Y"),"/page-1",sep=""))
    }
    for(val in page_annees){
      pages <- read_html(val)
      nbre_pages <- pages %>% html_nodes("ul.pagination-archive.pages") %>% html_nodes("li") %>% html_text()
      for(val2 in 1:nbre_pages[length(nbre_pages)]){
        urls_page <- append(urls_page, paste(gsub('.{1}$', '', val),val2,sep=""))
      }
    }
    # Récupération des urls des pages des articles des autres années
    annees <- LaTribuneNbreAnnees()
    for(val in annees){
      for(val2 in mois){
        my <- append(my, paste("https://www.latribune.fr/economie-2/",val2,"-",val,"/page-1",sep=""))
      }
    }
    for(val in my){
      pages <- read_html(val)
      nbre_pages <- pages %>% html_nodes("ul.pagination-archive.pages") %>% html_nodes("li") %>% html_text()
      for(val2 in 1:nbre_pages[length(nbre_pages)]){
        urls_page <- append(urls_page, paste(gsub('.{1}$', '', val),val2,sep=""))
      }
    }
    print(urls_page)
    urls_page <<- urls_page
  }else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}

## Récupération des urls des articles sur le journal "La Tribune" catégorie Économie
LaTribuneUrlsArticles <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    liste_articles <- read_html(url)
    url_article <- liste_articles %>% html_nodes("article.article-wrapper.row.clearfix ") %>% html_nodes("a") %>% html_attr("href")
    url_article <- unique(url_article)
    print(url_article)
    url_article
  }else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}

## Récupération des données de chaque article et calcul du nombre d'occurences par rapport au vecteur de mots spécifié
LaTribuneOccurences <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    # Vecteur des mots à extraire
    mots <- c("économie", "économies", "économique", "économiques", "incertain", "incertitude", "incertitudes", "politique", "politiques", "fiscalité", "dépenses", "dépense", "réglementation", "réglementations", "budget", "budgets", "déficit")
    # Récupération de la date de l'article
    article <- read_html(url)
    date_article <- article %>% html_nodes("time") %>% html_attr("datetime")
    date_article <- substr(date_article,1,10)
    # Récupération du texte de l'article (titres, sous-titres, corps)
    titre <- article %>% html_nodes("div.article-title-wrapper") %>% html_nodes("h1") %>% html_text()
    soustitre <- article %>% html_nodes("div.body-article") %>% html_nodes("h2") %>% html_text()
    corps <- article %>% html_nodes("div.body-article") %>% html_nodes("p") %>% html_text()
    corps <- corps[1:length(corps)-1]
    corps <- append(corps, titre)
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
    occurence
    df_latribune <<- rbind(df_latribune, c(date_article, occurence))
    print(paste(date_article, " ", occurence, sep=""))
  }else{
    print(paste("Erreur avec l'url : ", url, spe=""))
  }
}