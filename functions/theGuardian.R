## --------- Fonctions relatives au journal The Guardian --------- ##

## Récupération des urls des pages sur le journal "The Guardian" catégorie Économie
TheGuardianUrlsPages <- function(url = "https://www.theguardian.com/business/economics"){
  pages_guardian <- str_c(url, '?page=', 1:1300)
  print(pages_guardian)
  pages_guardian
}

## Récupération des urls des articles sur le journal "The Guardian" catégorie Économie
TheGuardianUrlsArticles <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status == 200){
    article_guardian <- read_html(url)
    liste_urls_guardian <- article_guardian %>% html_nodes("div.fc-item__content ") %>% html_nodes("a") %>% html_attr("href")
    urls_articles_guardian <<- append(urls_articles_guardian, unlist(liste_urls_guardian))
    print(liste_urls_guardian)
  }
  else{
    print(paste("Erreur avec l'url : ",url,spe=""))
  }
}

TheGuardianOccurences <- function(url){
  r = GET(url)
  status = status_code(r)
  if(status != 500){
    article <- read_html(url)
    if (!is.empty(article %>% html_nodes("div.css-1kkxezg") %>% html_text())){
      date <- article %>% html_nodes("div.css-1kkxezg") %>% html_text()
    }else{
      date <- article %>% html_nodes("p.content__dateline") %>% html_text()
    }
    date <- str_replace(date, "\n\n", "")
    date <- substr(date,5, 15)
    date <- unlist(strsplit(date, " "))
    if(is.empty(date[2]) == TRUE){
      print(paste("Erreur dans la récupération de la date dans : ",url,sep=""))
    }else{
      if(date[2] == "Feb"){date[2] = "Fév"}
      if(date[2] == "Apr"){date[2] = "Avr"}
      if(date[2] == "May"){date[2] = "Mai"}
      if(date[2] == "Jun"){date[2] = "Jui"}
      if(date[2] == "Aug"){date[2] = "Aoû"}
      if(date[2] == "Dec"){date[2] = "Déc"}
      date <- paste(date[1], date[2], date[3])
      date <- as.Date(date, format = "%d %b %Y")
      date <- as.character(date)
      titre <- article %>% html_nodes("div.css-13n7o1j") %>% html_text()
      if(is.empty(titre) == TRUE){
        titre <- article %>% html_nodes("div.u-cf") %>% html_text()
      }
      soustitre <- article %>% html_nodes("div.css-zjgnrw") %>% html_nodes("p") %>% html_text()
      if(is.empty(soustitre) == TRUE){
        soustitre <- article %>% html_nodes("div.content__standfirst") %>% html_nodes("p") %>% html_text()
      }
      corps <- article %>% html_nodes("div.css-15ibrj7") %>% html_nodes("p.css-38z03z") %>% html_text()
      if(is.empty(corps) == TRUE){
        corps <- article %>% html_nodes("div.signin-gate__first-paragraph-container") %>% html_nodes("p") %>% html_text()
      }
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
      # #arrange(desc(url))
      # Trier la matrice par rapport au vecteur de mots 
      sumBook <- booktm %>% filter(row.names(booktm) %in% mots)
      # Calcul du nombre d'occurences
      occurence <- sum(rowSums(sumBook))
      print(paste(date, " ", occurence,sep=""))
      df_guardian <<- rbind(df_guardian, c(date, occurence))
    }
  }else{
    print(paste("Erreur avec l'url : ",url,spe=""))
  }
}