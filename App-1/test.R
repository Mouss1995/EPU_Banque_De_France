library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(xlsx)
library(rvest)
library(stringr)
library(tidytext)
library(tidyverse)
library(tm)
library(zoo)
library(xlsx)
library(scales)

graph_fr <- function(date1, date2){
  

  # On importe les données du csv dans les dataframes
  df_lesechos <- read.csv(file = "data/df_lesechos.csv")
  df_latribune <- read.csv(file = "data/df_latribune.csv")

  # On supprime le jour pour le remplacer par 01
  df_lesechos$Date <- substr(df_lesechos$Date,1,7)
  df_latribune$Date <- substr(df_latribune$Date,1,7)
  df_lesechos$Date <- paste(df_lesechos$Date,"-01",sep="")
  df_latribune$Date <- paste(df_latribune$Date,"-01",sep="")
  
  # Transforme la colonne Occurences au format numérique
  df_lesechos$Occurences <- as.numeric(df_lesechos$Occurences)
  df_latribune$Occurences <- as.numeric(df_latribune$Occurences)

  # On convertit la colonne date au format Date
  df_lesechos <- transform(df_lesechos, Date = as.Date(Date))
  df_latribune <- transform(df_latribune, Date = as.Date(Date))
  
  # On élimine les valeurs datant d'avant 2000
  df_lesechos <- df_lesechos[!(df_lesechos$Date < "2018-01-01"),]
  df_latribune <- df_latribune[!(df_latribune$Date < "2018-01-01"),]

  # On élimine les valeurs par rapport à l'intervale sélectionné
  df_lesechos <- df_lesechos[!(df_lesechos$Date < date1),]
  df_latribune <- df_latribune[!(df_latribune$Date < date1),]
  df_lesechos <- df_lesechos[!(df_lesechos$Date > date2),]
  df_latribune <- df_latribune[!(df_latribune$Date > date2),]

  # On groupe par mois et on fait la somme des occurences
  df_lesechos <- df_lesechos %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_latribune <- df_latribune %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))

  # Calcul de la variance pour chaque journal
  echos_var <- var(df_lesechos$Occurences)
  tribune_var <- var(df_latribune$Occurences)
  
  # Divisions des occurences par l'écart type, ce qui nous donne un écart type unitaire
  df_lesechos$Occurences <- (df_lesechos$Occurences) / sqrt(echos_var)
  df_latribune$Occurences <- (df_latribune$Occurences) / sqrt(tribune_var)
  
  # La normalisation de chaque série mensuelles des différents journaux nous permets de les combiner
  # Création du dataframe qui va faire la somme des deux dataframes
  df_france <- bind_rows(df_lesechos,df_latribune)
  df_france <- df_france %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))

  # On divise par 2 la série obtenue car on a 2 sources d'informations
  df_france$Occurences <- df_france$Occurences / 2

  # Calcul de la moyenne de la série 
  moyenne <- mean(df_france$Occurences)

  # On ramène la série à une moyenne de 100 afin d'obtenir l'indicateur EPU de chaque mois
  df_france$Occurences <- df_france$Occurences*(100/moyenne)

  # Visualisation de la série à l'aide ggplot2
  fr <- df_france %>%
    ggplot(aes(x=Date, y=Occurences, text = paste0("Date : ", format(Date, "%Y-%m"), "\n",
                                                   "EPU : ", round(Occurences)))) +
    geom_area(fill="#DC6A6A", alpha=0.5, group=1) +
    geom_line(color="#DC6A6A", size=0.5, group=1) +
    ggtitle("FR Indice EPU") +
    ylab("EPU") +
    xlab("Années-Mois") +
    geom_point(size=0.5) +
    scale_x_date(breaks = df_france$Date, labels = date_format("%Y-%m")) +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(size=14, face="italic", family="Avenir Next"),
          axis.title.x = element_text(family="Avenir Next"),
          axis.title.y = element_text(family="Avenir Next"))
  
  fr <- ggplotly(fr, tooltip = "text")
  
}


graph_uk <- function(date1, date2){
  
  # On importe les données du csv dans les dataframes
  df_economist <- read.csv(file = "data/df_economist.csv")
  df_guardian <- read.csv(file = "data/df_guardian.csv")
  
  # On supprime le jour pour le remplacer par 01
  df_economist$Date <- substr(df_economist$Date,1,7)
  df_guardian$Date <- substr(df_guardian$Date,1,7)
  df_economist$Date <- paste(df_economist$Date,"-01",sep="")
  df_guardian$Date <- paste(df_guardian$Date,"-01",sep="")
  
  # Transforme la colonne Occurences au format numérique
  df_economist$Occurences <- as.numeric(df_economist$Occurences)
  df_guardian$Occurences <- as.numeric(df_guardian$Occurences)
  
  # On convertit la colonne date au format Date
  df_economist <- transform(df_economist, Date = as.Date(Date))
  df_guardian <- transform(df_guardian, Date = as.Date(Date))
  
  # On élimine les valeurs datant d'avant 2018
  df_guardian <- df_guardian[!(df_guardian$Date < "2018-01-01"),]
  df_economist <- df_economist[!(df_economist$Date < "2018-01-01"),]
  
  if(date1 < "2018-01-01"){ date1 = "2018-01-01" }
  
  # On élimine les valeurs par rapport à l'intervale sélectionné
  df_economist <- df_economist[!(df_economist$Date < date1),]
  df_guardian <- df_guardian[!(df_guardian$Date < date1),]
  df_economist <- df_economist[!(df_economist$Date > date2),]
  df_guardian <- df_guardian[!(df_guardian$Date > date2),]
  
  # On groupe par mois et on fait la somme des occurences
  df_economist <- df_economist %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_guardian <- df_guardian %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # Calcul de la variance pour chaque journal
  economist_var <- var(df_economist$Occurences)
  guardian_var <- var(df_guardian$Occurences)
  
  # Divisions des occurences par l'écart type, ce qui nous donne un écart type unitaire
  df_economist$Occurences <- (df_economist$Occurences) / sqrt(economist_var)
  df_guardian$Occurences <- (df_guardian$Occurences) / sqrt(guardian_var)
  
  # La normalisation de chaque série mensuelles des différents journaux nous permets de les combiner
  # Création du dataframe qui va faire la somme des deux dataframes
  df_uk <- bind_rows(df_economist,df_guardian)
  df_uk <- df_uk %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # On divise par 2 la série obtenue car on a 2 sources d'informations
  df_uk$Occurences <- df_uk$Occurences / 2
  
  # Calcul de la moyenne de la série 
  moyenne <- mean(df_uk$Occurences)
  
  # On ramène la série à une moyenne de 100 afin d'obtenir l'indicateur EPU de chaque mois
  df_uk$Occurences <- df_uk$Occurences*(100/moyenne)
  
  uk <- df_uk %>%
    ggplot(aes(x=Date, y=Occurences, text = paste0("Date : ", format(Date, "%Y-%m"), "\n",
                                                   "EPU : ", round(Occurences)))) +
    geom_area(fill="#5685D7", alpha=0.2, group=1) +
    geom_line(color="#FF0000", size=0.2, group=1) +
    ggtitle("UK Indice EPU") +
    ylab("EPU") +
    xlab("Années-Mois") +
    scale_x_date(breaks = df_uk$Date, labels = date_format("%Y-%m")) +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(size=14, face="italic", family="Avenir Next"),
          axis.title.x = element_text(family="Avenir Next"),
          axis.title.y = element_text(family="Avenir Next"))+
    geom_point(size=0.5)
  
  uk <- ggplotly(uk, tooltip = "text")
}
   
graph_global <- function(date1, date2){
  
  # On importe les données du csv dans les dataframes
  df_lesechos <- read.csv(file = "data/df_lesechos.csv")
  df_latribune <- read.csv(file = "data/df_latribune.csv")
  df_economist <- read.csv(file = "data/df_economist.csv")
  df_guardian <- read.csv(file = "data/df_guardian.csv")
  
  # On supprime le jour pour le remplacer par 01
  df_lesechos$Date <- substr(df_lesechos$Date,1,7)
  df_latribune$Date <- substr(df_latribune$Date,1,7)
  df_lesechos$Date <- paste(df_lesechos$Date,"-01",sep="")
  df_latribune$Date <- paste(df_latribune$Date,"-01",sep="")
  df_economist$Date <- substr(df_economist$Date,1,7)
  df_guardian$Date <- substr(df_guardian$Date,1,7)
  df_economist$Date <- paste(df_economist$Date,"-01",sep="")
  df_guardian$Date <- paste(df_guardian$Date,"-01",sep="")
  
  # Transforme la colonne Occurences au format numérique
  df_economist$Occurences <- as.numeric(df_economist$Occurences)
  df_guardian$Occurences <- as.numeric(df_guardian$Occurences)
  df_latribune$Occurences <- as.numeric(df_latribune$Occurences)
  df_lesechos$Occurences <- as.numeric(df_lesechos$Occurences)
  
  # On convertit la colonne date au format Date
  df_economist <- transform(df_economist, Date = as.Date(Date))
  df_guardian <- transform(df_guardian, Date = as.Date(Date))
  df_lesechos <- transform(df_lesechos, Date = as.Date(Date))
  df_latribune <- transform(df_latribune, Date = as.Date(Date))
  
  # On élimine les valeurs datant d'avant 2018
  df_guardian <- df_guardian[!(df_guardian$Date < "2018-01-01"),]
  df_economist <- df_economist[!(df_economist$Date < "2018-01-01"),]
  df_latribune <- df_latribune[!(df_latribune$Date < "2018-01-01"),]
  df_lesechos <- df_lesechos[!(df_lesechos$Date < "2018-01-01"),]
  
  if(date1 < "2018-01-01"){ date1 = "2018-01-01" }
  
  # On élimine les valeurs par rapport à l'intervale sélectionné
  df_economist <- df_economist[!(df_economist$Date < date1),]
  df_guardian <- df_guardian[!(df_guardian$Date < date1),]
  df_economist <- df_economist[!(df_economist$Date > date2),]
  df_guardian <- df_guardian[!(df_guardian$Date > date2),]
  
  # On groupe par mois et on fait la somme des occurences
  df_lesechos <- df_lesechos %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_latribune <- df_latribune %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_economist <- df_economist %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_guardian <- df_guardian %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # Calcul de la variance pour chaque journal
  echos_var <- var(df_lesechos$Occurences)
  tribune_var <- var(df_latribune$Occurences)
  economist_var <- var(df_economist$Occurences)
  guardian_var <- var(df_guardian$Occurences)
  
  # Divisions des occurences par l'écart type, ce qui nous donne un écart type unitaire
  df_lesechos$Occurences <- (df_lesechos$Occurences) / sqrt(echos_var)
  df_latribune$Occurences <- (df_latribune$Occurences) / sqrt(tribune_var)
  df_economist$Occurences <- (df_economist$Occurences) / sqrt(economist_var)
  df_guardian$Occurences <- (df_guardian$Occurences) / sqrt(guardian_var)
  
  # La normalisation de chaque série mensuelles des différents journaux nous permets de les combiner
  # Création du dataframe qui va faire la somme des deux dataframes
  df_france <- bind_rows(df_lesechos,df_latribune)
  df_france <- df_france %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_uk <- bind_rows(df_economist,df_guardian)
  df_uk <- df_uk %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # On divise par 2 la série obtenue car on a 2 sources d'informations
  df_france$Occurences <- df_france$Occurences / 2
  df_uk$Occurences <- df_uk$Occurences / 2
  
  # Calcul de la moyenne de la série 
  moyenne <- mean(df_france$Occurences)
  moyenne <- mean(df_uk$Occurences)
  
  # On ramène la série à une moyenne de 100 afin d'obtenir l'indicateur EPU de chaque mois
  df_france$Occurences <- df_france$Occurences*(100/moyenne)
  df_uk$Occurences <- df_uk$Occurences*(100/moyenne)
  
  # Création du dataframe global (epu fr + epu uk)
  df_global <- cbind.data.frame(df_france$Date, df_uk$Occurences, df_france$Occurences) 
  names(df_global)[1] <- "Date"
  names(df_global)[2] <- "Epu_uk"
  names(df_global)[3] <- "Epu_fr"
  df_global <- transform(df_global, Epu_uk = as.numeric(Epu_uk))
  df_global <- transform(df_global, Epu_fr = as.numeric(Epu_fr))
  df_global <- transform(df_global, Date = as.Date(Date))
  
  # Graphique de l'EPU global entre le Royaume-Uni et la France
  global <- df_global %>%
    ggplot(aes(x=Date),  text = paste0("Date : ", format(Date, "%Y-%m"), "\n", "EPU : ", round(Occurences))) +
    geom_line(aes(y=Epu_fr), color="#DC6A6A", size=0.2) + 
    geom_line(aes(y=Epu_uk), color="#5685D7", size=0.2) + 
    geom_area(aes(y=Epu_uk), fill="#5685D7", alpha=0.5) +
    geom_area(aes(y=Epu_fr), fill="#DC6A6A", alpha=0.5) +
    ggtitle("Global Indice EPU") +
    ylab("EPU") +
    xlab("Années-Mois") +
    geom_point(aes(y=Epu_uk), size=0.5) +
    geom_point(aes(y=Epu_fr), size=0.5) +
    scale_x_date(breaks = df_global$Date, labels = date_format("%Y-%m")) +
    theme(axis.text.x = element_text(angle = 90),
          plot.title = element_text(size=14, face="italic", family="Avenir Next"),
          axis.title.x = element_text(family="Avenir Next"),
          axis.title.y = element_text(family="Avenir Next"))
  
   global <- ggplotly(global, tooltip = "text")
}

accurancy <- function(pays, date1, date2){
  # On importe les données du csv dans les dataframes
  df_lesechos <- read.csv(file = "data/df_lesechos.csv")
  df_latribune <- read.csv(file = "data/df_latribune.csv")
  df_economist <- read.csv(file = "data/df_economist.csv")
  df_guardian <- read.csv(file = "data/df_guardian.csv")
  
  # On supprime le jour pour le remplacer par 01
  df_lesechos$Date <- substr(df_lesechos$Date,1,7)
  df_latribune$Date <- substr(df_latribune$Date,1,7)
  df_lesechos$Date <- paste(df_lesechos$Date,"-01",sep="")
  df_latribune$Date <- paste(df_latribune$Date,"-01",sep="")
  df_economist$Date <- substr(df_economist$Date,1,7)
  df_guardian$Date <- substr(df_guardian$Date,1,7)
  df_economist$Date <- paste(df_economist$Date,"-01",sep="")
  df_guardian$Date <- paste(df_guardian$Date,"-01",sep="")
  
  # Transforme la colonne Occurences au format numérique
  df_economist$Occurences <- as.numeric(df_economist$Occurences)
  df_guardian$Occurences <- as.numeric(df_guardian$Occurences)
  df_latribune$Occurences <- as.numeric(df_latribune$Occurences)
  df_lesechos$Occurences <- as.numeric(df_lesechos$Occurences)
  
  # On convertit la colonne date au format Date
  df_economist <- transform(df_economist, Date = as.Date(Date))
  df_guardian <- transform(df_guardian, Date = as.Date(Date))
  df_lesechos <- transform(df_lesechos, Date = as.Date(Date))
  df_latribune <- transform(df_latribune, Date = as.Date(Date))
  
  # On élimine les valeurs datant d'avant 2018
  df_guardian <- df_guardian[!(df_guardian$Date < "2018-01-01"),]
  df_economist <- df_economist[!(df_economist$Date < "2018-01-01"),]
  df_latribune <- df_latribune[!(df_latribune$Date < "2018-01-01"),]
  df_lesechos <- df_lesechos[!(df_lesechos$Date < "2018-01-01"),]
  
  if(date1 < "2018-01-01"){ date1 = "2018-01-01" }
  
  # On élimine les valeurs par rapport à l'intervale sélectionné
  df_economist <- df_economist[!(df_economist$Date < date1),]
  df_guardian <- df_guardian[!(df_guardian$Date < date1),]
  df_economist <- df_economist[!(df_economist$Date > date2),]
  df_guardian <- df_guardian[!(df_guardian$Date > date2),]
  
  # On groupe par mois et on fait la somme des occurences
  df_lesechos <- df_lesechos %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_latribune <- df_latribune %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_economist <- df_economist %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_guardian <- df_guardian %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # Calcul de la variance pour chaque journal
  echos_var <- var(df_lesechos$Occurences)
  tribune_var <- var(df_latribune$Occurences)
  economist_var <- var(df_economist$Occurences)
  guardian_var <- var(df_guardian$Occurences)
  
  # Divisions des occurences par l'écart type, ce qui nous donne un écart type unitaire
  df_lesechos$Occurences <- (df_lesechos$Occurences) / sqrt(echos_var)
  df_latribune$Occurences <- (df_latribune$Occurences) / sqrt(tribune_var)
  df_economist$Occurences <- (df_economist$Occurences) / sqrt(economist_var)
  df_guardian$Occurences <- (df_guardian$Occurences) / sqrt(guardian_var)
  
  # La normalisation de chaque série mensuelles des différents journaux nous permets de les combiner
  # Création du dataframe qui va faire la somme des deux dataframes
  df_france <- bind_rows(df_lesechos,df_latribune)
  df_france <- df_france %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_uk <- bind_rows(df_economist,df_guardian)
  df_uk <- df_uk %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # On divise par 2 la série obtenue car on a 2 sources d'informations
  df_france$Occurences <- df_france$Occurences / 2
  df_uk$Occurences <- df_uk$Occurences / 2
  
  # Calcul de la moyenne de la série 
  moyenne <- mean(df_france$Occurences)
  moyenne <- mean(df_uk$Occurences)
  
  # On ramène la série à une moyenne de 100 afin d'obtenir l'indicateur EPU de chaque mois
  df_france$Occurences <- df_france$Occurences*(100/moyenne)
  df_uk$Occurences <- df_uk$Occurences*(100/moyenne)
  
  if(pays == "Royaume-Uni" ){
    epu <- ts(df_uk$Occurences, start=c(2018, 1), end=c(2021, 01), frequency=12)
    fit <- HoltWinters(epu)
    # Précision de prévision
    accuracy(forecast(fit))
    # Prédire les 5 prochains mois
    forecast(fit, 5)
    plot(forecast(fit, 5), 
         main = "Prédiction des 5 prochains mois",
         xlab = "Années",
         ylab = "EPU")  }else if(pays == "France"){
    epu <- ts(df_france$Occurences, start=c(2018, 1), end=c(2021, 01), frequency=12)
    fit <- HoltWinters(epu)
    # Précision de prévision
    accuracy(forecast(fit))
    # Prédire les 5 prochains mois
    forecast(fit, 5)
    plot(forecast(fit, 5), 
         main = "Prédiction des 5 prochains mois",
         xlab = "Années",
         ylab = "EPU")
  }else{
  }
}


  

