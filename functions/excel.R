
excel <- function(){
  
  # On ordonne les données
  df_lesechos_xls <- read.csv(file = "data/df_lesechos.csv")
  df_latribune_xls <- read.csv(file = "data/df_latribune.csv")
  df_economist_xls <- read.csv(file = "data/df_economist.csv")
  df_guardian_xls <- read.csv(file = "data/df_guardian.csv")
  
  # On supprime le jour pour le remplacer par 01
  df_lesechos_xls$Date <- substr(df_lesechos_xls$Date,1,7)
  df_latribune_xls$Date <- substr(df_latribune_xls$Date,1,7)
  df_lesechos_xls$Date <- paste(df_lesechos_xls$Date,"-01",sep="")
  df_latribune_xls$Date <- paste(df_latribune_xls$Date,"-01",sep="")
  df_economist_xls$Date <- substr(df_economist_xls$Date,1,7)
  df_guardian_xls$Date <- substr(df_guardian_xls$Date,1,7)
  df_economist_xls$Date <- paste(df_economist_xls$Date,"-01",sep="")
  df_guardian_xls$Date <- paste(df_guardian_xls$Date,"-01",sep="")
  
  # Transforme la colonne Occurences au format numérique
  df_lesechos_xls <- transform(df_lesechos_xls, Occurences = as.numeric(Occurences))
  df_latribune_xls <- transform(df_latribune_xls, Occurences = as.numeric(Occurences))
  df_economist_xls <- transform(df_economist_xls, Occurences = as.numeric(Occurences))
  df_guardian_xls <- transform(df_guardian_xls, Occurences = as.numeric(Occurences))
  
  # On convertit la colonne date au format Date
  df_lesechos_xls <- transform(df_lesechos_xls, Date = as.Date(Date))
  df_latribune_xls <- transform(df_latribune_xls, Date = as.Date(Date))
  df_economist_xls <- transform(df_economist_xls, Date = as.Date(Date))
  df_guardian_xls <- transform(df_guardian_xls, Date = as.Date(Date))
  
  # On élimine les valeurs datant d'avant 2018
  df_lesechos_xls <- df_lesechos_xls[!(df_lesechos_xls$Date < "2018-01-01"),]
  df_latribune_xls <- df_latribune_xls[!(df_latribune_xls$Date < "2018-01-01"),]
  df_economist_xls <- df_economist_xls[!(df_economist_xls$Date < "2018-01-01"),]
  df_guardian_xls <- df_guardian_xls[!(df_guardian_xls$Date < "2018-01-01"),]
  df_lesechos_xls <- df_lesechos_xls[!(df_lesechos_xls$Date > "2020-01-01"),]
  df_latribune_xls <- df_latribune_xls[!(df_latribune_xls$Date > "2020-01-01"),]
  df_economist_xls <- df_economist_xls[!(df_economist_xls$Date > "2020-01-01"),]
  df_guardian_xls <- df_guardian_xls[!(df_guardian_xls$Date > "2020-01-01"),]
  
  # On groupe par mois et on fait la somme des occurences
  df_lesechos_xls <- df_lesechos_xls %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_latribune_xls <- df_latribune_xls %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_economist_xls <- df_economist_xls %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_guardian_xls <- df_guardian_xls %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  # On convertit la colonne date au format Character 
  df_lesechos_xls <- transform(df_lesechos_xls, Date = as.character(Date))
  df_latribune_xls <- transform(df_latribune_xls, Date = as.character(Date))
  df_economist_xls <- transform(df_economist_xls, Date = as.character(Date))
  df_guardian_xls <- transform(df_guardian_xls, Date = as.character(Date))
  
  df_global <- transform(df_global, Date = as.character(Date))
  
  df_france <- bind_rows(df_lesechos_xls,df_latribune_xls)
  df_france <- df_france %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  df_uk <- bind_rows(df_guardian_xls,df_economist_xls)
  df_uk <- df_uk %>% group_by(Date) %>% summarise(Occurences = sum(Occurences)) %>% arrange(desc(Date))
  
  
  # Création du fichier excel 
  
  filename <- "resultat-epu.xlsx"
  wb <- createWorkbook(type="xlsx")
  # Créer une feuille dans le classeur Excel pour contenir les données 
  sheet_global <- createSheet(wb, sheetName = "Global")
  sheet_fr <- createSheet(wb, sheetName = "France")
  sheet_uk <- createSheet(wb, sheetName = "Royaume-Uni")
  
  # Titre
  xlsx.addHeader(wb, sheet_global, value="Global Economic Policy Uncertainty Index",level=1, 
                 color="black", underline=1)
  xlsx.addLineBreak(sheet_global, 1)
  # Description
  paragraph="Cette feuille Excel contient les données relatives à l'indice EPU de la France et du Royaume-Uni. Les indices EPU sont calculés à partir du nombre de mots concernant l'incertitude économique présent au sein des articles de différents journaux de chaque pays."
  xlsx.addParagraph(wb, sheet_global, paragraph, fontSize=14, isItalic=TRUE, 
                    fontColor="darkred", colSpan=10, rowSpan=10)
  # Informations 
  auteur=paste("Auteur : Alan CUZON. \n",
               "cuzonalan@gmail.com.", sep="")
  xlsx.addParagraph(wb, sheet_global,value=auteur, isItalic=TRUE, colSpan=5, 
                    rowSpan=4, fontColor="darkgray", fontSize=14)
  xlsx.addLineBreak(sheet_global, 3)
  # Ajouter les data.frames
  xlsx.addHeader(wb, sheet_global,value="Base mensuelle de l'indice EPU France et Royaume-Uni", startCol=2, startRow=22)
  xlsx.addTable(wb, sheet_global, df_global, startCol=2, startRow=25)
  
  # Titre
  xlsx.addHeader(wb, sheet_fr, value="France Economic Policy Uncertainty Index",level=1, 
                 color="black", underline=1)
  xlsx.addLineBreak(sheet_fr, 1)
  # Description
  paragraph="Cette feuille Excel contient le nombre de mots concernant l'incertitude économique présent au sein des articles du journal Les Echos et La Tribune sur une durée de temps donnée." 
  xlsx.addParagraph(wb, sheet_fr, paragraph, fontSize=14, isItalic=TRUE, 
                    fontColor="darkred", colSpan=10, rowSpan=10)
  # Informations 
  auteur=paste("Auteur : Alan CUZON. \n",
               "cuzonalan@gmail.com.", sep="")
  xlsx.addParagraph(wb, sheet_fr,value=auteur, isItalic=TRUE, colSpan=5, 
                    rowSpan=4, fontColor="darkgray", fontSize=14)
  xlsx.addLineBreak(sheet_fr, 3)
  # Ajouter les data.frames
  xlsx.addHeader(wb, sheet_fr,value="Données mensuelles des occurences en France")
  xlsx.addTable(wb, sheet_fr, df_france)
  xlsx.addLineBreak(sheet_fr)

  # Titre
  xlsx.addHeader(wb, sheet_uk, value="Global Economic Policy Uncertainty Index",level=1, 
                 color="black", underline=1)
  xlsx.addLineBreak(sheet_uk, 1)
  # Description
  paragraph="Cette feuille Excel contient le nombre de mots concernant l'incertitude économique présent au sein des articles du journal The Guardian et The Economist sur une durée de temps donnée."
  xlsx.addParagraph(wb, sheet_uk, paragraph, fontSize=14, isItalic=TRUE, 
                    fontColor="darkred", colSpan=10, rowSpan=10)
  # Informations 
  auteur=paste("Auteur : Alan CUZON. \n",
               "cuzonalan@gmail.com.", sep="")
  xlsx.addParagraph(wb, sheet_uk,value=auteur, isItalic=TRUE, colSpan=5, 
                    rowSpan=4, fontColor="darkgray", fontSize=14)
  xlsx.addLineBreak(sheet_uk, 3)
  # Ajouter les data.frames
  xlsx.addHeader(wb, sheet_uk,value="Données mensuelles des occurences au Royaume-Uni", startCol=2, startRow=22)
  xlsx.addTable(wb, sheet_uk, df_uk, startCol=2, startRow=25)
  
  # Enregistrer le classeur dans un fichier
  saveWorkbook(wb, filename)
}


