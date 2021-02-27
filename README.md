---
title: "Utilisation des techniques de text-mining pour mesurer les anticipations d’inflation en France et au Royaume-Uni"
subtitle: "Banque de France"
author: "Alan CUZON"
date: "28 février 2021"
---


## Présentation

Ce projet consiste à rechercher la fréquence d'apparition de certains mots-clés relatifs à l'incertitude économique au sein de la presse économique, en France et au Royaume-Uni. La méthode utilisée s'approche de celle de  Bloom et al.


- &nbsp;Les résultats et la visualisation des données sont visible via le lien suivant : **https://epu-fr-uk-alancuzon.shinyapps.io/app-1/**


- &nbsp;Le fichier Excel suivant contient les bases de données mensuelles : **resultats-epu.xls**


- &nbsp;Le projet est également disponible sur GitHub : 


## Structure du projet 

Le projet a été réalisé sous R avec RStudio. La structure du projet est la suivante :


- Le fichier **initialisation.R** est le fichier principal. Il contient toutes les instructions pour la récupération et le traitement des données, ainsi que la visualisation.

- Le dossier **functions** contient toutes les fonctions nécessaires. Pour chaque journal, il y un fichier associé contenant les fonctions de web-scrapping et de text-mining. Il existe également une fonction qui permet de créer le fichier Excel, une fonction de calcul de l'indice EPU ainsi qu'une fonction de prévision.

- Le dossier **data** contient les données récupérées au format .csv.

- Le dossier **plot** contient les graphique générés lors de l'analyse statistique et de la prédiction des valeurs.

- Le fichier Excel **"resultat-epu.xls"** contient la base de données mensuelles de l'indice EPU qui a été calculé pour la France et le Royaume-Uni ainsi que les occurences mensuelles des mots-clés relatifs à l'incertitude économique.

- Le dossier **App-1** (application Shiny pour visualiser les données avec un graphique interactif pour chaque pays).


## Web-Scrapping et Text-Mining

Les packages **rvest** et **tm** ont été utilisés afin de réaliser les opérations de web-scrapping de text-mining. Pour chaque journal, les fonctions suivantes ont été réalisées : 

- fonction de récupération du nombre de pages de la catégorie économique présentes sur le site, 
- fonction de récupération des urls des pages contenant les articles, 
- fonction de récupération des urls des articles et, 
- fonction de calcul du nombre d'occurences.

Les données de chaque journal sont stockées dans un dataframe.

## Calcul de l'indice EPU (Economic Policy Uncertainty)


Le script "epu.R" permet le calcul de l'indice EPU. Pour chaque journal, l'indice EPU a été calculé selon ces étapes :

- Calcul de la **variance $\sigma^2$** de la série chronologique pour chaque journal,

- Normalisation des données en divisant chaque mois par **l'écart-type $\sigma$**. On obtient ainsi une série chronologique avec un **écart-type unitaire**,

- Calcule de la **moyenne** pour chaque mois de cette série afin d'obtenir une nouvelle série,

- Calcule de la **moyenne M** de cette nouvelle série,

- Multiplication par **(100/M)** pour chaque mois de cette série,

- La normalisation de ces données nous permets d'**additioner** les séries produites par les différents journaux d'un même pays,

- Pour finir, on **divise par 2** la série obtenue précédément, étant donnée qu'on utilise deux sources d'informations pour chaque pays. On obtient ainsi l'indice EPU normalisé.


## Prévision des valeurs 

Le script **"prediction.R"** permet de prédire les valeurs futures concernant l'indice EPU des cinq prochains mois en France et au Royaume-Uni.

- On convertit d'abord le vecteur en série temporelle avec la fonction **ts()**,

- La procédure de décomposition analyse les indices saisonniers et la variation à l'intérieur de chaque saison de la série chronologique (Voir Figure 1 & Figure 2),

- Lissage exponentiel avec la méthode de **Holt-Winters** avec prise en charge de la saisonnalité,

- La fonction **forecast()** nous permet de prédire les valeurs des cinq prochains mois (prévision à court terme pour augmenter la précision du modèle) (Voir Figure 3 & Figure 4).


## Choix des solutions mises en oeuvres

Les données ont été collecté sur les sites **Les Echos** et **La Tribune** pour la France, et sur les sites **The Economist** et **The Guardian** pour le Royaume-Uni. Certains sites présentent plus de difficultés que d'autres concernant le web-scrapping, comme par exemple un code html ou css qui peut varier selon les articles. Concerant ce projet, j'ai choisis de commencer l'analyse à partir de janvier 2018 car le site internet Les Echos ne présente pas d'articles antérieurs à cette date là (il n'y a pas d'archives sur le site internet).


&nbsp;De plus, le résultat final concernant le calcul et la prédiction de l'indice EPU peut être faussé. Certaines sources nécessitent un abonnement (sans lequel de nombreux articles ne sont pas accessibles, voir la totalité des articles comme le Financial Times), ce qui peut donc réduire considérablement la fréquence d'apparition des mots-clés.




