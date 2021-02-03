# laTribuneScrap <- function(url = "https://www.latribune.fr/actualites/economie/economie.html"){
#   tribune_articles <- read_html(url)
#   tribune_urls <- tribune_articles %>% html_nodes("article") %>% html_nodes("a") %>% html_attr("href")
#   unique(tribune_urls)
#   }
#