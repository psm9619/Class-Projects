library(abind)
library(rvest)
library(dplyr)
library(stringr)
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)}

# 세계 , IT/과학, 오피니언, 연합뉴스 속보
base_url <- "https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1="
category <- c("정치", "경제", "사회", "생활/문화", "세계" , "IT/과학", "오피니언")
cat_n <- c(100, 101, 102, 103, 104,105,110)

# news <- createWorkbook()

tot_news <- data.frame()
for (n in 1:7) {
  url <- paste0(base_url, cat_n[n]) ; print(url) 
  cat(cat_n[n], "\n") 
  html <- read_html(url)
# titles -------
  tit1 <- html %>%
    html_node('#main_content') %>%   html_node(".type06_headline") %>%  
    html_nodes ("dt") %>%  html_text()  %>% trim()  
  tit2 <- html %>%
    html_node('#main_content') %>%   html_node(".type06") %>%  
    html_nodes ("dt") %>%  html_text()  %>% trim() 
  tits <- c(tit1, tit2)
  titles <- c() 
  for (i in 1:length(tits)) {
    if (tits[i] %in% c("", "동영상기사"))  
      titles
    else  {
      a <- gsub('\\"', "", tits[i])
      titles <- c(titles, a)
    }
  }
# writing -------
  writing1 <- html %>%
    html_node('#main_content') %>%
    html_node(css = ".type06_headline") %>%
    html_nodes (".writing") %>%
    html_text()
  writing2 <- html %>%
    html_node('#main_content') %>%
    html_node(css = ".type06") %>%
    html_nodes (".writing") %>%
    html_text()
  writing <- c(writing1, writing2)
# article -------  
  art1 <- html %>%
    html_node('#main_content') %>%
    html_node(".type06_headline") %>%
    html_nodes (".lede") %>%
    html_text()
  art2 <- html %>%
    html_node('#main_content') %>%
    html_node(".type06") %>%
    html_nodes (".lede") %>%
    html_text()
  arts <- c(art1, art2)
  articles <-c()
  for (i in 1:length(arts)) {
    if (arts[i] %in% c("")) 
      articles
    else  {
      a <- gsub('\\"', "", arts[i])
      articles <- c(articles, a)
    }
  }
 df_news <- data.frame(categories = rep(category[n], length(titles)), 
                       titles= titles, writing = writing, articles = articles)
 tot_news <- rbind.data.frame (tot_news, df_news)
}
View(tot_news)













