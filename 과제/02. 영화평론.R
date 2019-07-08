library(rvest)
library(stringr)
library(dplyr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

url_base <- 'https://movie.naver.com' 
start_url <- '/movie/bi/mi/point.nhn?code='
code <- '101966#tab'
url <- paste0(url_base, start_url, code, encoding="euc-kr")
html <- read_html(url) %>%
  html_node('iframe.ifr') %>% html_attr('src') -> url2

ifr_url <- paste0(url_base, url2) 

count <- read_html(ifr_url) %>%  html_node("div.score_total") %>%
         html_node(".total") %>% html_text('em')

index <- str_locate(count, "총") 
count <- str_sub(count, index[1]+1, -2)
totcount <- as.numeric(gsub(",","", count)) 

(pages <- ceiling(totcount/10))

tot_review <- data.frame()
for (pg in 1:pages) {
  url <- paste0 (ifr_url, '&page=', pg)
  if (pg %% 100 == 0)
    print (pg)
  html2 <- read_html(url) %>%
    html_node('div.score_result') %>%
    html_nodes('li') -> lis
  
  score <- c()  ; review <- c()  ; writer <- c()  ;  time <- c()
  for (li in lis) {
    score <- c(score, html_node(li, '.star_score') %>% html_text('em') %>% trim())
    
    (tmp <- li %>%
        html_node('.score_reple') %>%
        html_text ('p')  %>%
        trim())
    
    index <- str_locate(tmp, "\r")
    (review <- c(review, 
                 str_sub(tmp, 1, index[1]-1)))
    tmp <- trim(str_sub(tmp, index[1], -1))
    index <- str_locate(tmp, "\r")
    (writer <- c(writer, str_sub(tmp, 1, index[1]-1)))
    tmp <- trim(str_sub(tmp, index[1], -1))
    index <- str_locate(tmp, "\r")
    (time <- c(time, str_sub(tmp, 1, index[1]-1)))
    # print(score)
    # print(review)
    # print(writer)
    # print(time)
  }
  
  review = data.frame(score=score, review=review, writer=writer, time=time, stringsAsFactors = F)
  tot_review <- rbind.data.frame(tot_review, review) 
}

class(tot_review$score)
tot_review$score <- factor(tot_review$score, levels = c(1:10), ordered = T)
ordered <- tot_review %>%
  arrange(desc(score))
View(ordered)

rev <- ordered$review
str(rev)
View(rev)
write.csv(ordered, "navermovie_toystory.csv")
















