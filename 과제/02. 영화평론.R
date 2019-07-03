library(rvest)
library(stringr)
library(dplyr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

url_base <- 'https://movie.naver.com'
start_url <- '/movie/bi/mi/point.nhn?code='
code <- '176040#tab'
(url <- paste0(url_base, start_url, code, encoding="euc-kr"))
html <- read_html(url)

(html %>%
  html_node('iframe.ifr') %>%
  html_attr('src') -> url2)

ifr_url <- paste0(url_base, url2) 

ifr_url
page <- c(1:11)

tot_review <- data.frame()
for (pg in 1:11) {
  url <- paste0 (ifr_url, '&page=', page[pg])
  print(url)
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
    print(score)
    print(review)
    print(writer)
    print(time)
  }
  
  review = data.frame(score=score, review=review, writer=writer, time=time)
  tot_review <- rbind.data.frame(tot_review, review) 
}

View(tot_review)

class(tot_review$score)
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
tot_review$score <- as.numeric.factor(tot_review$score)
class(tot_review$score)

ordered <- tot_review %>%
  arrange(desc(score))
View(ordered)

write.csv(ordered, "navermoview.csv")




