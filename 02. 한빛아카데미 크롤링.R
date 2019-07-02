# 관련 R 패키지 및 함수
# - 요청 : curl, httr, rvest, RSelenium
# - 정리 : 정규표현식, jsonlite, rvest
# - 저장 : write.*()
# - 반복 : for, parallel
# - 예외처리 : try, if
# - 최적화 : 

# rvest -> node, attr, text 만 기억하면 사용가능
# - node 란 html 의 tag
# - attr 은 attriubute 의 줄임, tag 아래 속성
# - text
# 
# - CSS 선택자를 가지고 node (즉 tag) 를 찾는다
#  : tag, #id, .class, [attr="val"], tag#id, tag.class, tag[attr="val"]

#---------------------------------------------------------------------------
# 한빛아카데미 도서 크롤링
library(rvest)
library(dplyr)
library(stringr)
trim <- function (x) {
  gsub("^\\s+|\\s+$", "", x)}
install.packages("openxlsx")
library(openxlsx)


base <- 'http://www.hanbit.co.kr/academy/books/new_book_list.html'
read_html(base) %>%
  html_node('#category_books') %>%
  html_nodes('a') %>%
  html_attr('href') -> category_urls
category_urls

l <- read_html(category_urls[11]) %>%
  html_node('.paginate')   %>%
  html_nodes('a') %>%
  html_attr('href') 
lis <- c(category_urls[11], paste0(base_forpage, l))
lis

base_forpage <- 'http://www.hanbit.co.kr'

total_books <- data.frame()
for(i in 1:length(category_urls)) {
  lis <- c()
  l <- read_html(category_urls[i]) %>%
    html_node('.paginate')   %>%
    html_nodes('a') %>%
    html_attr('href') 
  lis <- c(lis, category_urls[i], paste0(base_forpage, l))
  
  cat('\n', i, '\n')
  for (k in 1:length(lis)) {
    if (lis[k] != base_forpage) {
      htm <- read_html(lis[k]) %>%
        html_node('.sub_book_list_area') %>%
        html_nodes('li') -> sub_books

       price <- c() ;  title <- c() ; writer <- c()
      for (li in sub_books ) {
        pr <- html_node(li, '.price') %>% html_text() 
        price <- c(price, gsub ("\\\\", "", pr))
        title <-c(title, html_node(li, '.book_tit') %>% html_text())
        writer <- c(writer, html_node(li, '.book_writer') %>% html_text())
      }
      books <- data.frame(Title = title, Writer = writer, Price = price)
      category_books <- data.frame(category_books, books)    
      }
  }
}






cat_books <- data.frame()
total_books <- data.frame()
cat_num <- c()
for (i in 1:length(lis)) {
  if (lis[i] %in% category_urls){
    cat_num <- c(cat_num, i)
  }
  if (lis[i] != base_forpage)
    htm <- read_html(lis[i]) %>%
    html_node('.sub_book_list_area') %>%
    html_nodes('li') -> sub_books
  
  cat_n <- c()
  price <- c()
  title <- c()
  writer <- c()
  for (li in sub_books ) {
    pr <- html_node(li, '.price') %>% html_text() 
    pr <- gsub ("\\\\", "", pr)
    ti <- html_node(li, '.book_tit') %>% html_text()
    wr <- html_node(li, '.book_writer') %>% html_text()
    
    cat_n <- c(cat_n, )
    title <- c(title, ti)
    writer <- c(writer, wr)
    price <- c(price, pr)
  }
  books <- data.frame(Cat = cat_n , Title = title, Writer = writer, Price = price)
  print(books)
}


for (i in 1:11) {
  base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
  page <- 'page='
  ctgry <- categories[i]
  
  for (n in 1:6) {
    cat('[',i,n,']')
    
    url <- paste0(base_url, page, 1, ctgry[1])
    html <- read_html(url)
    html %>%
      html_node('.sub_book_list_area') %>%
      html_nodes('li') -> lis2
    
    price <- c()
    title <- c()
    writer <- c()
    for (li in lis2 ) {
      pr <- html_node(li, '.price') %>% html_text() 
      pr <- gsub ("\\\\", "", pr)
      ti <- html_node(li, '.book_tit') %>% html_text()
      wr <- html_node(li, '.book_writer') %>% html_text()
      
      title <- c(title, ti)
      writer <- c(writer, wr)
      price <- c(price, pr)
    }
    books <- data.frame(Title = title, Writer = writer, Price = price)
    print(books)
  }
}
  
  for (n in 1:6) {
  print(n)
  base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
  page <- 'page='
  category <- '&cate_cd=004007'
  
  url <- paste0(base_url, page,n, category)
  html <- read_html(url)
  
  # book_list <- html_node(html, '.sub_book_list_area')
  # book_list
  # (lis <- html_nodes(book_list, 'li'))
  # dplyr 기능을 이용하면 훨씬 간단히 해결
  html %>%
    html_node('.sub_book_list_area') %>%
    html_nodes('li') -> lis2
  lis2
  
  price <- c()
  title <- c()
  writer <- c()
  for (li in lis2 ) {
    pr <- html_node(li, '.price') %>% html_text() 
    pr <- gsub ("\\\\", "", pr)
    ti <- html_node(li, '.book_tit') %>% html_text()
    wr <- html_node(li, '.book_writer') %>% html_text()
    
    title <- c(title, ti)
    writer <- c(writer, wr)
    price <- c(price, pr)
  }
  books <- data.frame(Title = title, Writer = writer, Price = price)
  print(books)
  computer_books <- rbind(computer_books, books)
}

computer_books















