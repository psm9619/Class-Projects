library(rJava)
library(dplyr)
library(KoNLP) 
library(ggwordcloud)
library(wordcloud)
library(wordcloud2)
library(tm)
library(stringr)
library(ggplot2)
useSejongDic()
#mergeUserDic(data.frame("")) -> 내가 필요로 하는 단어가 사전에 없을 시 추가하는 것 (ex. 지명)
library(RColorBrewer)
View(data1)
data1 <- read.csv("navermovie_toystory.csv", stringsAsFactors = F)
data1$review[c(4770, 4774)] <- "빡침 진짜 3에서 완벽하게 끝내놓고 보니는 결국 우디를 버렸고
그걸 포장하기 위해 우디가 자기 삶을 찾았다고 한 듯 난 마지막에 보니가 우디를 다시 사랑할 줄 알았는데
보니도 ㅗ 쉽게 헤어지는 버즈랑 우디도 그렇고 간다니까 보내준 친구들도 이해 안 됨 
차라리 보지 말 걸 내 마음속에 있는 아름다운 엔딩이 거지 같아짐 돈 벌라고 한 편 더 만든 느낌 쉣 "

data_review <- data1$review

review_2 <- sapply(data_review, extractNoun, USE.NAMES = F)
head(review_2) 
# 더 이상 processing 에러가 뜨지 않음을 확인할 수 있다.
# 아마도 spacing 문제로 인해 noun extraction 이 성공적으로 이루어지지 않은 경우가 몇 가지 눈에 띈다. (ex. 아시아최초로, 같은거왜보냐정신연령이그정도 등 )
# 그러나 전체적으로는 양호한 면을 보이므로 전처리 중에 해결하도록 한다. 

review_2 <- unlist(review_2)

length(table(review_2))  # 현재 단어의 총 수는 8826 


# 전처리를 위해 데이터를 훑는 방법으로 전체 데이터 length 에서 50 region index 를 random generator 을 이용해 뽑았다.
set.seed(123)
index <- sample(1:(length(review_2)-31), 50, replace=FALSE)
index  
# 각 인덱스로부터 30씩 데이터를 뽑아서 새로운 list 에 저장한다. 
test <- c()
for (i in index) {
  test <- c(test, review[i:(i+29)])
} 
# 총 3000 개의 데이터가 있으므로 300씩 끊어 검사하며 전처리에 사용할 데이터를 t_gsub list 로 저장한다.

# test[1:300] ; test[301:600] ; test[601:900] ; test[901:1200] ; test[1201:1500]
t_gsub <- c("\\W", "\\d+",'[ㄱ-ㅎ]',"(ㅜ|ㅠ)","[[:punct:][:lower:][:upper:]]", 
             "였\\S*", "줄\\S*", "까지\\S*", "합니\\S*","들이\\S*", 
            "어쩐지", "냐\\S*", "쥐\\S*", "관람객", "토이스토\\S*" )
write(t_gsub, 'sub.txt')
sub <- readLines('sub.txt')
# 지속적인 활용을 위해 replace 해야 할 데이터를 txt 파일로 별개저장한다.
# 해당 데이터를 하나씩, review_2 를 카피한 review_replaced 에 적용 한다. 이 때, sub 하나마다 다시 review_replaced 를 처음부터 끝까지 반복 적용해야 하므로 중첩 사용된다.
review_replaced <- review_2
for (i in sub) {
   for (k in 1:length(review_2)) {
      review_replaced [k] <- gsub(i, "", review_replaced[k])
   }
} 

# 추가적으로 위에서 검토 중에 눈에 띄던 '관람객' + '..' 와 같이 주된 단어 뒤에 내용이 추가될시 제대로 파악되지 않는 경우를 해결하기 위해 약간의 전처리를 더 해준다.
# 이 떄 데이터의 선택은 count 빈도수가 <= 2 인 경우로 잡는다. 
t <- table(review_replaced)
length(t)  # 현재 단어의 총 수는 7636 위의 전처리 후 대략 1000 이 감소. 
repeated <- c()
for (i in 1:length(t)) {
   if (t[i] <= 2) {
      
   }
       repeated <- c(repeated, t[i])
} 
# ~6400 여개의 데이터가 뽑히는데 이 중 500개씩 훑어서 같은 맥락으로 5번 이상 반복되는 것으로 보이는 단어들만 뽑아 전처리를 추가한다.
# repeated[1:500] 
# repeated[501:1000]
# repeated[1001:1500]
# repeated[1501:2000]
# repeated[2001:2500]
# repeated[2501:3000]
# repeated[3001:3500]
# repeated[3501:4000]
# repeated[4001:4500]
# repeated[4501:5000]
# repeated[5001:5500]
# repeated[5501:6000]
# repeated[-c(1:6000)]


rep <- c( "가족", "가정", "감동", "강추", "결말", "기대", "기본", "끝", "나이", "너무", "노잼", "눈물","다른","다시", "다음", "대박", "덕후", "동심","딸",
          "레전드", "렉스", "마음","마지막", "매력", "먹먹", "명작", "뭉클", "미국", "버즈", "번외","별로","보니", "분리","빌런", "사랑","새드엔딩", "새로운",
          "생각","세대","소름", "손수건","스토리", "시리즈", "실망","아이들","아이언맨","알라딘","암","앤디","어른","어벤져스","억지", "우디","울컥", 
          "엔딩", "역대급", "완벽", "이별", "인생영화", "인형", "자유", "장난감", "재미", "존윅", "졸귀", "짱짱","최고", "추억", "친구","캐붕", "페미",
          "포키", "힐링", "필요","하품","함께", "행복","보니", "보핍")

review_replaced_2 <- review_replaced
for (i in rep) {
   r <- paste0(i, "\\S*")
   for (k in 1:length(review_replaced_2)) {
      review_replaced_2 [k] <- gsub(r, i, review_replaced_2[k])
   }
}
head(review_replaced_2) ; head(review_replaced)  # 기존의 review_replaced 와 비교시, 위의 추가 전처리 후에는 '보니야' 라는 단어가 '보니' 로 줄어들었음을 볼 수 있다.  
length(table(review_replaced_2))  # 현재 단어 수는 6675 으로 다시 1000 정도 감소한다.

review_replaced_3 <- review_replaced_2

review_replaced_3 <- gsub ("개재\\S*","재미", review_replaced_3)
review_replaced_3 <- gsub ("고마\\S*", "고마움", review_replaced_3)
review_replaced_3 <-gsub ( "깜\\S*" , "깜짝", review_replaced_3)
review_replaced_3 <-gsub ("재\\S*", "재미", review_replaced_3)
review_replaced_3 <-gsub ("기다\\S*", "기다림", review_replaced_3)
review_replaced_3 <-gsub ("꿀\\S*", "재미", review_replaced_3)
review_replaced_3 <-gsub ( "놀\\S*","놀다", review_replaced_3)
review_replaced_3 <-gsub ("느끼\\S*", "느낌", review_replaced_3)
review_replaced_3 <-gsub ("돈\\S*", "돈아까움", review_replaced_3)
review_replaced_3 <-gsub (  "돌아\\S*" , "돌아간기분", review_replaced_3)
review_replaced_3 <-gsub ("두번\\S*", "두번보세요", review_replaced_3)
review_replaced_3 <-gsub ("떠나보\\S*", "떠나보냄", review_replaced_3)
review_replaced_3 <-gsub ("떠\\S*", "떠남", review_replaced_3)
review_replaced_3 <-gsub ("또보\\S*" , "또보고싶음", review_replaced_3)
review_replaced_3 <-gsub ( "만나\\S*", "만남", review_replaced_3)
review_replaced_3 <-gsub ("많\\S*", "많음", review_replaced_3)
review_replaced_3 <-gsub ( "보고싶\\S*", "보고싶을거야", review_replaced_3)
review_replaced_3 <-gsub ( "못느\\S*", "못느낌", review_replaced_3)
review_replaced_3 <-gsub ("빵\\S*","빵빵터짐", review_replaced_3)
review_replaced_3 <-gsub ("슬\\S*", "슬픔", review_replaced_3)
review_replaced_3 <-gsub ( "애\\S*","아이들", review_replaced_3)
review_replaced_3 <-gsub ( "어린\\S*", "어린시절", review_replaced_3)
review_replaced_3 <-gsub ("울\\S*", "울음", review_replaced_3)
review_replaced_3 <-gsub ( "잊\\S*", "잊혀짐", review_replaced_3)
review_replaced_3 <-gsub ( "재밌\\S*", "재미", review_replaced_3)
review_replaced_3 <-gsub ("좋\\S*", "좋음", review_replaced_3)
review_replaced_3 <-gsub ("픽사\\S*", "픽사다움", review_replaced_3)

head(review_replaced_3)
length(table(review_replaced_3)) # 6035로 600정도 줄었다. 예상보다 시간대비 그리 효율적이지는 않았으나 시도에 의의를 두기로 한다.

# 이제 띄어쓰기 등의 문제로 extract noun 이 성공적으로 되지 못한 데이터 (nchar <= 1 | nchar >=10) 을 제거한다.
review_final <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10}, review_replaced_3)

table.review <- table(review_final) 
str(table.review)

df.review <- as.data.frame(table.review)  # using just df.review took too long time for loading
df.review.2 <-   df.review %>%
   filter (Freq >= 10)

library(devtools)
devtools::install_github("lchiffon/wordcloud2")


wordcloud2(df.review.2, figPath = "토이스토리_우디.png", size = 1.5,
           color = "random-dark")


# 위에서 에러가 난 바가 있는 데이터를 미리 fix 한다.
data1$review[c(4770, 4774)] <- "빡침 진짜 3에서 완벽하게 끝내놓고 보니는 결국 우디를 버렸고
그걸 포장하기 위해 우디가 자기 삶을 찾았다고 한 듯 난 마지막에 보니가 우디를 다시 사랑할 줄 알았는데
보니도 ㅗ 쉽게 헤어지는 버즈랑 우디도 그렇고 간다니까 보내준 친구들도 이해 안 됨 
차라리 보지 말 걸 내 마음속에 있는 아름다운 엔딩이 거지 같아짐 돈 벌라고 한 편 더 만든 느낌 쉣 "

## 아래는 평점이 10 ~ 8인, Positive review 로 분류된 데이터이다.

data2 <- data1 %>%
   filter(score >= 8)
pos_review_data <- data2$review   

pos_review <- sapply(pos_review_data, extractNoun, USE.NAMES = F)
pos_review <- unlist(pos_review)

# 전처리는 위에서 사용한 것과 같은 방법으로 한다.

sub <- readLines('sub.txt')

pos_2 <- pos_review
for (i in sub) {
   for (k in 1:length(pos_2)) {
      pos_2 [k] <- gsub(i, "", pos_2[k])
   }
} 
#----
pos_3 <- pos_2
for (i in rep) {
   r <- paste0(i, "\\S*")
   for (k in 1:length(pos_3)) {
      pos_3 [k] <- gsub(r, i, pos_3[k])
   }
}
#----
pos_4 <- pos_3

pos_4 <- gsub ("개재\\S*","재미", pos_4)
pos_4 <- gsub ("고마\\S*", "고마움", pos_4)
pos_4 <-gsub ( "깜\\S*" , "깜짝", pos_4)
pos_4 <-gsub ("재\\S*", "재미", pos_4)
pos_4 <-gsub ("기다\\S*", "기다림", pos_4)
pos_4 <-gsub ("꿀\\S*", "재미", pos_4)
pos_4 <-gsub ( "놀\\S*","놀다", pos_4)
pos_4 <-gsub ("느끼\\S*", "느낌", pos_4)
pos_4 <-gsub ("돈\\S*", "돈아까움", pos_4)
pos_4 <-gsub (  "돌아\\S*" , "돌아간기분", pos_4)
pos_4 <-gsub ("두번\\S*", "두번보세요", pos_4)
pos_4 <-gsub ("떠나보\\S*", "떠나보냄", pos_4)
pos_4 <-gsub ("떠\\S*", "떠남", pos_4)
pos_4 <-gsub ("또보\\S*" , "또보고싶음", pos_4)
pos_4 <-gsub ( "만나\\S*", "만남", pos_4)
pos_4 <-gsub ("많\\S*", "많음", pos_4)
pos_4 <-gsub ( "보고싶\\S*", "보고싶을거야", pos_4)
pos_4 <-gsub ( "못느\\S*", "못느낌", pos_4)
pos_4 <-gsub ("빵\\S*","빵빵터짐", pos_4)
pos_4 <-gsub ("슬\\S*", "슬픔", pos_4)
pos_4 <-gsub ( "애\\S*","아이들", pos_4)
pos_4 <-gsub ( "어린\\S*", "어린시절", pos_4)
pos_4 <-gsub ("울\\S*", "울음", pos_4)
pos_4 <-gsub ( "잊\\S*", "잊혀짐", pos_4)
pos_4 <-gsub ( "재밌\\S*", "재미", pos_4)
pos_4 <-gsub ("좋\\S*", "좋음", pos_4)
pos_4 <-gsub ("픽사\\S*", "픽사다움", pos_4)

pos_review_final <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10}, pos_4)



# -------
#--------


# 아래는 평점 7~4 대의 평론 분석이다.

data3 <- data1 %>%
   filter(score >= 4 & score <= 7) ; data3
mid_review_data <- data3$review   

mid_review <- sapply(mid_review_data, extractNoun, USE.NAMES = F)
mid_review <- unlist(mid_review)

# 전처리는 위에서 사용한 것과 같은 방법으로 한다.

sub <- readLines('sub.txt')

mid_2 <- mid_review
for (i in sub) {
   for (k in 1:length(mid_2)) {
      mid_2 [k] <- gsub(i, "", mid_2[k])
   }
} 
#----
mid_3 <- mid_2
for (i in rep) {
   r <- paste0(i, "\\S*")
   for (k in 1:length(mid_3)) {
      mid_3 [k] <- gsub(r, i, mid_3[k])
   }
}
#----
mid_4 <- mid_3

mid_4 <- gsub ("개재\\S*","재미", mid_4)
mid_4 <- gsub ("고마\\S*", "고마움", mid_4)
mid_4 <-gsub ( "깜\\S*" , "깜짝", mid_4)
mid_4 <-gsub ("재\\S*", "재미", mid_4)
mid_4 <-gsub ("기다\\S*", "기다림", mid_4)
mid_4 <-gsub ("꿀\\S*", "재미", mid_4)
mid_4 <-gsub ( "놀\\S*","놀다", mid_4)
mid_4 <-gsub ("느끼\\S*", "느낌", mid_4)
mid_4 <-gsub ("돈\\S*", "돈아까움", mid_4)
mid_4 <-gsub (  "돌아\\S*" , "돌아간기분", mid_4)
mid_4 <-gsub ("두번\\S*", "두번보세요", mid_4)
mid_4 <-gsub ("떠나보\\S*", "떠나보냄", mid_4)
mid_4 <-gsub ("떠\\S*", "떠남", mid_4)
mid_4 <-gsub ("또보\\S*" , "또보고싶음", mid_4)
mid_4 <-gsub ( "만나\\S*", "만남", mid_4)
mid_4 <-gsub ("많\\S*", "많음", mid_4)
mid_4 <-gsub ( "보고싶\\S*", "보고싶을거야", mid_4)
mid_4 <-gsub ( "못느\\S*", "못느낌", mid_4)
mid_4 <-gsub ("빵\\S*","빵빵터짐", mid_4)
mid_4 <-gsub ("슬\\S*", "슬픔", mid_4)
mid_4 <-gsub ( "애\\S*","아이들", mid_4)
mid_4 <-gsub ( "어린\\S*", "어린시절", mid_4)
mid_4 <-gsub ("울\\S*", "울음", mid_4)
mid_4 <-gsub ( "잊\\S*", "잊혀짐", mid_4)
mid_4 <-gsub ( "재밌\\S*", "재미", mid_4)
mid_4 <-gsub ("좋\\S*", "좋음", mid_4)
mid_4 <-gsub ("픽사\\S*", "픽사다움", mid_4)

mid_review_final <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10}, mid_4)

#--------
#--------

# 아래는 평점 3 점 이하의 평론 분석이다.

data4 <- data1 %>%
   filter(score <= 3) 
neg_review_data <- data4$review   

neg_review <- sapply(neg_review_data, extractNoun, USE.NAMES = F)
neg_review <- unlist(neg_review)

# 전처리는 위에서 사용한 것과 같은 방법으로 한다.

sub <- readLines('sub.txt')

neg_2 <- neg_review
for (i in sub) {
   for (k in 1:length(neg_2)) {
      neg_2 [k] <- gsub(i, "", neg_2[k])
   }
} 
#----
neg_3 <- neg_2
for (i in rep) {
   r <- paste0(i, "\\S*")
   for (k in 1:length(neg_3)) {
      neg_3 [k] <- gsub(r, i, neg_3[k])
   }
}
#----
neg_4 <- neg_3

neg_4 <- gsub ("개재\\S*","재미", neg_4)
neg_4 <- gsub ("고마\\S*", "고마움", neg_4)
neg_4 <-gsub ( "깜\\S*" , "깜짝", neg_4)
neg_4 <-gsub ("재\\S*", "재미", neg_4)
neg_4 <-gsub ("기다\\S*", "기다림", neg_4)
neg_4 <-gsub ("꿀\\S*", "재미", neg_4)
neg_4 <-gsub ( "놀\\S*","놀다", neg_4)
neg_4 <-gsub ("느끼\\S*", "느낌", neg_4)
neg_4 <-gsub ("돈\\S*", "돈아까움", neg_4)
neg_4 <-gsub (  "돌아\\S*" , "돌아간기분", neg_4)
neg_4 <-gsub ("두번\\S*", "두번보세요", neg_4)
neg_4 <-gsub ("떠나보\\S*", "떠나보냄", neg_4)
neg_4 <-gsub ("떠\\S*", "떠남", neg_4)
neg_4 <-gsub ("또보\\S*" , "또보고싶음", neg_4)
neg_4 <-gsub ( "만나\\S*", "만남", neg_4)
neg_4 <-gsub ("많\\S*", "많음", neg_4)
neg_4 <-gsub ( "보고싶\\S*", "보고싶을거야", neg_4)
neg_4 <-gsub ( "못느\\S*", "못느낌", neg_4)
neg_4 <-gsub ("빵\\S*","빵빵터짐", neg_4)
neg_4 <-gsub ("슬\\S*", "슬픔", neg_4)
neg_4 <-gsub ( "애\\S*","아이들", neg_4)
neg_4 <-gsub ( "어린\\S*", "어린시절", neg_4)
neg_4 <-gsub ("울\\S*", "울음", neg_4)
neg_4 <-gsub ( "잊\\S*", "잊혀짐", neg_4)
neg_4 <-gsub ( "재밌\\S*", "재미", neg_4)
neg_4 <-gsub ("좋\\S*", "좋음", neg_4)
neg_4 <-gsub ("픽사\\S*", "픽사다움", neg_4)

neg_review_final <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10}, neg_4)

#--------
table.pos <- table(pos_review_final)
table.mid <- table(mid_review_final) 
table.neg <- table(neg_review_final) 

df.pos <- as.data.frame(table.pos) %>% arrange(desc(Freq)) %>%
   mutate(review = pos_review_final)
df.mid <- as.data.frame(table.mid) %>% arrange(desc(Freq)) %>%
   mutate(review = mid_review_final)
df.neg <- as.data.frame(table.neg) %>% arrange(desc(Freq)) %>%
   mutate(review = neg_review_final)

df.pos_2 <- df.pos %>%
   select(review, Freq) %>%
   head(200) 
df.mid_2 <- df.mid %>%
   select(review, Freq) %>%
   head(200) 
df.neg_2 <- df.neg %>%
   select(review, Freq) %>%
   head(200)

ggplot(d, aes(label = review, size=pct)) +
   geom_text_wordcloud_area(shape = "circle") +
   scale_color_gradientn(colours = rainbow(5)) +
   scale_size_area(max_size=25) +
   theme_minimal() +  facet_wrap(~label)


par(mfrow=c(1,2))
palete <- brewer.pal(10, "Spectral")
w1 <- wordcloud(names(table.pos), freq = table.pos, scale = c(4,0.5),
          rot.per=0.25, min.freq=20, max.words=100, 
          random.order=F, random.color=T, colors=palete)


set.seed(42)
for (i in dfs) {
   print(
      ggplot(i, aes(label = review, size=Freq, color=Freq)) +
         geom_text_wordcloud_area(shape="diamond") +
         scale_size_area(max_size=25) +
         theme_minimal() +  facet_wrap(~type)
   )
}



ggplot(df.neg_2, aes(label = neg_review_final, size = Freq)) +
   geom_text_wordcloud_area(
      mask = png::readPNG(system.file("extdata/hearth.png",
                                      package = "ggwordcloud", mustWork = TRUE))) +
   scale_size_area(max_size = 18) +
   theme_minimal()





table.pos <- table(pos_review_final) 
str(table.pos)

df.pos <- as.data.frame(table.pos)  # using just df.review took too long time for loading
df.pos.2 <-   df.pos %>%
   filter (Freq >= 10)
palete <- brewer.pal(9, "Set3")

wordcloud2(df.review.2, figPath = "토이스토리_우디.png", size = 1.5,
           color = "random-light", background="grey15")

ggplot(df.review)
par(mfrow=c(1,3))
for (i in dfs) {
  print(wordcloud2(i, 
              size=2, col="random-light", 
              backgroundColor="black"))
}


wordcloud2(po, 
           size=2, col="random-light", 
           backgroundColor="black")

wordcloud2(dfs[2], 
           size=2, col="random-light", 
           backgroundColor="black")


View(data1)


wordcloud2(df.pos_2, 
           size=1, col="random-light", 
           backgroundColor="black")

wordcloud2(df.mid_2, 
           size=1, col="random-light", 
           backgroundColor="black")

wordcloud2(df.neg_2, 
           size=1, col="random-light", 
           backgroundColor="black")

pos_10 <- df.pos_2 %>% head(10) 
mid_10 <- df.mid_2 %>% head(10)
neg_10 <- df.neg_2 %>% head(10)


common_3sets <- intersect(intersect(df.pos_2$review, df.mid_2$review), df.neg_2$review)
common_PosNeg <- intersect(df.pos_2$review, df.neg_2$review)
head(common_PosNeg)

only_Pos <- df.pos_2 %>%
   filter(!review %in% common_PosNeg) %>%
   arrange(desc(Freq)) %>%
   head(30)

only_Neg <- df.neg_2 %>%
   filter(!review %in% common_PosNeg) %>%
   arrange(desc(Freq)) %>%
   head(30)

Pos_Neg <- data.frame (only_Pos30 = only_Pos$review, p.Freq = only_Pos$Freq,
                       only_Neg30 =  only_Neg$review, n.Freq = only_Neg$Fre)
Pos_Neg


a <- ggplot(Pos_Neg, aes(label = only_Pos30, size = p.Freq, color=p.Freq)) +
   geom_text_wordcloud_area(shape="diamond") +
   scale_size_area(max_size = 24) +
   theme(panel.background = element_rect(fill = 'grey3')) +
   scale_color_gradient(low = "yellow", high = "orange")

b <- ggplot(Pos_Neg, aes(label = only_Neg30, size = n.Freq, color=n.Freq)) +
   geom_text_wordcloud_area(shape="diamond") +
   scale_size_area(max_size = 24) +
   theme(panel.background = element_rect(fill = 'grey3')) +
   scale_color_gradient(low = "white", high = "blue")

c <- geom_text_wordcloud (data = Pos_Neg, aes(label = only_Neg30, size = n.Freq, color=n.Freq)) +
   geom_text_wordcloud_area(shape="diamond") +
   scale_size_area(max_size = 24) +
   theme(panel.background = element_rect(fill = 'grey3')) +
   scale_color_gradient(low = "white", high = "blue")
ggarrange (a,b)

wordcloud2(df.review.2, 
           size=2, col="random-light", 
           backgroundColor="black")




