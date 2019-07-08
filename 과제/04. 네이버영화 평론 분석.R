library(rJava)
library(dplyr)
library(KoNLP) 
library(ggwordcloud)
library(wordcloud2)
library(tm)
library(stringr)
library(ggplot2)
useSejongDic()
#mergeUserDic(data.frame("")) -> 내가 필요로 하는 단어가 사전에 없을 시 추가하는 것 (ex. 지명)
library(RColorBrewer)

data1 <- read.csv("navermovie_toystory.csv", stringsAsFactors = F)
data_review <- data1$review
#View(data_review)
review <- sapply(data_review, extractNoun, USE.NAMES = F)

# review 프로세싱에서 다음과 같은 에러가 발생하여 확인하자, spacing 이 되지 않은 경우였다.
 "java.lang.ArrayIndexOutOfBoundsException
   Warning message:
   In value[[3L]](cond) :
   can't processing '빡침진짜3에서완벽하게끝내놓고보니는결국우디를버렸고그걸포장하기위해우디가자기삶을찾았다고한듯난마지막에보니가우디를다시사랑할줄알았는데보니도ㅗ쉽게헤어지는버즈랑우디도그렇고간다니까보내준친구들도이해안됨차라리보지말걸내마음속에있는아름다운엔딩이거지같아짐돈벌라고한편더만든느낌쉣 "
# 전처리를 위해 직접 spacing 을 하여 extractNoun 을 재실행 한다.
data_review[4747] <- "빡침 진짜 3에서 완벽하게 끝내놓고 보니는 결국 우디를 버렸고
그걸 포장하기 위해 우디가 자기 삶을 찾았다고 한 듯 난 마지막에 보니가 우디를 다시 사랑할 줄 알았는데
보니도 ㅗ 쉽게 헤어지는 버즈랑 우디도 그렇고 간다니까 보내준 친구들도 이해 안 됨 
차라리 보지 말 걸 내 마음속에 있는 아름다운 엔딩이 거지 같아짐 돈 벌라고 한 편 더 만든 느낌 쉣 "

review_2 <- sapply(data_review, extractNoun, USE.NAMES = F)
head(review_2) 
# 더 이상 processing 에러가 뜨지 않음을 확인할 수 있다.
# 아마도 spacing 문제로 인해 noun extraction 이 성공적으로 이루어지지 않은 경우가 몇 가지 눈에 띈다. (ex. 아시아최초로, 같은거왜보냐정신연령이그정도 등 )
# 그러나 전체적으로는 양호한 면을 보이므로 전처리 중에 해결하도록 한다. 

review_2 <- unlist(review_2)

length(table(review_2))  # 현재 단어의 총 수는 8829  


# 전처리를 위해 데이터를 훑는 방법으로 전체 데이터 length 에서 50 region index 를 random generator 을 이용해 뽑았다.
set.seed(123)
index <- sample(1:length(review_2), 50, replace=FALSE)
index  
# 각 인덱스로부터 30씩 데이터를 뽑아서 새로운 list 에 저장한다. 
test <- c()
for (i in index) {
  test <- c(test, review[i:(i+29)])
} 
# 총 3000 개의 데이터가 있으므로 300씩 끊어 검사하며 전처리에 사용할 데이터를 t_gsub list 로 저장한다.

# test[1:300] ; test[301:600] ; test[601:900] ; test[901:1200] ; test[1201:1500]
t_gsub <- c("\\W", "\\d+",'[ㄱ-ㅎ]',"(ㅜ|ㅠ)","[[:punct:][:lower:][:upper:]]", 
             "였\\S*", "줄\\S*", "까지\\S*", "합니\\S*", 
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
# review_2[1:300] # sub txt 를 이용한 전처리가 잘 되었는지 체크한다. 성공적으로 이루어진 것으로 파악된다
# 추가적으로 위에서 검토 중에 눈에 띄던 '관람객' + '..' 와 같이 주된 단어 뒤에 내용이 추가될시 제대로 파악되지 않는 경우를 해결하기 위해 약간의 전처리를 더 해준다.
# 이 떄 데이터의 선택은 count 빈도수가 <= 2 인 경우로 잡는다. 
t <- table(review_replaced)
length(t)  # 현재 단어의 총 수는 7678 . 위의 전처리 후 대략 1000 이 감소. 
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
length(table(review_replaced_2))  # 현재 단어 수는 6633 으로 다시 1000 정도 감소한다.

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
review_replaced_3 <-gsub ("말\\S*","말이필요없음", review_replaced_3)
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
length(table(review_replaced_3)) # 6019로 600정도 줄었다. 예상보다 확연한 감소는 아닌만큼 시간대비 효율적이지 않은 전처리였으나 시도에 의의를 두기로 한다.

# 이제 띄어쓰기 등의 문제로 extract noun 이 성공적으로 되지 못한 데이터 (nchar <= 1 | nchar >=10) 을 제거한다.
review_final <- Filter(function(x) {nchar(x) >= 2 & nchar(x) <= 10}, review_replaced_3)

table.review <- table(review_final) 
str(table.review)

df.review <- as.data.frame(table.review)  # using just df.review took too long time for loading
df.review.2 <-   df.review %>%
  arrange(desc(Freq))  %>%
  head(300)

wordcloud2(as.table(df.review.2), 
           size=2, col="random-light", 
           backgroundColor="black")


str(df.review)
ggplot (df.review, aes(label = review_final, size = Freq)) +
  geom_text_wordcloud_area(rm_outside=TRUE) +
  scale_size_area(max_size = 20) +
  theme_minimal()
figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")

wordcloud2(demoFreq, minRotation = -pi/6, maxRotation = -pi/6, minSize = 10,
           rotateRatio = 1)







