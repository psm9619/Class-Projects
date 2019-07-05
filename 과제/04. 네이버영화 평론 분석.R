library(rJava)
library(KoNLP)
library(wordcloud)
useSejongDic()
#mergeUserDic(data.frame("")) -> 내가 필요로 하는 단어가 사전에 없을 시 추가하는 것 (ex. 지명)
library(RColorBrewer)

data1 <- read.csv("navermoview.csv", stringsAsFactors = F)
data_review <- data1$review

review <- sapply(data_review, extractNoun, USE.NAMES = F)

# review 프로세싱에서 다음과 같은 에러가 발생하여 확인하자, spacing 이 되지 않은 경우였다.
 "java.lang.ArrayIndexOutOfBoundsException
   Warning message:
   In value[[3L]](cond) :
   can't processing '빡침진짜3에서완벽하게끝내놓고보니는결국우디를버렸고그걸포장하기위해우디가자기삶을찾았다고한듯난마지막에보니가우디를다시사랑할줄알았는데보니도ㅗ쉽게헤어지는버즈랑우디도그렇고간다니까보내준친구들도이해안됨차라리보지말걸내마음속에있는아름다운엔딩이거지같아짐돈벌라고한편더만든느낌쉣 "
# 전처리를 위해 직접 spacing 을 하여 reprocess 한다.

data_review [4414] <- "빡침 진짜 3에서 완벽하게 끝내놓고 보니는 결국 우디를 버렸고
그걸 포장하기 위해 우디가 자기 삶을 찾았다고 한 듯 난 마지막에 보니가 우디를 다시 사랑할 줄 알았는데
보니도 ㅗ 쉽게 헤어지는 버즈랑 우디도 그렇고 간다니까 보내준 친구들도 이해 안 됨 
차라리 보지 말 걸 내 마음속에 있는 아름다운 엔딩이 거지 같아짐 돈 벌라고 한 편 더 만든 느낌 쉣 "

review_2 <- sapply(data_review, extractNoun, USE.NAMES = F)
# 더 이상 프로세스 에러가 발생하지 않음을 확인할 수 있다.
head(review_2) 
# 아마도 spacing 문제로 인해 noun extraction 이 성공적으로 이루어지지 않은 경우가 몇 가지 눈에 띈다. (ex. 아시아최초로, 같은거왜보냐정신연령이그정도 등 )
# 그러나 전체적으로는 양호한 면을 보이므로 전처리 중에 해결하도록 한다. 

review_2 <- unlist(review_2)
review <- str_replace_all(review_2, "[^:alpha:]]","")

# 전처리를 위해 데이터를 훑는 방법으로 전체 데이터 length 에서 50 region index 를 random generator 을 이용해 뽑았다.
set.seed(123)
index <- sample(1:ceiling(length(review_2)), 100, replace=FALSE)
index  
# 각 인덱스로부터 30씩 데이터를 뽑아서 새로운 list 에 저장한다. 
test <- c()
for (i in index) {
  test <- c(test, review[i:(i+29)])
} 
# 총 3000 개의 데이터가 있으므로 300씩 끊어 검사하며 전처리에 사용할 데이터를 t_gsub list 로 저장한다.













