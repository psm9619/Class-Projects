library(RSelenium)
library(rvest)
library(stringr)


remDr <- remoteDriver(remoteServerAdd = "localhost", port=4445L, browserName ="chrome")
remDr$open()

remDr$navigate("https://nid.naver.com/nidlogin.login")

txt_id <- remDr$findElement(using="id", value="id")
txt_pw <- remDr$findElement(using="id", value="pw")
login_btn <- remDr$findElement(using="class", value="btn_global")

txt_id$setElementAttribute("value", "psm9619") # 아이디 입력
txt_pw$setElementAttribute("value", "372194110SmP*") # *에 비밀번호 입력
login_btn$clickElement()

remDr$navigate("https://mail.naver.com/")
mail_texts <- remDr$findElement(using="id", value="list_for_view")

mail_texts <- mail_texts$getElementText()
mail_texts
tmp <- str_split(mail_texts, '\n') %>% .[[1]]

sender <- c()
subject <- c()
time <- c()
for (i in 1:20) {
  sender <- c(sender, tmp[4*i-3])
  subject <- c(subject, tmp[4*i-2])
  time <- c(time, tmp[4*i-1])
}
df_mail <- data.frame(sender=sender, subject=subject, time=time)
df_mail


# --------------------------- 
remDr$close()









