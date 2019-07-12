install.packages("jsonlite")
library(jsonlite)
pi
json_pi <- toJSON (pi, digits=3)
fromJSON(json_pi)

city <- '대전'
json_city <- toJSON(city)
fromJSON(json_city)

# [
#   {
#     "NAME" : "TEST",
#     "Age" : 25,
#     "SEX" : "F",
#     "Address" : "Seoul" ,
#     "Hobby" : "Basketball"
#   }
# ]

name <- c("TesT")
age <- c(25)
sex <- c("F")
address <- c("Seoul")
hobby <- c("Basketball")

person <- data.frame (name,age, sex, address, hobby)
names(person)
str(person)
person

json_person <- toJSON(person)
json_person
prettify(json_person)

df_json_person <- fromJSON(json_person)

# all(df1 == df2) -> 두 데이터프레임의 데이터 값이 같은지 비교
all (person==df_json_person)

cars
json_cars <- toJSON(cars)
json_cars
df_json_cars <- fromJSON(json_cars)
df_json_cars



































