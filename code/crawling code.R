Sys.setenv(JAVA_HOME='c:/Program Files/Java/jre1.8.0_221')

getwd()


#setwd("C:/Users/leejuhee/Desktop/R_pro")

install.packages('KoNLP')
library(data.table)
library(wordcloud)
library(KoNLP)
library(RColorBrewer)
useSejongDic()


text <- readLines("blog_lucky_final_1.txt", encoding="UTF-8")
head(text)
buildDictionary(ext_dic = "woorimalsam")
pal2 <- brewer.pal(8, "Dark2")
blog <- sapply(text, extractNoun, USE.NAMES=F)
head(blog)
blog2 <- unlist(blog)


wordcount <- table(blog2) # 단어의 빈도수 계산
temp <- sort(wordcount, decreasing = T)[1:30] # 1-10위 내림차순 정렬
temp
barplot(temp,
        names.arg = names(temp),
        col = "lightblue",
        main = "빈도수 높은 단어", ylab = "단어 빈도수")

# Gsub 작업
length(blog2)  #28804
blog2 <- blog2[nchar(blog2)>1]
length(blog2)  # 21803

head(blog2,30)
blog2<-gsub("서울","",blog2)
blog2<-gsub("1인가구","",blog2)
blog2<-gsub("1인","",blog2)
blog2<-gsub("가구","",blog2)
blog2<-gsub("[A-Z]+","",blog2)#영어
blog2<-gsub("[a-z]+","",blog2)

blog2<-gsub("ㅠㅠ" ,"",blog2)
blog2<-gsub("ᆞ" ,"",blog2)
blog2<-gsub("\\." ,"",blog2)
blog2<-gsub(" ","",blog2)#공백문자
blog2<-gsub("\\d+","",blog2) #숫자
blog2<-gsub("[:punct:]+","",blog2)#특수문자

head(blog2,50)
sort(table(blog2),decreasing=T)[1:30]

write(blog2,"blog_crawling-2.txt")
blog3<-read.table("blog_crawling-2.txt")

length(unlist(blog3))   #8000
sort(table(blog3),decreasing=T)[1:30]

wordcount <- table(blog3) 
wordcount

plot.new()

palete <- brewer.pal(9,"Set3") 
wordcloud(names(wordcount),
          freq=wordcount,
          scale=c(4,0.5),
          max.words = 200,
          random.order=F,
          rot.per=0.3,
          random.color = T,
          colors=palete )


# blog4에 gsub파일 만들어서 삭제 후 저장.
blog4<-unlist(blog3)
gsubtxt<-readLines("gsub_blog.txt",encoding = 'UTF-8')


head(gsubtxt)
length(gsubtxt)
len<-length(gsubtxt)
for( i in 1:len) {
  blog4 <- gsub((gsubtxt[i]),'',blog4)     
}
head(blog4,50)
length(blog4)


write(blog4,"blog_crawling_remake.txt")
blog5<-read.table("blog_crawling_remake.txt")

sort(table(blog5),decreasing = T)[1:30]
length(unlist(blog5)) #13000

wordcount<-table(blog5) 
palete <- brewer.pal(9,"Set3") 
plot.new()
wordcloud(names(wordcount),freq=wordcount
          ,scale=c(4,0.5),rot.per=0.25,min.freq=2,
          random.order=F,random.color=T,colors=palete)
legend(0.3,1 ,"1인가구 관련 키워드",cex=0.8,fill=NA,border=NA,bg="white" ,text.col="red",text.font=2,box.col="red")
