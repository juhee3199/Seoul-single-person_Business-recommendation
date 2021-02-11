getwd()
setwd("C:/Users/leejuhee/Desktop/R_pro")

store<-read.csv('store.csv',header=T)
head(store)

dong_st<-subset(store,store$행정동명=='행운동',select=c('상호명','지점명','상권업종중분류명','경도','위도'))


# 선정된 상위 3개업종  
# 종합소매점,부동산,병원
st_retail<-subset(dong_st,dong_st$상권업종중분류명=='종합소매점',select=c('상호명','경도','위도'))
head(st_retail,30)
nrow(st_retail)

st_es<-subset(dong_st,dong_st$상권업종중분류명=='부동산중개',select=c('상호명','경도','위도'))
head(st_es)
nrow(st_es)

st_hosp<-subset(dong_st,dong_st$상권업종중분류명=='병원',select=c('상호명','경도','위도'))
head(st_hosp)
nrow(st_hosp)





library(ggmap)
register_google(key='AIzaSyAsdDvFfwk6aNgQjIDGgMFLUKDxkBSVehE')

#종합소매점
df1<-data.frame(name=st_retail$상호명
               ,lon=st_retail$경도
               ,lat=st_retail$위도)
head(df1)
gc<-data.frame(lon=df1$lon,lat=df1$lat)
cen<-c(mean(df1$lon),mean(df1$lat))
map<-get_googlemap(center=cen,
                   maptype= 'roadmap',
                   zoom=16,
                   size=c(640,640),
                   marker=gc)
ggmap(map)


#부동산
df2<-data.frame(name=st_es$상호명
                ,lon=st_es$경도
                ,lat=st_es$위도)
head(df2)
gc<-data.frame(lon=df2$lon,lat=df2$lat)
cen<-c(mean(df2$lon),mean(df2$lat))
map<-get_googlemap(center=cen,
                   maptype= 'roadmap',
                   zoom=16,
                   size=c(640,640),
                   marker=gc)
ggmap(map)


#병원.
df<-data.frame(name=st_hosp$상호명
               ,lon=st_hosp$경도
               ,lat=st_hosp$위도)
head(df)
gc<-data.frame(lon=df$lon,lat=df$lat)
cen<-c(mean(df$lon),mean(df$lat))
map<-get_googlemap(center=cen,
                   maptype= 'roadmap',
                   zoom=16,
                   size=c(500,500),
                   marker=gc)
ggmap(map)
