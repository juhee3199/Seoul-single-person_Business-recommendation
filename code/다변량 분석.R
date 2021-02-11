getwd()

set.seed(100)


shop_norm=read.csv('shop_hum_norm.csv',header = T,encoding='utf-8',row.names ='X')

str(shop_norm)


#기초통계 구하기 
colMeans(shop_norm)
sapply(shop_norm,sd)
summary(shop_norm)

#산점도 그래프
plot(shop_norm)

#다중공선성까지 보여주는 산점도
library(MVA)
library(psych)
pairs.panels(shop_norm,main="variable scatterplot matrix")


#상관분석 
library(corrplot)
cor(shop_norm)
x<-cor(shop_norm)
corrplot(x)
#상관성 분석결과 같이 묶여있던 항목들 대대분이 강한관계를 가지고 있다.  
#0.1-0.3 이 약한 관계/ 0.4-0.7이면 강한 관계 


# ------------------------------------------------------------------------------------------------------------------------

#주성분분석 
m=colMeans(shop_norm)
S=cov(shop_norm)
R=cor(shop_norm)
eigen(S)
eigen(R)

#상관행렬을 위한 주성분 분석 /변수들의 측정단위가 다르고 분산값의 차이가 크게 나기 때문에 공분산행렬보다 상관행렬을 사용한다. 
p_cor=princomp(shop_norm,cor=T)
summary(p_cor)     #누적분산이 0.61인 주성분3까지 사용하겠다. 
attributes(p_cor)
p_cor$sdev
p_cor$loadings
p_cor$scores


#주성분 screeplot 
screeplot(p_cor,npcs = 12,type = "lines",main="principal component scree plot-correlation")
#주성분 2차원그래프
biplot(p_cor)   #각 변수들의 화살표 그러나 두번쨰 주성분까지와의 관계밖에 설명 못함 


par(mfrow=c(1,3))
barplot(p_cor$loadings[,1],col = rainbow(12),las = 2, main = "comp1")
abline(h = c(-0.3,0.3), col="blue")
barplot(p_cor$loadings[,2],col = rainbow(12),las = 2, main = "comp2")
abline(h = c(-0.3,0.3), col="blue")
barplot(p_cor$loadings[,3],col = rainbow(12),las = 2, main = "comp3")
abline(h = c(-0.3,0.3), col="blue")
# 위 그래프를 보고 1번째 주성분에서 어떤 변수가 유효한지 0.3 이상인것들이 유효한 변수이다. 




#주성분 3차원 그래프 
library(rgl)
p3<-princomp(shop_norm,scale=TRUE)
plot3d(p3$scores[,1:3])  # 주성분 점수의 3차원 그래프? 아닌듯 애매 
plot3d(p3$loadings[,1:3],main="3D plot",type="s",col=rainbow(12)) # 상관행렬을 이용한 3차원 주성분 그래프 단 화살표 없음 


#요인분석 
fact1=factanal(shop_norm,factors = 3,rotation = "none")
fact2=factanal(shop_norm,factors = 3,scores ="regression")
fact3=factanal(shop_norm,factors = 3,rotation = "promax")
fact1;fact2;fact3;
#인자분석시 여러가지 방법으로 인자분석 수행 후 출력결과 중 특히 인자 패턴을 비교하며 인자의 의미를 파악하도록 해야한다. 
#여러가지의 인자분석 결과 fact1는 상관행렬로 최대우도법을 이용하고 varimax 회전을하여 주성분분석을 한다. 
#귀무가설:인자의 개수는 3개이다. 에 대한 varimax 회전 인자분석의 카이제곱 검정 결과 p-=0.364>0.05=a 이므로 인자 3개가 적합하다. 


#요인분석결과 0.4 이상의 유효한 변수만 보이는 그래프 

par(mfrow=c(1,3))
barplot(fact2$loadings[,1],col = rainbow(12),las = 2, main = "factor1")
abline(h = c(-0.4,0.4), col="blue")
barplot(fact2$loadings[,2],col = rainbow(12),las = 2, main = "factor2")
abline(h = c(-0.4,0.4), col="blue")
barplot(fact2$loadings[,3],col = rainbow(12),las = 2, main = "factor3")
abline(h = c(-0.4,0.4), col="blue")




#3차원 인자분석 그래프 
library(rgl)
namevar=names(fact2$loadings)=c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12")
plot3d(fact2$scores[,1:3],main="verimaxed score 3D polt",type="p",col=4,size=4)  # verimax회전된 3차원 인자점수  그래프 
plot3d(fact2$loadings[,1:3],main="verimaxed pattern 3D polt",type="s",col=rainbow(12)) # verimax회전된 3차원 인자패턴 



# --------------------------------------------------------------------------------------------


#군집분석 
#계층적군집방법 -최단연결법 유클리드거리 이용 
library("cluster")
out<-hclust(dist(shop_norm),method="single") 
rev(out)
plot(out,hang=-1)
rect.hclust(out,k=4)
#위의 계층적 군집방법으로는 데이터를 제대로 설명하지 못한다. 그래서 k평균 방법을 사용했다., 
#덴드로그램이 군집을 제대로 못한다. 



#계층적 군집방법- KMEANS
#kmeans 결과 군집간 제곱합/ 총제곱합=77.4%로 적당하다. 여기서 구집간 제곱합이 너무 커지면 군집이 세분화되어 복잡하고 군집을 제대로 대표하지 못한다.  군집을 3개로 정하겠다. 
kout<-kmeans(shop_norm,4)
kout
shop_norm1<-shop_norm

shop_norm1$cluster<-kout$cluster
head(shop_norm1)
table(shop_norm1$cluster)  #각 군집 당 소속된 관측값들의 개수 표



# k-평균군집분석그래프 
par(mfrow=c(1,1))

shop_norm1$cluster2<-factor(shop_norm1$cluster,levels=c(1,2,3,4),labels=c("가군","나군","다군","라군"))
head(shop_norm1)
library("caret")
featurePlot(x=shop_norm1[,c(1:8)],y=shop_norm1$cluster2,plot="box",scales=list(x=list(rot=90),y=list(relation="free")),layout=c(6,1),auto.key=list(columns=2))
featurePlot(x=shop_norm1[,c(1:8)],y=shop_norm1$cluster2,plot="pairs",auto.key=list(columns=3))
featurePlot(x=shop_norm1[,c(1:8)],y=shop_norm1$cluster2,plot="density",scales=list(x=list(relation="free"),y=list(relation="free")),adjust=1.5,pch="|",layout=c(6,1),auto.key=list(columns=2)) 


#각 군집의 기술통계량 
kcluster1<-subset(shop_norm1,shop_norm1$cluster==1)
kcluster2<-subset(shop_norm1,shop_norm1$cluster==2)
kcluster3<-subset(shop_norm1,shop_norm1$cluster==3)
kcluster4<-subset(shop_norm1,shop_norm1$cluster==4)



#그래프1
require(reshape2)

require(plyr)

require(ggplot2)

require(scales)




coord_radar <- function (theta = "x", start = 0, direction = 1) 
  
{
  
  theta <- match.arg(theta, c("x", "y"))
  
  r <- if (theta == "x") 
    
    "y"
  
  else "x"
  
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          
          direction = sign(direction),
          
          is_linear = function(coord) TRUE)
  
}




rescale_df=function(data,groupvar=NULL){
  
  if(is.null(groupvar)) df=data
  
  else df=data[,-which(names(data) %in% groupvar)]
  
  
  
  select=sapply(df,is.numeric)
  
  df[select]=lapply(df[select], scales::rescale)
  
  if(!is.null(groupvar)) {
    
    df=cbind(df,data[[groupvar]])
    
    colnames(df)[length(df)]=groupvar
    
  }        
  
  df
  
}







ggRadar=function(data=shop_norm1,
                 
                 xvars=NULL,
                 
                 yvar=NULL,autorescale=FALSE,
                 
                 groupvar=NULL,legend.position="bottom",
                 
                 radar=TRUE,polar=FALSE,
                 
                 mean=TRUE,nrow=FALSE,
                 
                 colour="red"){
  
  if(is.null(xvars)) {
    
    select=sapply(data,is.numeric)
    
    xvars=colnames(data)[select]
    
  }
  
  if(is.null(yvar)){
    
    # if(!is.null(groupvar)) {
    
    #         for(i in 1:length(groupvar)) data[[groupvar[i]]]=factor(data[[groupvar[i]]])
    
    # }
    
    # data
    
    if(autorescale) data=rescale_df(data,groupvar)
    
    longdf=melt(data,id.vars=groupvar,measure.vars=xvars)
    
    longdf
    
    if(mean)
      
      df=ddply(longdf,c(groupvar,"variable"),summarize,mean(value,na.rm=TRUE))
    
    if(nrow) 
      
      df=ddply(longdf,c(groupvar,"variable"),"nrow") 
    
    
    
    colnames(df)[length(df)]="value"
    
    #print(df)
    
  } else{
    
    longdf=data
    
  }
  
  
  
  if(is.null(groupvar)){
    
    p<-ggplot(data=df,aes_string(x="variable",y="value",group=1))+
      
      geom_point(size=3,colour=colour)+
      
      geom_polygon(colour=colour,fill=colour,alpha=0.4)
    
    
    
  } else {
    
    df=df[!(df$variable %in% groupvar),]
    
    for(i in 1:length(groupvar)) df[[groupvar[i]]]=factor(df[[groupvar[i]]])
    
    p<-ggplot(data=df,aes_string(x="variable",y="value",
                                 
                                 colour=groupvar,fill=groupvar,group=groupvar))+
      
      geom_point(size=3)+
      
      geom_polygon(alpha=0.4)
    
  }        
  
  p<- p+ xlab("")+ylab("")+theme(legend.position=legend.position)
  
  
  
  if(radar==TRUE) p<-p+coord_radar()
  
  if(polar==TRUE) p<-p+coord_polar()    
  
  p    
  
}

ggRadar(data=shop_norm1,groupvar="cluster",autorescale = TRUE)



