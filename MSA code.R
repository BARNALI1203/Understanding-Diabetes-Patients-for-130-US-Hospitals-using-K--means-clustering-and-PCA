dat1<-read.csv(file.choose())
dat1
library(dplyr)
library(ggplot2)
library(factoextra)
library(purrr)
diabetes3<-dat1 %>% 
  mutate(age=recode(age,"[0-50)"=1, "[50-60)" = 2, "[60-70)" = 3, "[70-80)" = 4, "[80-100)" = 5)) %>% 
  mutate(max_glu_serum=recode(max_glu_serum,"None"=1,"Norm"=2,">200"=3,">300"=4)) %>% 
  mutate(A1Cresult=recode(A1Cresult,"None"=1,"Norm"=2,">7"=3,">8"=4))%>%
  mutate(admission_type_id=recode(admission_type_id,"Emergency"=1,"Other type"=2))%>%
  mutate(discharge_disposition_id=recode(discharge_disposition_id,"Home"=1,"Other discharge"=2))%>%
  mutate(admission_source_id=recode(admission_source_id,"Emergency Room"=1,"Other source"=2,"Referral"=3))%>%
  mutate(insulin=recode(insulin,"Down"=1,"No"=2,"Steady"=3,"Up"=4))%>%
  mutate(change=recode(change,"Ch"=1,"No"=2))%>%
  mutate(diabetesMed=recode(diabetesMed,"Yes"=1,"No"=2))%>%
  mutate(gender=recode(gender,"Female"=1,"Male"=2))%>%
  mutate(race=recode(race,"AfricanAmerican"=1,"Asian"=2,"Caucasian"=3,"Hispanic"=4,"Other"=5))
diabetes3

set.seed(123)
#standardizing the dataset
standardize=function(x){
  z<-(x-mean(x))/sd(x)
  return(z)
  
}
diabetes3[2:22]<-apply(diabetes3[2:22],2,standardize)


#Finding the optimal number of clusters
tot_withiness<-map_dbl(1:10,function(k){
  model<-kmeans(x=diabetes3,centers = k)
  model$tot.withinss
})

elbow_df<-data.frame(
  k=1:10,
  tot_withiness=tot_withiness
)
ggplot(elbow_df,aes(x=k,y=tot_withiness))+geom_line()+geom_point()+scale_x_continuous(breaks = 1:10)

#K means cluster 
k2<-kmeans(diabetes3[2:22],centers=3)
k2
str(k2)
summary(k2)
fviz_cluster(k2,data=diabetes3[2:22])


#ANOVA for K means
m=cbind(diabetes3[2:22],k2$cluster)
m
temp1=c()
for (i in (1:21)){
  a1<-aov(m[,i]~m[,22])
  temp1[i]<-summary(a1)[[1]][1,5]
  
}
temp1

#Performing PCA
library(factoextra)
res.pca<- prcomp(diabetes3[2:22],center = TRUE, scale = FALSE)
fviz_eig(res.pca)
pca11=res.pca$x[,1:5]
summary(res.pca)
pcadata<-res.pca$rotation
components<-pcadata[,1:5]
as.data.frame(components)

#Optimal number of clusters
tot_withiness<-map_dbl(1:10,function(k){
  model<-kmeans(x=components,centers = k)
  model$tot.withinss
})

elbow_df<-data.frame(
  k=1:10,
  tot_withiness=tot_withiness
)
ggplot(elbow_df,aes(x=k,y=tot_withiness))+geom_line()+geom_point()+scale_x_continuous(breaks = 1:10)

#K means Clustering after PCA
k3<-kmeans(pca11,centers=6,nstart = 100, iter.max = 1000)
k3
str(k3)
fviz_cluster(k3,data=pca11)

#ANOVA for PCA
d=cbind(pca11,k3$cluster)
d

temp=c()
for (i in (1:5)){
  a<-aov(d[,i]~d[,6])
  temp[i]<-summary(a)[[1]][1,5]
  
}
temp

