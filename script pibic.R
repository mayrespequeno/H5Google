library(dplyr)
library(haven)
library(factoextra)
library(tidyr)
library(ggplot2)

Sem_outliers <- read_sav("Sem outliers.sav")
  a<-Sem_outliers

Summary(a)
  
a$H5.Google<-as.numeric(a$H5.Google)
class(a$H5.Google)

a<-a %>% mutate(QualisRec=recode(Qualis, 
                              `1`="A1",
                              `2`="A2",
                              `3`="B1",
                              `4`="B2",
                              `5`="B3",
                              `6`="B4",
                              `7`="B5",
                              `8`="C"
))


a%>%group_by(Qualis)%>%
  summarise(H5.Google=mean(H5.Google, na.rm = T))%>%
  ggplot(aes(x= reorder(Qualis,-H5.Google),y=H5.Google))+
  geom_col(fill = "white", color = "black")+
  geom_text(aes(label=sprintf("%0.2f", H5.Google), vjust= -.5))+
  theme_minimal()+
  labs(x = "Qualis", y="Score H5 Google")+
  theme(panel.grid = element_blank())

g2<-a%>%group_by(Qualis)%>%summarise(media=mean(H5.Google, na.rm = T),
                                 dp = sd(H5.Google, na.rm = T))

g2%>%ggplot(aes(x= reorder(Qualis,-media),y=media))+
  geom_point()+
  geom_errorbar( aes(x=Qualis, ymin=media-dp, ymax=media+dp))+
  theme_minimal()+
  labs(x = "Qualis", y="Score H5 Google")+
  theme(panel.grid = element_blank())
  
a%>%ggplot(aes(x= reorder(Qualis,-H5.Google),y=H5.Google))+
  geom_boxplot()+
  theme_minimal()+
  labs(x = "Qualis", y="Score H5 Google")+
  theme(panel.grid = element_blank())

b<-a%>%na.omit()


c<-b%>%select(H5.Google, H5.Mediana.Google)

#amabas variaveis com 1189 casos


fviz_nbclust(c, kmeans, method = "wss")
fviz_nbclust(c, kmeans, method = "silhouette")
fviz_nbclust(c, kmeans, method = "gap_stat")

clst<-kmeans(x = c, 4, nstart = 25)

b$cluster<-clst$cluster

fviz_cluster(clst, df=c)

# 3. Visualize


b<-b %>% mutate(cluster=recode(cluster, 
                                 `1`="Cluster 1",
                                 `2`="Cluster 2",
                                 `3`="Cluster 3",
                                 `4`="Cluster 4"))


b%>%
ggplot(aes(x = H5.Google, y= H5.Mediana.Google, 
           label = QualisRec, color = factor(cluster)))+
  geom_text()+
  theme_bw(base_size = 14)+
  scale_y_continuous(limits = c(0,100))+
  scale_x_continuous(limits = c(0,75))+
  scale_fill_manual(guides("Agrupamentos"))+
  labs(x = "H5 Google", y="Mediana H5 Google")+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")
  
  

##############

b%>%group_by(Qualis)%>%select(cluster)
base<-b%>%select(Qualis, cluster, H5.Google, LOG.H5.Google, H5.Mediana.Google)


base
library(tidyr)


t<-base%>%gather(oriundo,qualis_cluster,-H5.Google,-LOG.H5.Google,-H5.Mediana.Google)

modelo1<-lm(H5.Google~qualis_cluster, data = t)
summary(modelo1)
anova(modelo1)

anova1<-aov(H5.Google~qualis_cluster, data = t)
anova1
TukeyHSD(anova1)

medias <- with(t,tapply(H5.Google,qualis_cluster,mean)) 
erro <- with(t,tapply(H5.Google,qualis_cluster,function(x) sqrt(var(x)/length(x))))


x <- barplot(medias,beside=T,ylab="H5",xlab="Qualis ou Cluster")
arrows(x0=x,y0=medias-erro,
       x1=x,y1=medias+erro,
       angle=90,length=0.14,code=3)

df<-data.frame(medias,erro)

df%>%ggplot(aes(x = reorder(rownames(df), -medias), y =medias))+
  geom_point()+
  geom_errorbar(aes(ymin=medias-erro, ymax=medias+erro))+
  theme_minimal()+
  scale_fill_manual(guides("Agrupamentos"))+
  labs(x = "Qualis ou Cluster", y="Media")+
  theme(panel.grid = element_blank(),
        legend.position = "bottom")


b2<-b%>%filter(cluster ==3); table(b2$Qualis)
  