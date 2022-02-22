#Essa análise utiliza somente a variável H5 Google e exclui a mediana do Google

#Gráfico 01: NOTA WEB QUALIS CAPES (A1 a C)

#Packages utilizados
library(magrittr)
library(dplyr)
library(ggplot2)

#lendo a base
library(readxl)
BASEH5 <- read_excel("BASEH5.xlsx")
View(BASEH5)
#BASEH5$H5.Mediana.Google <- NULL

#sumarizando 

objeto1 <- BASEH5%>%
  select(ISSN, Qualis)%>%
  group_by(ISSN, Qualis)%>%
  summarise(n = sum(n()))

BASEH5 <- BASEH5%>%
  mutate(QualisRec = recode_factor(Qualis, 
                                   `1` = "A1", 
                                   `2` = "A2", 
                                   `3` = "B1", 
                                   `4` = "B2", 
                                   `5` = "B3", 
                                   `6` = "B4", 
                                   `7` = "B5",
                                   `8` = "C"))

objeto2 <- BASEH5%>%
  select(ISSN, QualisRec)%>%
  group_by(QualisRec)%>%
  summarise(n = sum(n()))

#verificar pq as linhas 2018,2019,2020 e 2021 possuem o ISSN repetidos duas vezes, assim reduz meus casos para 2021

ggplot(objeto2, aes(y = n, x = QualisRec)) +
  geom_bar(stat = "identity", 
           color="black", fill="grey") +
  scale_y_continuous(limits = c(0,600)) +
  theme_bw(base_size = 14)+
  xlab(NULL) + 
  ylab("Frequência")+
  geom_text(
    aes(label = n, y = n + 0.05),
    position = position_dodge(0.9),
    vjust = 0)

#GRAFICO 2 DISTRIBUIÇÃO COM CURVA NORMAL DO ÍNDICE H5 - com correção Log10
n = 1200
mean = 0.87
sd = 0.36
binwidth = 0.3
set.seed(1)
df <- data.frame(x = rnorm(n, mean, sd))


objeto3 <- BASEH5%>%
  na.omit()%>%
  select(H5.Google)

objeto3%>%na.omit()%>%
  ggplot(aes(log(H5.Google))) + 
  theme_bw(base_size = 14)+
  geom_histogram(color="black", fill="grey")+
  ylab("Frequência")+
  annotate("text", x = 4, y = 100, label = "Média = 0.87
           Desvio = 0.36
           N = 886")

objeto3.3 <- BASEH5%>%
  na.omit()%>%
  select(H5.Mediana.Google)

objeto3.3%>%na.omit()%>%
  ggplot(aes(log(H5.Mediana.Google))) + 
  theme_bw(base_size = 14)+
  geom_histogram(color="black", fill="grey")+
  ylab("Frequência")


#GRAFICO 3 QUALIS SUPERIOR E INFERIOR DE ACORDO COM GRUPOS DO INDICE H5

#Recodificando a Variavel H5 para  os grupos

objeto4 <- BASEH5%>%select(H5.Google)
objeto4<- objeto4[!is.na(objeto4)]
max(objeto4)
min(objeto4)


objeto5 <- BASEH5%>%
  select(Superior.Inferior, H5.Google)%>%
  na.omit()

objeto6 <- BASEH5%>%
  select(H5.Google)%>%
  na.omit()

#transformando a coluna em vetor para ver se funciona
avector <- as.vector(objeto6['H5.Google'])
class(avector) 

avector <- objeto6[['H5.Google']]
class(avector)

h5categorias <- cut(avector, breaks = c(0, 5, 10, Inf), 
                    labels = c("Baixo","Medio","Alto"),
                    right = FALSE)

#Certo, agora preciso retornar isso para o objeto 5

objeto7 <- data.frame(objeto5, h5categorias)

objeto7 <- objeto7%>%
  mutate(Rec.S.I = recode_factor(Superior.Inferior , 
                                 `1` = "Superior", 
                                 `2` = "Inferior"))

objeto8 <- objeto7%>%
  select(Rec.S.I, h5categorias)%>%
  group_by(Rec.S.I, h5categorias)%>%
  summarise(n = sum(n()))

ggplot(objeto8, 
       aes(y = n, x = Rec.S.I, fill = h5categorias)) +
  geom_bar(stat = "identity")

ggplot(objeto8,aes(x = Rec.S.I,y = n)) +    
  geom_bar(aes(fill = h5categorias),position = "dodge", stat = "identity", color="black")+
  scale_y_continuous(limits=c(0, 300))+
  theme_bw(base_size = 14)+
  ylab("Contagem")+ xlab(NULL)+
  scale_fill_manual("Grupos H5", 
                    values = c("Baixo" = "gray73", "Medio" = "grey55", "Alto" = "grey27"))

#GRAFICO 4 DISTRIBUIÇÃO DE CITAÇÕES DO INDICE H5 POR QUALIS

BASEH5%>%na.omit()%>%
  ggplot(aes(y = H5.Google, x = QualisRec))+
  geom_boxplot(fill = "grey55", outlier.colour="red", outlier.shape=1, outlier.size=2)+
  scale_y_continuous(limits=c(0, 400))+
  theme_bw(base_size = 14)+
  ylab("H5 Google")+ xlab(NULL)


#existe um cado no boxplot A1 que pode acabar com meu modelo, preciso removê-lo,

BASEH5%>%na.omit()%>%
  ggplot(aes(y = H5.Google, x = QualisRec, label = Revista))+
  geom_boxplot(fill = "grey55", outlier.colour="red", outlier.shape=1, outlier.size=2)+
  geom_text(check_overlap = TRUE,
            position=position_jitter(width=0.14), size = 2)+
  scale_y_continuous(limits=c(0, 400))+
  theme_bw(base_size = 14)+
  ylab("H5 Google")+ xlab(NULL)

#assim, descobri que as revistas NATURES (LONDON), PLOS ONE E JOURNAL OF CLEANER PRODUCTION
#devem ser retiradas da minhas amostra

BASEH5$Revista[BASEH5$Revista == "NATURE (LONDON)"] <- NA

#CALCULANDO CLUSTERS
install.packages("factoextra")
library(factoextra)

#Balanceamento da base

objeto9<-BASEH5%>%
  na.omit()%>%
  select(H5.Google)

objeto9 <- scale(objeto9)
head(objeto9, n = 3)

#Número ótimo de clusters

#kmeans(objeto9, 4, iter.max=10, nstart=1) #nesse caso eu determinei o numero ótimo

#mas para saber o número ótimo, vamos a essa função:
options(scipen=999)

fviz_nbclust(objeto9, kmeans, method = "wss")

fviz_nbclust(objeto9, kmeans, method = "gap_stat")
#Desta forma, utilizando a noção da soma dos quadrados intra cluster é possível verificar que
#o número ótimo de clusters para a amostra é 4.

set.seed(123)
km.res = kmeans(objeto9, 4, nstart=25)
print(km.res)

#em qualquer cenário que estou construindo, somente um revista está ocupando todo o cenário
#buscar que caso é esse para retirar da construção do modelo.

aggregate(objeto9, by=list(cluster = km.res$cluster), mean)

objeto10 <- cbind(objeto9, cluster = km.res$cluster)

head(objeto10)



#GRAFICO 5 GRUPAMENTO POR CLUSTERS

fviz_cluster(
  km.res,
  data = objeto10,
  geom = "point",
  pointsize = 1.5)

ggplot()+
  geom_point(data = objeto10, 
             mapping = aes(x = log(H5.Google), 
                           y = log(H5.Google)))+
               facet_grid(rows = vars(cluster))

             