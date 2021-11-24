library(questionr) #rename
library(ggplot2) #Graph
library(stats) #Tests
library(dplyr) #Manipulations
library(tidyr) #Dropna
library(moments)
library(broom)
library(car)
library(ggpubr)
library(readxl)
library(xtable)


##############################
#Ouverture
getwd()
df = read_xlsx("Fichiers/Hungary/pro.xlsx")
df=read.csv("Fichiers/Slovenia/Sl_Pop_Cities_2011_Rank.csv")

#Couverture de la population
sum(df$Pl_Pc_2011)
#Nettoyage
df<-subset(df, select = -c(Level))
df<-df[-19,]
df<-df %>% mutate("Cz_Pr_2011" = M+F) 

#Sélectionner les lignes ok
df<-df[(df$Année)== "2015",]
df<-df[(df$Unité)== "Population résidante permanente - Total",]

df %>% select(Lu_provinces) %>% filter(Lu_provinces %in% c("Dippach"))
grep("Canton", df$Lu_provinces, ignore.case = TRUE)
df<-df %>% select(Cy_regions,Cy_Codreg,Cy_Pr_2011,SEX) %>% filter(SEX =="Total")

#Supprimez ligne en fonction du code
df<-df[substr(df$level,1,10) != "region", ]


df<- rename.variable(df,"Name","Hu_provinces")
df<- rename.variable(df,"VALEUR","Lt_Pr_2011")
df<- rename.variable(df,"...4","F")

#Changer l'ordre des colonnes
df<-df[,c(2,1)]
#Grouper par numéros de communes et sommer
df<-df %>% group_by(Level,Name) %>% mutate("Somme") %>% summarise(Somme=sum(Somme))


#Remove O et Na
df$Uk_Pc_2011[df$Uk_Pc_2011==0] <- NA
df$F<- as.numeric(as.character(df$F))
df<-drop_na(df)
str(df)


#Ln et standardization
df$Hu_LnPp_2011=log(df$Hu_Pp_2011)
mean(df$Hu_LnPp_2011)

df$Hu_LnPp_2011_Stdz=scale(df$Hu_LnPp_2011)
mean(df$Hu_LnPp_2011_Stdz)
sd(df$Hu_LnPp_2011_Stdz)




#Répartition
ggplot(df) +
  aes(x=Ch_LnPc_2011_Stdz)+
  geom_density(kernel="gaussian")+
  stat_function(fun = dnorm,colour = "red")


#Save file
write.csv(df, file="Fichiers/Hungary/Hu_Pop_Provinces_2011.csv")

# Test
kurtosis(df$Bg_LnPc_2011_Stdz)
skewness(df$Bg_LnPc_2011_Stdz)

#Ks test dans tableau
df=read.csv("Fichiers/Austria/At_Pop_Regions_2011.csv")
test=read.csv("Fichiers/test/Zipf/Zipf_Eu_2011.csv")
test=read.csv("Fichiers/Test/Zipf/Zipf_Eu_Others.csv")

ks<-ks.test(df$Gr_LnPc_2011_Stdz,pnorm)
ks<-tidy(ks)
ks<- rename.variable(ks,"statistic","D")
ks<- mutate(ks,"method"=NULL,"alternative"=NULL)
ks<-mutate(ks,"Country"="Greece")
ks<-mutate(ks,"N"=316)
ks<-ks[,c(3,1,2,4)]

test<-rbind(test,ks)
write.csv(df, file="Fichiers/Test/ks.csv")



#Manipulation sur test KS
str(df)
df<-test
df<-df %>% mutate("D" = round(df$D, digits = 4))
df<-df %>% mutate("p.value" = round(df$p.value, digits = 4))

df2<-df %>% filter(Country %in% c("Portugal","Spain"))
df<-signif(test$p.value,digits = 4)
new<-order(df$p.value)
df[new,]

#Not normal
filter(df, p.value<0.05)
#Maybe normal
filter(test, p.value>0.05 & N>500)




###################France et outre mer ##################
df1<-read.csv("Fichiers/Spain/Es_Pop_Cities_2011.csv")
df2<-read.csv("Fichiers/Italy/It_Pop_Cities_2011.csv")


df2<-subset(df2, select = -c(CODCOL,COL,PMUN,PCAP))
df2<- rename.variable(df2,"CODCOM","Fr_Codct")
df2<- rename.variable(df2,"COM","Fr_cities")
df2<- rename.variable(df2,"PTOT","Fr_Pc_2017")

df<-rbind(df1,df2)
df<-full_join(df1,df2)

#### Zipf law ###
df=read.csv("Fichiers/United_Kingdom/Uk_Pop_Regions_2011.csv")
df<-df %>% mutate("X" =NULL) 

df<-df[order(-df$Uk_Pr_2011),]
row.names(df)<-NULL
df<-mutate(df, "Uk_Rank"= 1:nrow(df))  #On se sert de l'indexation de la table UNE FOIS TRIE
df$Uk_RankLn<-log(df$Uk_Rank)

write.csv(df, file="Fichiers/United_Kingdom/Uk_Pop_Regions_2011_Rank.csv")


#Regression rank en fonction de la population, on test coeff = -1
model<-lm(Uk_RankLn~Uk_LnPr_2011,data=df)
reg<-tidy(model)

reg<-mutate(reg,"term"=NULL)
reg<-reg[-1,]
reg<-rename.variable(reg,"statistic","t.student")

reg<-mutate(reg,"p.value"= round(reg$p.value, digits = 4))
reg<-mutate(reg,"estimate" = round(reg$estimate, digits = 4))
reg<-mutate(reg,"std.error" = round(reg$std.error, digits = 4))
reg<-mutate(reg,"t.student" = round(reg$t.student, digits = 4))

reg<-mutate(reg,"Country"="United Kingdom")
reg<-mutate(reg,"N"=12)
reg<-reg[,c(5,6,1,2,3,4)]

test<-rbind(test,reg)
write.csv(test, file="Fichiers/Test/Zipf/Zipf_Regions_Eu.csv")



ggplot(df, aes(x=Hr_LnPr_2011, y=Hr_RankLn)) +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
  ggtitle("Zipf's Law test: Croatia_R") +
  xlab("Population size (log)") + ylab("City rank (log)")
