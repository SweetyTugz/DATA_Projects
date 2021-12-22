heart <- read.csv("C:/Users/sweet/OneDrive/Desktop/Heart_Disease_prediction/heart.csv")
View(heart)
library(tidyverse)
head(heart)
tail(heart)
glimpse(heart)
colnames(heart)
data <-heart
data <-heart %>%
  mutate(sex=if_else(sex==1,"FEMALE","MALE"),
         fbs=if_else(fbs ==1,">120","<-120"),
         exang=if_else(exang ==1, "YES", "NO"),
         cp=if_else(cp==1,"ATYPICAL ANGINA",if_else(cp==2,"NON-ANGINAL PAIN","ASYMPTOMATIC")),
         restecg=if_else(restecg ==0,"NORMAL", if_else(restecg==1,"ABGNORMALITY","PROBABLE OR             DEFINITE")),
         slope=as.factor(slope),
         ca=as.factor(ca),
         thal=as.factor(thal),
         target=if_else(target==1,"YES","NO")
  ) %>%
  
  ggplot(data,aes(x=data$target, fill=data$target))+geom_bar()+
  xlab("Heart Disease")+ylab("count")+
  ggtitle("Presence and absence of Heart Disease")+
  scale_fill_discrete(name=' Heart Disease',labels=c("Absence",Presence))
ggplot(data,aes(x=data$target, fill=data$target))+geom_bar()+
  xlab("Heart Disease")+ylab("count")+
  ggtitle("Presence and absence of Heart Disease")+
  scale_fill_discrete(name=' Heart Disease',labels=c("Absence","Presence"))
prop.table(table(data$target))
data %>%
  group_by(i..age) %>% count() %>% filter(n>10) %>%
  ggplot()+geom_col(aes(i..age,n), fill='green')+ggtitle("AGE Analysis")+xlab
data %>%
  group_by(i..age) %>% count() %>% filter(n>10) %>%
  ggplot()+geom_col(aes(i..age,n), fill='green')+ggtitle("AGE Analysis")+xlab("AGE")+ylab("Age Count")
data %>%
  group_by(ï..age) %>% count() %>% filter(n>10) %>%
  ggplot()+geom_col(aes(ï..age,n), fill='green')+ggtitle("AGE Analysis")+xlab("AGE")+ylab("Age Count")
data %>%
  ggplot(aes(x=sex,y=trestbps))+
  geom_boxplot(fill='purple')+
  xlab("sex")+ylab("BP")+facet_grid(~cp)
heart %>%
  ggplot(aes(x=sex,y=trestbps))+
  geom_boxplot(fill='purple')+
  xlab("sex")+ylab("BP")+facet_grid(~cp)
data %>%
  ggplot(aes(x=sex,y=trestbps))+
  geom_boxplot(fill='purple')+
  xlab("sex")+ylab("BP")+facet_grid(~cp)
data %>%
  ggplot(aes(x=sex,y=chol))+
  geom_boxplot(fill='orange')+
  xlab("sex")+ylab("Cholestrol")+facet_grid(~cp)
  mutate_if(is.character,as.factor) %>%
  dplyr::select(target,sex,fbs,exang,cp,restecg,slope,ca,thal,everything())