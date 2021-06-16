getwd()
library(readr)
heart <- read_csv("heart.csv",col_types = cols(sex=col_character(),
                          cp=col_character(),fbs=col_character(),
                          exng=col_character(),slp=col_character(),
                          thall=col_character(),output=col_character(),
                          restecg=col_character()))
View(heart)
heart=data.frame(heart)
#summary of Data
summary(heart)
library(dplyr)
heart_1=heart%>%rename("Chest_pain"="cp","cholestrol"="chol",
                     "exercise_induced_angina"="exng","number_of_vessel"="caa",
                     "blood_pressure"="trtbps","fast_blood_sugar"="fbs",
                     "max_heart_rate_achieved"="thalachh","stress_test"="thall")

summary(heart_1)
heart1=heart_1%>%mutate(sex=recode(sex,"1"="Male","0"="Female"))
heart1=heart1%>%mutate(exercise_induced_angina=recode(exercise_induced_angina,"1"="yes",
                                                       "0"="No"))
heart1=heart1%>%mutate(Chest_pain=recode(Chest_pain,"0"="t-angina",
                                    "1"="a-angina","2"="non-anginal",
                                    "3"="asymptomatic"))
heart1=heart1%>%mutate(slp=recode(slp,"0"="upsloping","1"="flat","2"="downsloping"))
View(heart1)

for (i in seq(1,ncol(heart1),1)){           
  print(unique(heart1[i]))    # checking uniques
}

table(heart1$sex) # number of people according to sex
table(heart1$Chest_pain) #number of people according to chest pain
table(heart1$slp) # number of people for slope of the peak exercise
table(heart1$exercise_induced_angina) # number of people for exercise reduced angina

any(is.na(heart1)) # checking missing values

# data visualization
library(ggplot2)

ggplot(data=heart1,aes(x=sex,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=age,fill=sex))+geom_histogram(col="black")
ggplot(data=heart1,aes(x=Chest_pain,fill=sex))+geom_bar()+
  theme(axis.text=element_text(size = 8))
ggplot(data=heart1,aes(x=slp,fill=sex))+geom_bar()
ggplot(data=heart1,aes(x=exercise_induced_angina,fill=sex))+geom_bar()

#Cholesterol relationships with other variables
x1=ggplot(data=heart1,aes(x=cholestrol,y=blood_pressure,col=sex))+
  geom_point()+geom_smooth(method="lm",se=FALSE)

x2=ggplot(data=heart1,aes(x=cholestrol,y=max_heart_rate_achieved,col=sex))+
  geom_point()+geom_smooth(method="lm",se=FALSE)

x3=ggplot(data=heart1,aes(x=cholestrol,y=age,col=sex))+geom_point()+
  geom_smooth(method="lm",se=FALSE)

install.packages("ggpubr")
library(ggpubr)
ggarrange(x1,x2,x3,ncol=1,nrow=3)

#Heart rate achieve relationship with other variables
y1=ggplot(data=heart1,aes(x=max_heart_rate_achieved,y=age,col=sex))+
  geom_point()+geom_smooth(method="lm",se=FALSE)

y2=ggplot(data=heart1,aes(x=max_heart_rate_achieved,y=blood_pressure,col=sex))+
  geom_point()+geom_smooth(method="lm",se=FALSE)

ggarrange(y1,y2,ncol=1,nrow=2)
 
#chest pain in terms of Sex

ggplot(data=heart1,aes(x=Chest_pain,y=age,fill=sex))+
  geom_boxplot()+facet_wrap(~sex)+xlab("chest pain")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 8))

ggplot(data=heart1,aes(x=Chest_pain,y=blood_pressure,fill=sex))+
  geom_boxplot()+facet_wrap(~sex)+xlab("chest pain")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 8))


ggplot(data=heart1,aes(x=Chest_pain,y=cholestrol,fill=sex))+
  geom_boxplot()+facet_wrap(~sex)+xlab("chest pain")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 8))

ggplot(data=heart1,aes(x=Chest_pain,y=max_heart_rate_achieved,fill=sex))+
  geom_boxplot()+facet_wrap(~sex)+xlab("chest pain")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 8))

# stress level in terms of fast blood sugar and sex

x1=ggplot(data=heart1,aes(x=stress_test,y=age,fill=fast_blood_sugar))+
geom_boxplot()+facet_wrap(~sex)+xlab("stress test")+
theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x2=ggplot(data=heart1,aes(x=stress_test,y=blood_pressure,fill=fast_blood_sugar))+
  geom_boxplot()+facet_wrap(~sex)+xlab("stress test")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x3=ggplot(data=heart1,aes(x=stress_test,y=cholestrol,fill=fast_blood_sugar))+
  geom_boxplot()+facet_wrap(~sex)+xlab("stress test")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x4=ggplot(data=heart1,aes(x=stress_test,y=max_heart_rate_achieved,fill=fast_blood_sugar))+
  geom_boxplot()+facet_wrap(~sex)+xlab("stress test")+
  theme(axis.title=element_text(size = 10),text = element_text(size =10))

ggarrange(x1,x2,x3,x4,ncol=1,nrow=4)

# chances of heart attack in terms pf chest pain and Sex
x1=ggplot(data=heart1,aes(x=output,y=age,fill=Chest_pain))+
geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x2=ggplot(data=heart1,aes(x=output,y=cholestrol,fill=Chest_pain))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x3=ggplot(data=heart1,aes(x=output,y=blood_pressure,fill=Chest_pain))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x4=ggplot(data=heart1,aes(x=output,y=max_heart_rate_achieved,fill=Chest_pain))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

ggarrange(x1,x2,x3,x4,ncol=1,nrow=4)

# chances of heart attack with in terms of resting electrocardiograph results and Sex
x1=ggplot(data=heart1,aes(x=output,y=max_heart_rate_achieved,fill=restecg))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x2=ggplot(data=heart1,aes(x=output,y=blood_pressure,fill=restecg))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x3=ggplot(data=heart1,aes(x=output,y=cholestrol,fill=restecg))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))

x4=ggplot(data=heart1,aes(x=output,y=age,fill=restecg))+
  geom_boxplot()+facet_wrap(~sex)+xlab("Heart attack chances")+
  theme(axis.title=element_text(size = 10),text = element_text(size = 10))


ggarrange(x1,x2,x3,x4,ncol=1,nrow=4)
