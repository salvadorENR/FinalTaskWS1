#Loading packages
library(datos)
library(ggplot2)
library(readr)
library(scales)
library(stringr)
library(utf8)
library(devtools)
library(ggpubr)
#Loading data base
DBT=read_csv2("DB_WS1.csv")
attach(DBT)
DBT$V10[1]="Very true"

d1=data.frame(table(V2))
d2=data.frame(x=d1$V2,y=d1$Freq)
d2$y<-round(d2$y/length(V2)*100,digits = 1)
d2$x<-enc2utf8(c("Female","Male"))
d2$x=factor(d2$x,levels=c("Female","Male"))

ggplot(d2, aes(x="",y=y,fill=x)) + geom_col(color = "black")+
  #ggtitle("Gender")+ 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_text(aes(label = percent(y/100)),position = position_stack(vjust = 0.5),size=10)+
  coord_polar(theta = "y") +theme(legend.position = "right")+
  scale_fill_manual(values = c("#3F762B", "#ffffff"))+
  guides(fill = guide_legend(title = "Gender"))+
  theme(axis.text = element_blank(),axis.ticks = element_blank(),
        axis.title = element_blank(),panel.grid = element_blank(),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff"),
        legend.background = element_rect(fill = "#ffffff"))

#Valores (1) Very true, (2) Not very true, (3) Sort of true, (4) Not at all true

Cod<-function(A){
  ch<-numeric();
  for (i in 1:length(A)) {
    ch[i]=switch(A[i], "Very true" = 1, "Not very true"=2,"Sort of true"=3, "Not at all true"=4)
  }
  ch;
}
NV3=Cod(V3)

d1=data.frame(table(V3))
colnames(d1) <- c('cat','freq')
d2=data.frame(x=d1$cat,y=d1$freq)
d2$x=factor(d2$x,levels=c("Very true","Not very true","Sort of true","Not at all true"))


ggplot(d2, aes(x,y)) +
  geom_bar(stat = "Identity",color="black", fill="#ffffff")+ 
  geom_text(aes(label = y), vjust = -0.3, color = "black")+
  labs(y="Number of people", x = "Answers") +
  #ggtitle("Participación de los profesores por departamento\n ")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5)) 




