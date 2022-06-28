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

Cod1<-function(A){
  for (i in 1:length(A)) {
    if(A[i]=="Male"){A[i]=1} else {A[i]=2};
  }
  A=as.numeric(A)
}
Genre=Cod1(V2)
Genre
pie(table(Genre))

table(V2)

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

