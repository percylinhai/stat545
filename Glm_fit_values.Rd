library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidytext)
dat<-read.table("~/Study/study resources/STAT545/Project/Transcripts/Split_Transcripts/sample12.txt",h=T)

glm_fit<-glm(Identity~
Broadness
+demonstrative
+indef
+Negative
#+surprise
,data=dat,family=binomial())

Trump<-dat%>%filter(Identity==0)
Clinton<-dat%>%filter(Identity==1)
trump_dist<-NULL
clinton_dist<-NULL

for (i in 1:24){
	trump<-Trump[i,]
	p<-predict(glm_fit,data.frame
	(Broadness=trump$Broadness,
	demonstrative=trump$demonstrative,
	indef=trump$indef,
	Negative=trump$Negative))
	trump_dist<-c(p,trump_dist)
}

for (i in 1:22){
	clinton<-Clinton[i,]
	p<-predict(glm_fit,data.frame(
	Broadness=clinton$Broadness,
	demonstrative=clinton$demonstrative,
	indef=clinton$indef,
	Negative=clinton$Negative))
	clinton_dist<-c(p,clinton_dist)
}

plot(density(clinton_dist),col="Blue",xlim=c(-40,40),main="Density of fitted values in training data")
lines(density(trump_dist),col="Red")
legend(22,0.08,c("Clinton","Trump"),lty=c(1,1),col=c("blue","red"))

full_dat<-data.frame(logit=c(clinton_dist,trump_dist),speaker=c(rep("Clinton",22),rep("Trump",24)))
ggplot()+
xlim(c(-40,40))+
geom_density(aes(x=logit,fill="Blue",color="Blue"),data=subset(full_dat,speaker=="Clinton"),alpha=0.4)+
geom_density(aes(x=logit,fill="Red",color="Red"),data=subset(full_dat,speaker=="Trump"),alpha=0.4)+
scale_colour_manual(name="Speaker",values=c("Blue","Red"),labels=c("Clinton","Trump"))+
scale_fill_manual(name="Speaker",values=c("Blue","Red"),labels=c("Clinton","Trump"))

#Permute speaker tags
total_logit<-cbind(clinton_dist,trump_dist)
null_dist_sep<-NULL
for (i in 1:1000000){
	p_logit<-sample(total_logit)
	clinton_logit<-p_logit[1:22]
	trump_logit<-p_logit[23:46]
	clinton_mean<-mean(clinton_logit)
	trump_mean<-mean(trump_logit)
	null_dist_sep<-c(clinton_mean-trump_mean,null_dist_sep)
}
null_dist_sep<-sort(null_dist_sep)
(mean(clinton_dist)-mean(trump_dist))