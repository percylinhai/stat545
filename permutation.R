library(ggplot2)
dat<-read.table("~/Study/study resources/STAT545/Project/Transcripts/Split_Transcripts/sample12.txt",h=T)
Trump<-dat%>%filter(Identity==0)
Clinton<-dat%>%filter(Identity==1)
null_dist_trump<-NULL
null_dist_clinton<-NULL
for (i in 1:1000){
	b<-sample(Clinton$Broadness,1)
	d<-sample(Clinton$demonstrative,1)
	i<-sample(Clinton$indef,1)
	n<-sample(Clinton$Negative,1)
	p<-predict(glm_fit,
	data.frame(Broadness=b,demonstrative=d,indef=i,Negative=n))
	null_dist_clinton<-c(p,null_dist_clinton)
}
for (i in 1:1000){
	b<-sample(Trump$Broadness,1)
	d<-sample(Trump$demonstrative,1)
	i<-sample(Trump$indef,1)
	n<-sample(Trump$Negative,1)
	p<-predict(glm_fit,
	data.frame(Broadness=b,demonstrative=d,indef=i,Negative=n))
	null_dist_trump<-c(p,null_dist_trump)
}
null_dist_clinton<-sort(null_dist_clinton)
null_dist_trump<-sort(null_dist_trump)
full_dat<-data.frame(logit=c(null_dist_clinton,null_dist_trump),speaker=c(rep("Clinton",1000),rep("Trump",1000)))
ggplot()+
geom_density(aes(x=logit,fill="Blue",color="Blue"),data=subset(full_dat,speaker=="Clinton"),alpha=0.4)+
geom_density(aes(x=logit,fill="Red",color="Red"),data=subset(full_dat,speaker=="Trump"),alpha=0.4)+
scale_colour_manual(name="Speaker",values=c("Blue","Red"),labels=c("Clinton","Trump"))+
scale_fill_manual(name="Speaker",values=c("Blue","Red"),labels=c("Clinton","Trump"))

hist(null_dist_clinton,col="Blue",xlim=c(-40,40))
hist(null_dist_trump,col="Red",add=T)
box()

plot(density(null_dist_clinton),col="Blue")
lines(density(null_dist_trump),col="Red")