# Example Code
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidytext)

debate<-read.csv("/Users/percylinhai/Study/study resources/STAT545/Project/debate.csv", h=T, stringsAsFactors=F)

#Word count per Line
debate<-debate %>% mutate(word_count = sapply(gregexpr("[[:alpha:]]+", Text), function(x) sum(x>0)))

ggplot(debate %>% filter(Speaker %in% c("Clinton", "Trump")), 
aes(x=Line, y=word_count, fill=Speaker))+
geom_bar(stat="identity")+
scale_x_continuous(breaks=seq(0,nrow(debate),25))+
labs(x="Time (Line in Transcript)",
y="Word Count in Candidate Response",
title="Candidates' Response Lengths over Time")+
scale_fill_manual(values=c("Blue","Red"))

#Word frequency and importance"Raddatz","Cooper"
# Calculate word frequencies
debate_words <- debate %>% 
	filter(Speaker %in% c("Clinton", "Trump")) %>%
	unnest_tokens(word, Text) %>%
	count(Speaker, word, sort=TRUE) %>%
	ungroup()

# Remove some unimportant words
stopwords<-data_frame(word = c("when", "that's", "some", "were"))
debate_words<-anti_join(debate_words, stopwords, by="word")
	
# Calculate tf-idf
debate_words<-debate_words %>%
	bind_tf_idf(word, Speaker, n) %>%
	arrange(desc(tf_idf)) %>%
	mutate(word=factor(word, levels=rev(unique(word))))



# Create plots of the tf-idf for each candidate
# Clinton
clinton_words<-debate_words %>% filter(Speaker == "Clinton")
clinton_words$word<-factor(clinton_words$word,levels=rev(clinton_words$word))
clinton <- ggplot(clinton_words %>%
	top_n(10),
	aes(x=word, y=tf_idf))+
	geom_bar(aes(alpha=tf_idf),
	stat="identity",
	fill = "Blue")+
	theme(text = element_text(size=15))+
	coord_flip()+
	labs(x=NULL, y="tf_idf", title="Clinton")+
	scale_alpha_continuous(range=c(0.6,1), guide=FALSE)
	
# Trump
trump_words<-debate_words %>% filter(Speaker == "Trump")
trump_words$word<-factor(trump_words$word,levels=rev(trump_words$word))
trump <- ggplot(trump_words %>%
	top_n(10),
	aes(x=word, y=tf_idf))+
	geom_bar(aes(alpha=tf_idf),
	stat="identity",
	fill = "Red")+
	theme(text = element_text(size=15))+
	coord_flip()+
	labs(x=NULL, y="tf_idf", title="Trump")+
	scale_alpha_continuous(range=c(0.6,1), guide=FALSE)
	

# Plot
grid.arrange(clinton,trump,ncol=2)

