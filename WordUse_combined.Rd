library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidytext)

debate<-read.csv("/Users/percylinhai/Study/study resources/STAT545/Project/Transcripts/Split_Transcripts/combined.csv", h=T, stringsAsFactors=F)

word_rank<-read.csv("/Users/percylinhai/Study/study resources/STAT545/Project/WordUse_rank_table.csv",h=T)

# English pronouns
first_person_sg <- c("i", "i'll", "i've", "i'm", "me", "myself", "my", "mine")

first_person_pl <- c("we", "we're", "we'll", "we've", "us", "ourselves", "our", "ours")

second_person <- c("you", "you're", "you'll", "you've", "yourself", "yourselves", "your", "yours")

third_person <- c("she", "she'll", "she's", "she'd", "her", "he'll", "he's", "he'd", "he", "him", "it", "it'll", "it's", "they", "they'll", "they're", "they've", "them", "himself", "herself", "itself", "themselves", "his", "her", "its","their", "theirs")

relative <- c("that", "which", "who", "whom", "whose", "whichever", "whoever","whomever", "that'll", "that's", "who'll", "who've")

demonstrative <- c("this", "that", "these", "those")

indefinite <- c("anybody", "anyone", "anything", "each", "either", "everybody", "everyone", "everything", "neither", "nobody", "no one", "nothing", "one", "somebody", "someone", "something", "both", "few", "many", "several", "all", "any", "most", "none", "some")

interrogative <- c("what", "who", "which", "whom", "whose")

debate_word <- debate %>% 
	unnest_tokens(word, Text) %>%
	count(Speaker, word, sort=TRUE) %>%
	ungroup()

# Remove some unimportant words
stopwords<-data_frame(word = c(first_person_sg,first_person_pl,second_person,third_person))
debate_word<-anti_join(debate_word, stopwords, by="word")

debate_important_words<-debate_word %>%
	bind_tf_idf(word, Speaker, n) %>%
	arrange(desc(tf_idf)) %>%
	mutate(word=factor(word, levels=rev(unique(word))))

debate_important_words$rank <- match(debate_important_words$word,word_rank$Word)

clinton_words<-debate_important_words %>%
	filter(Speaker == "Clinton_1")

#Broadness
#Test correlations between total number of words and word broadness
dat<-read.table("~/Study/study resources/STAT545/Project/Transcripts/Split_Transcripts/word_broadness.txt",header=T)
lfit<-lm(Word_broadness~Word_count,data=dat)
plot(dat,main="Word_broadness and Word_count")
abline(lfit,col='Red')
eq<-paste0("Broadness=-9.269e-05*Word_count+0.541 (p-value<1E-5)")
mtext(eq)

#Broadness
speakers<-unique(debate$Speaker)
speakers<-speakers[speakers != ""]
for (speaker in speakers){
	a<-debate %>% unnest_tokens(word, Text) %>%group_by(word, Speaker)%>%filter(Speaker==speaker)
	a<-anti_join(a,stopwords,by="word")
	a_r<-a[1:1000,] %>% summarise(count=n()) %>% group_by(Speaker)%>%ungroup
	broad<-length(a_r$word)/sum(a_r$count)
	result<-c(speaker,length(a_r$word),sum(a_r$count),broad)
	print(result)
}

# Bring them all together in a list
pronouns <- list("first_person_sg" = first_person_sg,
				"first_person_pl" = first_person_pl, 
				"second_person" = second_person, 
				"third_person" = third_person, 
				"relative" = relative, 
				"demonstrative" = demonstrative, 
				"indefinite" = indefinite, 
				"interrogative" = interrogative)
				
debate_words <- debate %>%
    unnest_tokens(word, Text) %>% # tidytext function
    group_by(word, Speaker) %>%
    summarise(count = n()) %>%
    group_by(Speaker) %>%
   	mutate(frequency = count/sum(count)) %>%
   	arrange(desc(frequency)) %>%
    ungroup()
    
# Create a pronoun variable using the list of pronouns
debate_words$pronoun <- ifelse(debate_words$word %in% pronouns$first_person_sg, "first_person_sg",
						ifelse(debate_words$word %in% pronouns$first_person_pl, "first_person_pl",
						ifelse(debate_words$word %in% pronouns$second_person, "second_person",
						ifelse(debate_words$word %in% pronouns$third_person, "third_person",
						ifelse(debate_words$word %in% pronouns$relative, "relative",
						ifelse(debate_words$word %in% pronouns$demonstrative, "demonstrative",
						ifelse(debate_words$word %in% pronouns$indefinite, "indefinite",
						ifelse(debate_words$word %in% pronouns$interrogative, "interrogative", NA))))))))

speakers<-unique(debate$Speaker)
speakers<-speakers[speakers != ""]
for (speaker in speakers){						
	speaker_words<-debate_words %>% filter(Speaker==speaker)
	p_words<-speaker_words %>% filter(!is.na(pronoun))
	total_p<-sum(p_words$count)
	first_sg<-speaker_words %>% filter(pronoun == "first_person_sg")
	first_pl<-speaker_words %>% filter(pronoun == "first_person_pl")
	second_p<-speaker_words %>% filter(pronoun == "second_person")
	third_person<-speaker_words %>% filter(pronoun == "third_person")
	relative<-speaker_words %>% filter(pronoun == "relative")
	demon<-speaker_words %>% filter(pronoun == "demonstrative")
	indef<-speaker_words %>% filter(pronoun == "indefinite")
	inte<-speaker_words %>% filter(pronoun == "interrogative")
	output<-c(speaker,sum(first_sg$count)/total_p,sum(first_pl$count)/total_p,sum(second_p$count)/total_p,sum(third_person$count/total_p),sum(relative$count)/total_p,sum(demon$count)/total_p,sum(indef$count)/total_p,sum(inte$count)/total_p)
	print(output)
}

for (speaker in speakers){
	a<-debate_word%>%filter(Speaker==speaker)
	#Trump tags
	Tags<-c("tremendous","politicians")
	Flag<-FALSE
	for (tag in Tags){
		flag<-tag %in% a$word
		if(flag){
			Flag<-TRUE
			break
		}
	}
	print(speaker)
	print(Flag)
}

dat<-read.table("~/Study/study resources/STAT545/Project/Transcripts/Split_Transcripts/sample12.txt",h=T)
glm_fit<-glm(Identity~Rank+Broadness+first_sg+first_pl+second_p+third_p+relative+demonstrative+indef+interro+Negative+Positive+surprise+anger+anticipation+disgust+fear+joy+negative+positive+sadness+trust,data=dat,family=binomial())
glm_fit<-glm(Identity~
Broadness
+demonstrative
+indef
+Negative
#+surprise
,data=dat,family=binomial())



glm_fit<-glm(Identity~Broadness
+demonstrative
+indef
+Negative
,data=dat,family=binomial())
