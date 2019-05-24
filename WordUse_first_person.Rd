#First-person pronoun
library(dplyr)
library(ggplot2)
library(scales)
library(tidytext)

debate<-read.csv("/Users/percylinhai/Study/study resources/STAT545/Project/debate.csv", h=T, stringsAsFactors=F)

# English pronouns
first_person_sg <- c("i", "i'll", "i've", "i'm", "me", "myself", "my", "mine")

first_person_pl <- c("we", "we're", "we'll", "we've", "us", "ourselves", "our", "ours")

second_person <- c("you", "you're", "you'll", "you've", "yourself", "yourselves", "your", "yours")

third_person <- c("she", "she'll", "she's", "her", "he'll", "he's", "he", "him", "it", "it'll", "it's", "they", "they'll", "they're", "they've", "them", "himself", "herself", "itself", "themselves", "his", "her", "its","their", "theirs")

relative <- c("that", "which", "who", "whom", "whose", "whichever", "whoever","whomever", "that'll", "who'll", "who've")

demonstrative <- c("this", "that", "these", "those")

indefinite <- c("anybody", "anyone", "anything", "each", "either", "everybody", "everyone", "everything", "neither", "nobody", "no one", "nothing", "one", "somebody", "someone", "something", "both", "few", "many", "several", "all", "any", "most", "none", "some")

interrogative <- c("what", "who", "which", "whom", "whose")

# Bring them all together in a list
pronouns <- list("first_person_sg" = first_person_sg,
				"first_person_pl" = first_person_pl, 
				"second_person" = second_person, 
				"third_person" = third_person, 
				"relative" = relative, 
				"demonstrative" = demonstrative, 
				"indefinite" = indefinite, 
				"interrogative" = interrogative)
				
# Calculate word count & frequency
debate_words <- debate %>%
	filter(Speaker %in% c("Clinton", "Trump")) %>%
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
				
				
ggplot(debate_words %>% filter(pronoun == "first_person_sg"), 
	aes(x = reorder(word, -frequency), y = frequency, fill = Speaker))+ 
	geom_bar(stat="identity", position="dodge", colour = "#333333")+
	scale_y_continuous(breaks = seq(0, .03, 0.005),labels = percent)+
	labs(x = "First Person Singular Pronoun", 
		 y = "Percent of Candidates' Words", 
		 title = "Who's the more 'self-centered' candidate?")+
	scale_fill_manual(values = c("#4169E1", "#E91D0E"))+
	theme(plot.title = element_text(margin = margin(b = 10, unit = 'pt'),
                                   face = 'bold',
                                   colour = '#333333',
                                   size = rel(1.25),
                                   hjust = 0,
                                   vjust = 1))
                          
trump_first<-debate_words %>% filter(pronoun == "first_person_sg") %>% filter(Speaker == "Trump")
sum(trump_first$frequency)
clinton_first<-debate_words %>% filter(pronoun == "first_person_sg") %>% filter(Speaker == "Clinton")
sum(clinton_first$frequency)
trump_all_pronoun<-debate_words %>% filter(!is.na(pronoun)) %>% filter(Speaker == "Trump")
sum(trump_first$count)/sum(trump_all_pronoun$count)
clinton_all_pronoun<-debate_words %>% filter(!is.na(pronoun)) %>% filter(Speaker == "Clinton")
sum(clinton_first$count)/sum(clinton_all_pronoun$count)

trump_first_pl<-debate_words %>% filter(pronoun == "first_person_pl") %>% filter(Speaker == "Trump")
sum(trump_first_pl$frequency)
clinton_first_pl<-debate_words %>% filter(pronoun == "first_person_pl") %>% filter(Speaker == "Clinton")
sum(clinton_first_pl$frequency)

sum(trump_first_pl$count)/sum(trump_all_pronoun$count)
sum(clinton_first_pl$count)/sum(clinton_all_pronoun$count)