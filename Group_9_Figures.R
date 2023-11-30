#library(tidyverse)
library(ggplot2)
library(dplyr)
lyrics = read.csv("my_dataset.csv", encoding = "latin1")

party = lyrics %>% filter(title == "Party In The U.S.A.")
veng_un = lyrics %>% filter(title == "Vengeance Unleashed")

pop = lyrics %>% filter(genre == "pop")
metal = lyrics %>% filter(genre == "metal")

colours = c("#7f3b08","#b35806","#e08214","#fdb863","#fee0b6","#d8daeb", "#b2abd2", "#8073ac", "#542788", "#2d004b")

###### Top 10 Words/ 10 Word Selection Plots
party = party[order(party$count,decreasing = TRUE),]
party_10 = party[1:10,]
xlabels =party$word
party_10 %>% ggplot(aes(x = reorder(word, -count), y = count, fill = reorder(word, -count))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(labels = xlabels) +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=0, hjust=1, size = 15),
        axis.title = element_text(size = 15), axis.text = element_text(size = 12)) +
  scale_fill_manual(values = colours) +
  labs(title = "\"Party in the U.S.A\" Top 10 Word Count", x = "Word", y = "Frequency of Word",
       caption = "12.5% of words occur once in the song\n52 unique words out of 424 total words") 


veng_un = veng_un[order(veng_un$count),]
veng_un_10 = veng_un[68:77,]
xlabels2 = veng_un_10$word
xlabels2[1] = "vengeance"
xlabels2[2] = "crucify"
xlabels2[3] = "defense"
xlabels2[4] = "unleashed"
xlabels2[6] = "indifference"
#data cleaning has not converted all words to their 
#proper spelling; corrections are for
#visualization purposes

veng_un_10 %>% ggplot(aes(x = reorder(word, count), y = count, fill = reorder(word, count))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_x_discrete(labels = xlabels2) +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1, size = 12),
        axis.title = element_text(size = 12), axis.text = element_text(size = 12)) +
  scale_fill_manual(values = colours) +
  labs(title = "\"Vengeance Unleashed\" 10 Word Sample", x = "Word", 
       y = "Frequency of Word",caption = "37.629% of words occur once\n73 single occurences out of 194 total words") #+


###### Word Histogram of each song
party %>% ggplot(aes(x = count)) +
  geom_histogram( show.legend = FALSE, fill = "#7f3b08", bins = 55,
                  linewidth = 15) +
  scale_x_continuous(breaks = seq(1,34,2)) +
  scale_y_continuous(breaks = seq(0, 52, 4)) +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=0, hjust=1, size = 12),
        axis.title = element_text(size = 12), axis.text = element_text(size = 12)) +
  scale_fill_manual(values = rep(colours, 13)) +
  labs(title = "\"Party in the U.S.A\" Word Distribution", x = "Frequency of a Word", 
       y = "Number of Words Sharing the Same Frequency") +
  coord_flip()

veng_un %>% ggplot(aes(x = count)) +
  geom_histogram( show.legend = FALSE, fill = "#2d004b") +
  scale_x_continuous(breaks = seq(1,12,1)) +
  scale_y_continuous(breaks = seq(0, 75, 5)) +
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=0, hjust=1, size = 12),
        axis.title = element_text(size = 12), axis.text = element_text(size = 12)) +
  scale_fill_manual(values = rep(colours, 13)) +
  labs(title = "\"Vengeance Unleashed\" Word Distribution", x = "Frequency of a Word", 
       y = "Number of Words Sharing the Same Frequency") +
  coord_flip()

######## Total Metal and [Almost all] Pop Word Distribution 
combine_frame = pop %>% bind_rows(metal)
combine_frame = combine_frame[48740:246720,]
#remove about 540 songs from pop to make it more comparable to
#metal
pmhist = combine_frame %>% ggplot(aes(x = count, fill = genre)) +
  geom_histogram(bins = 60 , color = c("#542788"), size = 0.6,
                 show.legend = FALSE) 
pmhist +
  facet_grid(~genre, scales = "free", space = "free") +
  scale_x_continuous(breaks = seq(0,200,10)) +
  scale_y_continuous(breaks = seq(0,60000,5000)) +
  theme(text = element_text(size=15),
        axis.title = element_text(size = 12), axis.text = element_text(size = 8)) +
  labs(title = "Word Distribution Among Metal and Pop Songs",
       x = "Frequency of a Word", y = "Number of Words Sharing the Same Frequency")


