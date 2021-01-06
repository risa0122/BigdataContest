####패키지,라이브러리(없는 패키지 알아서 설치하기)####
library(tidytext)
library(plyr)
library(dplyr)
library(textdata)
library(ggplot2)
library(tidyr) #separate()제공
#install.packages("igraph")
library(igraph)
#install.packages("ggraph")
library(ggraph)
library(wordcloud)
library(wordcloud2)

#경로 각자 바꾸기#
setwd('/Users/jongeun/Desktop/test')
dff <- read.csv('merged.csv')

dff <- dff[,c(1,4)]
na.omit(dff)

text_dff <- data.frame(rid=nrow(dff),text=dff$comment)
#text_dff <- data.frame(rid=1:100,text=dff[1:100,]$comment)
tidy_bigramm <- text_dff %>% unnest_tokens(bigram,text,token = "ngrams", n = 2)


# remove stop word
bigrams_separatedd <- tidy_bigramm %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filteredd <- bigrams_separatedd %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)


# 바이그램 빈도 테이블 
bigram_countss <- bigrams_filteredd %>% 
  count(word1, word2, sort = TRUE)

# 바이그램 결합 
bigrams_unitedd <- bigrams_filteredd %>%
  unite(bigram, word1, word2, sep = " ")



#########

dic1 <- read.table(file='/Users/jongeun/Desktop/test/dic/english-adjectives.txt',
                   header = F,
                   sep='')

dic2<- read.table(file='/Users/jongeun/Desktop/test/dic/english-noun.txt',
                  header = F,
                  sep='')

dic1 <- data.frame('x1' = dic1)
dic2 <- data.frame('x2' = dic2)

dic1 <- dic1 %>% rename('word1'='V1')
dic2 <- dic2 %>% rename('word2'='V1')

bigrams_filteredd <- data.frame('word1'= bigrams_filteredd[,2], 'word2'=bigrams_filteredd[,3])

#####    추출    #####


#2
f_words <- bigrams_filteredd %>%
  inner_join(dic1,by = c(word1 = "word1")) %>%
  inner_join(dic2,by = c(word2 = "word2"))


#join <- inner_join(mutate(data, x=factor(x, levels=combined)),
#mutate(dic, x=factor(x, levels=combined)))

f_words$word2 <- gsub("book","story",f_words$word2)

d <- f_words$word1
dd <- f_words$word2

for (i in 1:length(f_words)) {
  adj_noun <- paste(d,dd)
}

adj_noun <- data.frame(adj_noun)

adj_noun$adj_noun <- gsub("amazing stories","amazing story",adj_noun$adj_noun)
adj_noun$adj_noun <- gsub("free copy","",adj_noun$adj_noun)
adj_noun$adj_noun <- gsub("advanced copy","",adj_noun$adj_noun)
adj_noun$adj_noun <- gsub("bad guys","bad guy",adj_noun$adj_noun)
adj_noun$adj_noun <- gsub("bad bay","bad boy",adj_noun$adj_noun)
adj_noun$adj_noun <- gsub("female characters","female character",adj_noun$adj_noun)

na.omit(adj_noun)

####빈도####
cont <- count(adj_noun,adj_noun)
cont <- cont %>%  rename("freq"="n")
cont <- cont[-1,]

cont_order <- cont[order(cont$freq,decreasing = TRUE),]
cont_order <- cont_order[-3,]
cont_order <- cont_order[-2,]
cont_order <- cont_order[-4,]
cont_order <- cont_order[-9,]
#cont <- arrange(cont,desc(freq))
cut_cont <- cont_order[1:9,] #빈도수 상위 9개 행 추출

####히스토그램####
g <- ggplot(cut_cont, aes(x=reorder(adj_noun,-freq),y=freq))
g+ theme_classic()+geom_bar(stat="identity", fill="orange", colour="white")+ggtitle("Adjective-Noun Frequency")+theme(axis.text.x=element_text(angle=45, hjust=1))



#### igraph 패키지 설치 필요####
#방향성을 가진 연결망
#install.packages("igraph")
#library(igraph)

#형용사+명사 조합 카운트
#f_words$word2 <- gsub("book","story",f_words$word2)

f_words_count <- f_words %>%
  count(word1,word2,sort = TRUE)

f_words_count <- f_words_count[-1,]
f_words_count <- f_words_count[-3,]
f_words_count <- f_words_count[-2,]
f_words_count <- f_words_count[-4,]
f_words_count <- f_words_count[-9,]
f_words_count <- f_words_count[-38,]
#bigram_countss

#filter for only relatively common combinations
bigram_graph <- f_words_count %>% 
  filter(n>50) %>%   #빈도수 보고 숫자 결정
  graph_from_data_frame()
bigram_graph

#install.packages("ggraph")
#library(ggraph)
set.seed(2016)

b <- grid::arrow(type="closed", length=unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha=n), show.legend=FALSE,
                 arrow = b, end_cap = circle(.04, 'inches')) + 
  geom_node_point(color="orange",size=4) +
  geom_node_text(aes(label = name), vjust=1, hjust=1) +
  theme_void()

####워드크라우드####
#library(wordcloud)
#library(wordcloud2)

cont_order %>% wordcloud2(size = 1.2, color = "random-light", fontFamily = 'Tahoma', minRotation = -pi/2, maxRotation = -pi/2)

wordcloud(words = cont_order$adj_noun, freq = cont_order$freq, min.freq=5,random.order=FALSE,colors=brewer.pal(8,"Dark2"))
