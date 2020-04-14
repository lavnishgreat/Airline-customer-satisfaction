## Word Cloud
library(tm)
free_text<-df$freeText[10001:10282]
words.vec<-VectorSource(free_text)
words.corpus<-Corpus(words.vec)
words.corpus<-tm_map(words.corpus,
                     content_transformer(tolower))
words.corpus<-tm_map(words.corpus,
                     removePunctuation)
words.corpus<-tm_map(words.corpus,
                     removeNumbers)
words.corpus<-tm_map(words.corpus,removeWords, stopwords("English"))

free_text<- data.frame(text = sapply(words.corpus, as.character), stringsAsFactors = FALSE)

#word cloud for free text
textdis<-VCorpus(VectorSource(free_text))
inspect(textdis)    
head(textdis)


tdm<-TermDocumentMatrix(textdis)
m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts<-sort(wordCounts,decreasing = TRUE)
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
wordcloud(cloudFrame$word,cloudFrame$freq,res=300,colors=brewer.pal(8, "Dark2"))


#Rounding off the floating values
df$`Departure Delay in Minutes`<-round(df$`Departure Delay in Minutes`)
df$`Arrival Delay in Minutes`<-round(df$`Arrival Delay in Minutes`)
df$`Flight time in minutes`<-round(df$`Flight time in minutes`)